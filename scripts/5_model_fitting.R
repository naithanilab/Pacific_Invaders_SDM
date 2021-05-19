library(tidyverse)
library(parallel)
library(doParallel)
library(wrswoR) # fast integer sampling

library(raster)
library(sf)
library(mecofun)
library(dismo)
library(mgcv)
library(gbm)
library(randomForest)
library(blockCV)
library(sperrorest)

rm(list = ls())
setwd("~/PacificInvadersSDM/")
source("scripts/utils.R")

load("//import/calc9z/data-zurell/koenig/blacklist_final.RData")
load("data/world_mask.RData")
load("data/summary_table.RData")
chelsa_bioclim = raster::stack(str_sort(list.files("//import/calc9z/data-zurell/koenig/chelsa_bioclim/", pattern = ".tif", full.names = T), numeric = T))
names(chelsa_bioclim) = str_replace(names(chelsa_bioclim), pattern = "CHELSA_bio10", "bio")

specs_all = summary_table$species
specs_proc = list.files("//import/calc9z/data-zurell/koenig/models_fit/") %>% str_remove(".RData") %>% str_replace("_", " ")
specs = setdiff(specs_all, specs_proc)

cl = makeCluster(5)
registerDoParallel(cl)
cv_method = "sperrorest" # alternative: "blockCV"

foreach(spec = specs, .packages = c("tidyverse", "wrswoR", "raster", "sf", "mecofun", "dismo", "mgcv", "gbm", "randomForest", "blockCV", "sperrorest")) %dopar% {
  # -------------------------------------------------- #
  #                  Pre-Processing                 ####
  # -------------------------------------------------- #
  set.seed(1)
  
  ## Prepare presence data ####
  occ_df = blacklist_final %>% filter(species == spec, status == "native")
  occ_coords = dplyr::select(occ_df, lon, lat) %>% as.matrix()
  occ_extent = extent(as.vector(apply(occ_coords, 2, range))) * 1.5           # enlarged extent of occurrences
  world_mask_crop = crop(world_mask, occ_extent)                              # crop world_mask to enlarged extent, if extent(world_mask) < occ_extent it will just use the world_mask as is
  occ_thinned = thin_coordinates_sample(coords = occ_coords)                  # custom function for fast spatial thinning
  
  ## Create background samples for for IDW fitting ####
  background_indices = sample(which(values(world_mask_crop) == 1), nrow(occ_thinned))
  background_coords = xyFromCell(world_mask_crop, background_indices)
  
  # Create raster proportional to occ_extent with lower resolution (to speed up IDW)
  lon_range = abs(xmin(occ_extent) - xmax(occ_extent))
  lat_range = abs(ymin(occ_extent) - ymax(occ_extent))
  max_range = max(lon_range, lat_range)
  idw_raster = raster(matrix(1, nrow = round(lat_range*100/max_range, 0), round(lon_range*100/max_range, 0)))
  extent(idw_raster) = occ_extent
  
  # Inverse distance weighted absence sampling ####
  idw = geoIDW(occ_coords, background_coords)
  idw_raster = predict(idw_raster, idw)                                   # Predict at coarse resolution
  idw_raster = resample(idw_raster, world_mask_crop, method = 'bilinear') # Resample back to original resolution
  idw_raster = mask(idw_raster, world_mask_crop)                          # and crop to current mask
  values(idw_raster)[values(idw_raster) < 0.05] = 0.05                    # Set minimum probability to 0.05
  values(idw_raster)[is.na(values(idw_raster))] = 0                       # Set NA cells to p=0
  occ_cells = extract(idw_raster, occ_coords, cellnumber = T)[,"cells"]
  values(idw_raster)[occ_cells] = 0                                       # Set p=0 in cells with (unthinned) occurrences
  
  # use ca. 10-times more absences than presences for regression models: Barbet-Massin (2012)
  abs_indices = wrswoR::sample_int_crank(ncell(idw_raster), 10*nrow(occ_thinned), values(idw_raster)) # fast integer sampling
  abs_coords = xyFromCell(world_mask_crop, abs_indices)
  colnames(abs_coords) = c("lon", "lat")
  abs_thinned = thin_coordinates_thin(abs_coords) # 5 km minimum threshold
  
  ## Prepare final dataset ####
  # Merge presence and absence coordinates
  coords_final = bind_rows(bind_cols(species = spec, present = 1, occ_thinned),
                           bind_cols(species = spec, present = 0, abs_thinned)) %>% 
    as.data.frame()
  
  # Extract BioClim variables
  env_vars = extract(chelsa_bioclim, y = coords_final[,c("lon","lat")]) %>% 
    as.data.frame()

  # Pre-processed data
  data_prep = bind_cols(coords_final, env_vars) %>% drop_na()
  pred_selection =  mecofun::select07_cv(X = dplyr::select(data_prep, bio_01:bio_19), 
                                         y = data_prep$present)

  # Final datasets for regression and machine learning algorithms  
  data_final_regr = dplyr::select(data_prep, species, present, lon, lat, pred_selection$pred_sel) %>%     # data for regression, with P/A = 1:10
    drop_na()
  data_final_ml = bind_rows(filter(data_final_regr, present == 1), sample_frac(filter(data_final_regr, present == 0), 0.1)) %>%   # data for ML algos, with P/A = 1:1    
    drop_na()
                            
  # -------------------------------------------------- #
  #                 Model fitting                   ####
  # -------------------------------------------------- #
  pa_fractions = as.vector(table(data_final_regr$present))
  
  # GLM with linear and quadratic term:
  glm_fit = glm(formula = as.formula(paste('present ~', paste(pred_selection$pred_sel, paste0('+ I(', pred_selection$pred_sel ,'^2)'), collapse=' + '))), 
                family='binomial',
                weights = ifelse(data_final_regr$present == 1, 1, pa_fractions[2] / pa_fractions[1]), # equal total weight for P and A
                data = data_final_regr)
  
  # GAM
  gam_fit = gam(formula = as.formula(paste(paste('present ~', paste(paste0('s(',pred_selection$pred_sel,',k=4)'),collapse=' + ')))), 
                family='binomial', 
                weights = ifelse(data_final_regr$present == 1, 1, pa_fractions[2] / pa_fractions[1]), # equal total weight for P and A
                data = data_final_regr)
  
  # Random Forest
  rf_fit = randomForest(x = dplyr::select(data_final_ml, starts_with("bio")),  # TODO?: repeat 10x
                        y = data_final_ml$present, 
                        ntree=1000, 
                        nodesize=20)
  
  # GBM
  LR = 0.01
  for(i in 1:50){                                                              # TODO?: repeat 10x
    print(LR)
    gbm_fit = try(gbm.step(data = data_final_ml, 
                           gbm.x = pred_selection$pred_sel, 
                           gbm.y = "present", 
                           family = 'bernoulli', 
                           tree.complexity = 2, 
                           bag.fraction = 0.75, 
                           learning.rate = LR, 
                           verbose=F, plot.main=F))
    if (class(gbm_fit) == "try-error" | class(gbm_fit) == "NULL"){
      LR = LR/2
    } else
      if(gbm_fit$gbm.call$best.trees<1000){
        LR = LR/2
      } else 
        if(gbm_fit$gbm.call$best.trees>5000){
          LR = LR*2
        } else { 
          break    # optimal learning rate found
        }
  }
  
  # -------------------------------------------------- #
  #              Model assessment                   ####
  # -------------------------------------------------- #
  # create spatially blocked CV tiles proportional to occ_extent
  if(cv_method == "blockCV"){
    data_final_regr_sf = sf::st_as_sf(data_final_regr, coords = c("lon", "lat"), crs = crs(world_mask_crop))
    cv_tiles_regr = blockCV::spatialBlock(data_final_regr_sf, species = "species",
                                          cols = round(10 * lon_range / max_range, 0),     # maximum 10 columns
                                          rows = round(10 * lat_range / max_range, 0),     # or rows
                                          k = 5, showBlocks = F, progress = F)
    
    data_final_ml_sf = sf::st_as_sf(data_final_ml, coords = c("lon", "lat"), crs = crs(world_mask_crop))
    cv_tiles_ml = blockCV::spatialBlock(data_final_ml_sf, species = "species",
                                        cols = round(10 * lon_range / max_range, 0),       # maximum 10 columns
                                        rows = round(10 * lat_range / max_range, 0),       # or rows
                                        k = 5, showBlocks = F, progress = F)
    
  } else if(cv_method == "sperrorest"){
    cv_part_regr = sperrorest::partition_tiles(data= data_final_regr, coords = c('lon','lat'), nsplit = c(3,3), reassign = T, min_frac = 0.15)[[1]]
    cv_tiles_regr = rep(0, nrow(data_final_regr))
    for (tile in seq_len(length(cv_part_regr))){
      cv_tiles_regr[cv_part_regr[[tile]]$test] <- tile
    }
    data_final_regr$fold_id = cv_tiles_regr
    
    cv_part_ml = sperrorest::partition_tiles(data= data_final_ml, coords = c('lon','lat'), nsplit = c(3,3), reassign = T, min_frac = 0.15)[[1]]
    cv_tiles_ml = rep(0, nrow(data_final_ml))
    for (tile in seq_len(length(cv_part_ml))){
      cv_tiles_ml[cv_part_ml[[tile]]$test] <- tile
    }
    data_final_ml$fold_id = cv_tiles_ml
  }

  # cross-validated predictions (5-fold spatial block cv) | Assumes a column "fold_id" in second argument
  pred_glm = make_cv_preds(glm_fit, data_final_regr)
  pred_gam = make_cv_preds(gam_fit, data_final_regr)
  pred_rf  = make_cv_preds(rf_fit,  data_final_ml)
  pred_gbm = make_cv_preds(gbm_fit, data_final_ml)
  
  # Calculate performance metrics
  eval_glm = calc_performance(glm_fit, data_final_regr, pred_glm)
  eval_gam = calc_performance(gam_fit, data_final_regr, pred_gam)
  eval_rf  = calc_performance(rf_fit,  data_final_ml, pred_rf)
  eval_gbm = calc_performance(gbm_fit, data_final_ml, pred_gbm)
  
  # -------------------------------------------------- #
  #           Assemble and save results             ####
  # -------------------------------------------------- #
  results_list = list(data = list(data_regr = data_final_regr, data_ml = data_final_ml),
                      models = list(glm = glm_fit, gam = gam_fit, rf = rf_fit, gbm = gbm_fit),
                      performance = bind_rows(eval_glm, eval_gam, eval_rf, eval_gbm))
  save(results_list, file = paste0("//import/calc9z/data-zurell/koenig/models_fit/", gsub(" ", "_", spec), ".RData"))
  
  gc()
  return(spec)
}