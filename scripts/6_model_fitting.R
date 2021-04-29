# Status check (test for different stati on same continent / TDWG region, Compare occs with Range maps from BIEN/GIFT)
# pseudoabsences (buffer 200 km, inverse distance method, see Damaris Turorial)
# multicollinearity
# modeling (3-4 models, GAM, GML, RF, BRT)
# ensemble ()
# 
# resources: 
#   https://github.com/damariszurell/SSDM-JSDM
#   https://damariszurell.github.io/EEC-SDM/5_pseudoabsence.html
#   https://rspatial.org/raster/sdm/6_sdm_methods.html#random-forest
#   Zurell et al (2020) JBi
#   Barbet-Massin et al. (2012) EcoMod

library(tidyverse)
library(tidymodels)
library(ranger)
library(xgboost)
library(stacks)

library(raster)
library(spThin)
library(dismo)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

load("data/occ.RData")
load("data/ranking_table.RData")
load("~/Data/world_mask.RData")
bioclim = stack(list.files("~/Data/CHELSA_bioclim/", pattern = ".tif", full.names = T))

cl = makeCluster(50)
registerDoParallel(cl)
specs = unique(ranking_table$species)

foreach(spec = specs, .packages = c("tidyverse", "tidymodels", "ranger", "xgboost", "stacks", "raster", "spThin", "dismo")) %dopar% {
  # -------------------------------------------------- #
  #                  Pre-Processing                 ####
  # -------------------------------------------------- #
  set.seed(1)
  
  ## Create species DF ####
  occ_df = occ %>% filter(species == spec, status == "native")
  occ_coords = dplyr::select(occ_df, lon, lat)
  occ_extent = as.vector(apply(occ_coords, 2, range))
  world_mask_tmp = crop(world_mask, extent(occ_extent) * 1.5) # enlarge extent by factor 1.5
  
  ## Inverse distance weighted absence sampling ####
  # Create random absences across enlarged extent
  abs_naive = randomPoints(world_mask_tmp, p = occ_coords, n = nrow(occ_coords)) 
  
  # Sample absences using inverse distance weighted interpolation
  idw = geoIDW(as.matrix(occ_coords), as.matrix(abs_naive))
  idw_raster = predict(world_mask_tmp, idw) # Takes a few minutes 
  idw_raster = mask(idw_raster, world_mask_tmp)
  abs_coords =  randomPoints(idw_raster, p = occ_coords, n = nrow(occ_coords), prob=T)
  colnames(abs_coords) = c("lon", "lat")
  
  ## Spatial thinning of presences and absences ####
  occs_thinned = thin(bind_cols(species = spec_tmp, occ_coords), long.col = "lon", lat.col = "lat", thin.par = 5, spec.col = "species", reps = 10, write.files=F, locs.thinned.list.return=T)
  occs_thinned = occs_thinned[[which.max(sapply(occs_thinned, nrow))]] 
  abs_thinned = thin(bind_cols(species = spec_tmp, abs_coords), long.col = "lon", lat.col = "lat", thin.par = 5, spec.col = "species", reps = 10, write.files=F, locs.thinned.list.return=T)
  abs_thinned = abs_thinned[[which.max(sapply(abs_thinned, nrow))]] 
  
  ## Prepare final dataset ####
  # Merge presence and absence coordinates
  coords_final = bind_rows(bind_cols(species = spec_tmp, present = 1, occs_thinned),
                           bind_cols(species = spec_tmp, present = 0, abs_thinned)) %>% 
    as_tibble() %>% 
    rename_all(tolower)
  
  # Extract BioClim variables
  env_vars = extract(bioclim, y = dplyr::select(coords_final, "longitude", "latitude")) %>% 
    as_tibble() %>% 
    rename_all(str_replace, "CHELSA_bio10", "bio") 
  
  # Pre-processed data
  data_prep = bind_cols(coords_final, env_vars) %>% 
    mutate(present = as_factor(present)) %>% 
    drop_na()
  
  data_prep = bind_cols(as_tibble(abs_naive), env_vars) %>% 
    mutate(present = as_factor("0"), species = "Bosios") %>% 
    drop_na()
  # -------------------------------------------------- #
  #                    Modeling                     ####
  # -------------------------------------------------- #
  
  # ---------------------------------------------------#
  ## Set up data splits, transform variables, etc.  ####
  # Split data 
  data_split = data_prep %>% initial_split(prop = 0.8) 
  data_train = training(data_split)
  data_test  = testing(data_split)
  cv_folds = spatial_clustering_cv(data_train, coords = c("longitude", "latitude"), v = 5)
  
  # Define general recipe for data transformation
  sdm_recipe = recipe(present ~ ., data = data_train) %>% 
    update_role(species, longitude, latitude, new_role = "ID vars") %>%     # Don't use these  as predictors
    step_normalize(all_predictors()) %>%                                    # Center + rescale predictors
    step_corr(all_predictors(), threshold = 0.7) %>%                        # Remove predictors with r > 0.7
    step_poly(all_predictors(), degree = 2)                                 # Add polynomial terms
  
  # Define workflow using above recipe
  sdm_workflow = workflow() %>%
    add_recipe(sdm_recipe)
  
  # ---------------------------------------------------#
  ## Fit models ####
  doParallel::registerDoParallel()
  
  ### GLM ####
  gc()
  glm_spec = logistic_reg(mode = "classification") %>% set_engine("glm")
  glm_workflow = sdm_workflow %>% add_model(glm_spec) 
  
  glm_fit = fit_resamples(glm_workflow,                              # No tuning needed -> cross validation only
                          cv_folds, 
                          metrics = metric_set(roc_auc),
                          control = control_grid(save_pred = T, save_workflow = T))
  
  ### Random Forest ####
  gc()
  rf_spec = rand_forest(mode = "classification", 
                        trees = tune(),                              # Number of trees
                        mtry = tune(),                               # Number of sampled predictors per step
                        min_n = tune()) %>%                          # Minimum number of data points in a node that is required for it to be split further
    set_engine("ranger")
  
  rf_workflow = sdm_workflow %>% add_model(rf_spec)
  
  rf_fit = tune_grid(rf_workflow, 
                     cv_folds, 
                     grid = grid_latin_hypercube(                    # Sample parameter space semi-randomly
                       trees(),
                       finalize(mtry(), data_train),                 # Range of values depends on the dataset, thus use finalize()
                       min_n(),                                      
                       size = 5                                     
                     ),
                     metrics = metric_set(roc_auc),                  # Calculate AUC only
                     control = control_grid(save_pred = T, save_workflow = T))  # Save predictions for ensemble modeling
  
  ### BRT ####
  gc()
  brt_spec = boost_tree(mode = "classification", 
                        trees = tune(),                              # Number of trees
                        tree_depth = tune(), min_n = tune(), 
                        loss_reduction = tune(),                     # first three: model complexity
                        sample_size = tune(), mtry = tune(),         # randomness
                        learn_rate = tune()) %>%                     # step size
    set_engine("xgboost")
  
  brt_workflow = sdm_workflow %>% add_model(brt_spec)
  
  brt_fit = tune_grid(brt_workflow, cv_folds, 
                      grid = grid_latin_hypercube(                   # Sample parameter space semi-randomly
                        trees(),
                        tree_depth(), min_n(), loss_reduction(),
                        sample_size = sample_prop(),                 # Bag fraction
                        finalize(mtry(), data_train),                # Range of values depends on the dataset, thus use finalize()
                        learn_rate(),
                        size = 5
                      ),
                      metrics = metric_set(roc_auc),                 # Calculate AUC only
                      control = control_grid(save_pred = T, save_workflow = T)) # Save predictions for ensemble modeling
  
  # ---------------------------------------------------#
  ## Stack models, save results ####
  model_stack = stacks() %>%           # Stack model predictions
    add_candidates(glm_fit) %>% 
    add_candidates(rf_fit) %>% 
    add_candidates(brt_fit)
  
  ensemble_pred = model_stack %>%       
    blend_predictions() %>%            # calculate weights for ensemble members
    fit_members()                      # re-train members with non-zero weight
  
  save(ensemble_pred, file = paste0("data/models_fit/", str_replace(spec_tmp, " ", "_"), ".RData"))
}