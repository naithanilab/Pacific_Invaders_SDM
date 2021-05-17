library(tidyverse)
library(parallel)
library(doParallel)

library(tidymodels)
library(ranger)    # Random forest engine
library(xgboost)   # BRT engine
library(earth)     # MARS engine
library(stacks)    # Ensembles

library(raster)
library(spThin)
library(dismo)
library(spatialsample)

rm(list = ls())
setwd("~/PacificInvadersSDM/")

load("//import/calc9z/data-zurell/koenig/occ.RData")
load("data/status_pacific.RData")
load("data/world_mask.RData")
bioclim = raster::stack(str_sort(list.files("//import/calc9z/data-zurell/data/env/global/bioclim/", pattern = "2.5m", full.names = T), numeric = T))

cl = makeCluster(5)
registerDoParallel(cl)
hawaiian_invasives = status_pacific %>% 
  filter(Islandgroup == "Hawaiian" & inva_stat == "T") %>% 
  distinct(Species) %>% 
  pull(Species)

foreach(spec = specs, .packages = c("tidyverse", "tidymodels", "ranger", "xgboost", "stacks", "raster", "spThin", "dismo", "spatialsample")) %dopar% {
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
  
  # Insert cutoff at 5% --> values(idw_r)[values(idw_r) < 0.05] <- 0.05
  # Use buffer distance instead of IDW (proportional to range size)
  
  abs_coords =  randomPoints(idw_raster, p = occ_coords, n = nrow(occ_coords), prob=T)
  colnames(abs_coords) = c("lon", "lat")
  
  ## Spatial thinning of presences and absences ####
  occs_thinned = thin(bind_cols(species = spec, occ_coords), long.col = "lon", lat.col = "lat", thin.par = 5, spec.col = "species", reps = 10, write.files=F, locs.thinned.list.return=T)
  occs_thinned = occs_thinned[[which.max(sapply(occs_thinned, nrow))]] 
  abs_thinned = thin(bind_cols(species = spec, abs_coords), long.col = "lon", lat.col = "lat", thin.par = 5, spec.col = "species", reps = 10, write.files=F, locs.thinned.list.return=T)
  abs_thinned = abs_thinned[[which.max(sapply(abs_thinned, nrow))]] 
  
  ## Prepare final dataset ####
  # Merge presence and absence coordinates
  coords_final = bind_rows(bind_cols(species = spec, present = 1, occs_thinned),
                           bind_cols(species = spec, present = 0, abs_thinned)) %>% 
    as_tibble() %>% 
    rename_all(tolower)
  
  # Extract BioClim variables
  env_vars = extract(bioclim, y = dplyr::select(coords_final, "longitude", "latitude")) %>% 
    as_tibble() %>% 
    rename_all(str_replace, "wc2.1_2.5m_", "") 
  
  # Pre-processed data
  data_prep = bind_cols(coords_final, env_vars) %>% 
    mutate(present = as_factor(present)) %>% 
    drop_na()
  
  rm(occ, world_mask)
  # -------------------------------------------------- #
  #                    Modeling                     ####
  # -------------------------------------------------- #
  ## Set up data splits, transform variables, etc.  ####
  # Split data 
  data_split = data_prep %>% initial_split(prop = 0.8) 
  data_train = training(data_split)
  data_test  = testing(data_split)
  cv_folds = spatial_clustering_cv(data_train, coords = c("longitude", "latitude"), v = 5)
  
  # Define general recipe for data transformation
  sdm_recipe = recipe(present ~ ., data = data_train) %>% 
    update_role(species, longitude, latitude, new_role = "ID vars") %>%     # Don't use these  as predictors
    step_log(num_range("bio_", 12:19), offset = 1) %>% 
    step_normalize(all_predictors()) %>%                                    # Center + rescale predictors
    step_corr(all_predictors(), threshold = 0.7) %>%                        # Remove predictors with r > 0.7
    step_poly(all_predictors(), degree = 2)                                 # Add polynomial terms
  
  # Define workflow using above recipe
  sdm_workflow = workflow() %>%
    add_recipe(sdm_recipe)
  
  # ---------------------------------------------------#
  ## Fit models ####
  ### GLM ####
  gc()
  glm_spec = logistic_reg(mode = "classification") %>% set_engine("glm")
  glm_workflow = sdm_workflow %>% add_model(glm_spec) 
  
  glm_fit = fit_resamples(glm_workflow,                              # No tuning needed -> cross validation only
                          cv_folds, 
                          metrics = metric_set(roc_auc),
                          control = control_grid(save_pred = T, save_workflow = T))
  
  ### MARS ####
  gc()
  mars_spec = mars(mode = "classification",
                   num_terms = tune(),                               # Number of retained features in the final model
                   prod_degree = tune()) %>%                         # Degree of interaction among features (1: purely additive, 2: two-way interaction, ...)
  set_engine("earth")
  
  mars_workflow = sdm_workflow %>% add_model(mars_spec)
  
  mars_fit = tune_grid(mars_workflow,
                       cv_folds,
                       grid = grid_latin_hypercube(
                         finalize(num_terms(), data_train),
                         prod_degree(),
                         size = 20
                       ),
                       metrics = metric_set(roc_auc),                  # Calculate AUC only
                       control = control_grid(save_pred = T, save_workflow = T))  # Save predictions for ensemble modeling
  
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
                       size = 50                                     
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
                        size = 50
                      ),
                      metrics = metric_set(roc_auc),                 # Calculate AUC only
                      control = control_grid(save_pred = T, save_workflow = T)) # Save predictions for ensemble modeling
  
  # ---------------------------------------------------#
  ## Stack models, save results ####
  model_stack = stacks() %>%           # Stack model predictions
    add_candidates(glm_fit) %>% 
    add_candidates(mars_fit) %>% 
    add_candidates(rf_fit) %>% 
    add_candidates(brt_fit)
  
  ensemble_pred = model_stack %>%       
    blend_predictions() %>%            # calculate weights for ensemble members
    fit_members()                      # re-train members with non-zero weight
  
  save(ensemble_pred, file = paste0("//import/calc9z/data-zurell/koenig/ensembles_fit/", str_replace(spec, " ", "_"), ".RData"))
}
