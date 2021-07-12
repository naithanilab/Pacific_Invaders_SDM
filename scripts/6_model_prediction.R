library(tidyverse)
library(raster)
library(mecofun)
library(dismo)
library(mgcv)
library(gbm)
library(randomForest)

rm(list = ls())
setwd("~/PacificInvadersSDM/")
source("scripts/utils.R")

bioclim = raster::stack(str_sort(list.files("//import/calc9z/data-zurell/koenig/chelsa_bioclim/", pattern = ".tif", full.names = T), numeric = T))
names(bioclim) = str_replace(names(bioclim), pattern = "CHELSA_bio10", "bio")
bioclim_hawaii = crop(bioclim, extent(c(-161, -154, 18.5, 22.5)))
bioclim_subset = subset(bioclim_hawaii, grep("bio", colnames(results_list$data$data_reg), value = T))

# ------------------------------------------ #
#            Predict to Hawaii            ####
# ------------------------------------------ #
# Individual algorithms
load("//import/calc9z/data-zurell/koenig/ensembles_fit/Psidium_guajava.RData")

plot_status("Psidium guajava", bbox = c(-161, 18.5,-154,22.5), alpha = 1)
pred_glm = raster::predict(bioclim_subset, results_list$models$glm, type = "response")
pred_gam = raster::predict(bioclim_subset, results_list$models$gam, type = "response")
pred_rf = raster::predict(bioclim_subset, results_list$models$rf, type = "prob")
pred_brt = raster::predict(bioclim_subset, results_list$models$brt, type = "response")

plot(pred_glm); points(filter(occ, species == "Psidium guajava")[c("lon", "lat")])
plot(pred_gam); points(filter(occ, species == "Psidium guajava")[c("lon", "lat")])
plot(pred_rf); points(filter(occ, species == "Psidium guajava")[c("lon", "lat")])
plot(pred_brt); points(filter(occ, species == "Psidium guajava")[c("lon", "lat")])

# Ensemble predictions