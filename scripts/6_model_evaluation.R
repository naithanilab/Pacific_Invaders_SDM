library(tidyverse)
library(raster)
library(dismo)
library(mgcv)
library(gbm)
library(randomForest)

rm(list = ls())
setwd("~/PacificInvadersSDM/")
source("scripts/utils.R")

load("//import/calc9z/data-zurell/koenig/blacklist_final.RData")
bioclim = raster::stack(str_sort(list.files("//import/calc9z/data-zurell/koenig/chelsa_bioclim/", pattern = ".tif", full.names = T), numeric = T))
names(bioclim) = str_replace(names(bioclim), pattern = "CHELSA_bio10", "bio")
bioclim_hawaii = crop(bioclim, extent(c(-161, -154, 18.5, 22.5)))

# -------------------------------------------------- #
#                Predict to Hawaii                ####
# -------------------------------------------------- #
filepath = sample(list.files("//import/calc9z/data-zurell/koenig/models_fit", full.names = T), 1)
spec = filepath %>% str_remove("//import/calc9z/data-zurell/koenig/models_fit/") %>% 
  str_remove(".RData") %>% str_replace("_", " ")
  
load(filepath)
bioclim_subset = subset(bioclim_hawaii, grep("bio", colnames(results_list$data$data_regr), value = T))

plot_status(filter(blacklist_final, species == spec), species = spec)
plot_status(filter(blacklist_final, species == spec), species = spec, bbox = c(-161, 18.5,-154,22.5))

pred_glm = raster::predict(bioclim_subset, results_list$models$glm, type = "response")
pred_gam = raster::predict(bioclim_subset, results_list$models$gam, type = "response")
pred_rf = raster::predict(bioclim_subset, results_list$models$rf, type = "response")
pred_brt = raster::predict(bioclim_subset, results_list$models$gbm, type = "response")

plot(pred_glm); points(filter(blacklist_final, species == spec)[c("lon", "lat")], pch = 20, cex = 0.5)
plot(pred_gam); points(filter(blacklist_final, species == spec)[c("lon", "lat")], pch = 20, cex = 0.5)
plot(pred_rf); points(filter(blacklist_final, species == spec)[c("lon", "lat")], pch = 20, cex = 0.5)
plot(pred_brt); points(filter(blacklist_final, species == spec)[c("lon", "lat")], pch = 20, cex = 0.5)

pred_stack = raster::stack(pred_glm, pred_gam, pred_rf, pred_brt)
pred_ensemble = calc(pred_stack, median)
plot(pred_ensemble, main = spec)
points(filter(blacklist_final, species == spec)[c("lon", "lat")], pch = 20, cex = 0.5)