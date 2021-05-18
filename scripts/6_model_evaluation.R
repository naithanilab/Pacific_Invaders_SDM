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

load("//import/calc9z/data-zurell/koenig/blacklist_final.RData")
bioclim = raster::stack(str_sort(list.files("//import/calc9z/data-zurell/koenig/chelsa_bioclim/", pattern = ".tif", full.names = T), numeric = T))
names(bioclim) = str_replace(names(bioclim), pattern = "CHELSA_bio10", "bio")
bioclim_hawaii = crop(bioclim, extent(c(-161, -154, 18.5, 22.5)))

# -------------------------------------------------- #
#                Predict to Hawaii                ####
# -------------------------------------------------- #
spec = "Calotropis gigantea"

load(paste0("//import/calc9z/data-zurell/koenig/models_fit/", str_replace(spec, " ", "_"), ".RData"))
bioclim_subset = subset(bioclim_hawaii, grep("bio", colnames(results_list$data$data_regr), value = T))

plot_status(filter(blacklist_final, species == spec))
plot_status(filter(blacklist_final, species == spec), bbox = c(-161, 18.5,-154,22.5))

pred_glm = raster::predict(bioclim_subset, results_list$models$glm, type = "response")
pred_gam = raster::predict(bioclim_subset, results_list$models$gam, type = "response")
pred_rf = raster::predict(bioclim_subset, results_list$models$rf, type = "response")
pred_brt = raster::predict(bioclim_subset, results_list$models$gbm, type = "response")

plot(pred_glm); points(filter(blacklist_final, species == spec)[c("lon", "lat")])
plot(pred_gam); points(filter(blacklist_final, species == spec)[c("lon", "lat")])
plot(pred_rf); points(filter(blacklist_final, species == spec)[c("lon", "lat")])
plot(pred_brt); points(filter(blacklist_final, species == spec)[c("lon", "lat")])

pred_stack = raster::stack(pred_glm, pred_gam, pred_rf, pred_brt)
pred_ensemble = calc(pred_stack, median)
plot(pred_ensemble, main = spec)