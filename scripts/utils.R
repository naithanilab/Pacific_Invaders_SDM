library(tidyverse)
library(sf)
library(hexbin)
library(maps)


check_objects = function(){
  if(! "occ" %in% names(.GlobalEnv)){
    load("data/occ.RData", envir = .GlobalEnv)
  }
  if(! "world" %in% names(.GlobalEnv)){
    assign("world", map_data("world"), envir = .GlobalEnv)
  }
}

# Plot Functions
plot_occ_overview = function(){
  check_objects()
  occ_overview_maps = ggplot(occ, aes(x = lon, y = lat)) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
    geom_hex(bins = 100) +
    ylim(-90,90) +
    scale_fill_continuous(type = "viridis", trans = "log10") +
    facet_wrap(vars(status)) +
    coord_fixed() +
    theme_bw() 
}

plot_status = function(species = NA, 
                       bbox = c(-180, -60, 180, 80), 
                       status = c("native", "naturalized", "invasive", "non-native", "unknown"),
                       alpha = NA,
                       title = NA){
  check_objects()
  
  if(!is.na(species)){
    df_plot = dplyr::filter(occ, species == !!species & status %in% !!status)
  } else {
    df_plot = dplyr::filter(occ, status %in% !!status)
  }
  if(!is.na(species) & is.na(title)){
    title = species
  } else {
    title = ""
  }

  if(nrow(df_plot) == 0){return("No matching occurrences")}
  if(is.na(alpha)){alpha = 1/log10(nrow(df_plot))}
 
  ggplot(df_plot, aes(x = lon, y = lat, color = status, text = paste("status source:", status_source))) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
    geom_point(shape = 1, alpha = alpha) +
    scale_color_manual(values = c(native = "#038cfc", "non-native" = "#ffff52", naturalized = "#ffc252", invasive = "#ff5e52", unknown = "black")) +
    ggtitle(title) +
    xlim(bbox[1], bbox[3]) +
    ylim(bbox[2], bbox[4]) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_bw()
}

reclassify_species = function(occs){
  occs_status = occs %>% 
    filter(status != "unknown") %>% 
    mutate(native = factor(ifelse(status == "native", "native", "non_native")))
  
  knn_model = nearest_neighbor() %>% 
    set_engine("kknn") %>% 
    set_mode("classification") %>% 
    translate()
  
  knn_fit = knn_model %>% 
    fit(native ~ lon + lat, data = occs_status)
  
  occs_pred = knn_fit %>% 
    predict(dplyr::select(occs, lon, lat), type = "prob") %>% 
    rename_with(str_replace, pattern = "[.]pred", replacement = "p")
  
  result = tibble(occs_pred, dplyr::select(occs, lon, lat))
  # ggplot(filter(result, p_native > 0.95), aes(x = lon, y = lat)) + 
  #   geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
  #   coord_fixed() +
  #   geom_point(shape = 1, color = "#038cfc") +
  #   ggtitle(occs$species[1]) +
  #   xlim(-180, 180) +
  #   ylim(-60, 80) +
  #   theme_bw()
}