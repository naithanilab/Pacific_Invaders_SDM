library(tidyverse)
library(sf)
library(geosphere)
library(dggridR)
library(maps)
library(PresenceAbsence)
library(rvest)
library(taxize)

get_status_powo = function(spec){
  # Function to download the biogeographical status for a given species from http://www.plantsoftheworldonline.org/

  # Prepare species name
  spec_split = str_split(spec, pattern = " ", simplify = T)
  genus = spec_split[1]
  species = spec_split[2]
  
  # Retrieve IPNI ids of species
  ipni_ids = ipni_search(genus = genus, species = species) %>% pull(id)
  
  # Search each IPNI_id on POWO and scrape status information
  distr = lapply(ipni_ids, function(id){
    distr = tryCatch({
      on.exit(closeAllConnections())
      url = paste0("http://www.plantsoftheworldonline.org/taxon/urn:lsid:ipni.org:names:", id)
      website = url %>%  read_html
      native = website %>% html_nodes(xpath = "//h3[contains(., 'Native to:')]/following-sibling::p[position()=1]") %>% 
        html_text() %>% 
        str_squish() %>% 
        str_split(", ") %>% 
        as_vector()
      introduced = website %>% html_nodes(xpath = "//h3[contains(., 'Introduced into:')]/following-sibling::p[position()=1]") %>% 
        html_text() %>% 
        str_squish() %>% 
        str_split(", ") %>% 
        as_vector()
      return(list(native = native, introduced = introduced))
    }, error = function(e){
      return(NULL)
    })
  })
  
  # 
  distr_vec = distr[!sapply(distr, is.null)] %>%  unlist()
  distr_vec = distr_vec[!is.na(distr_vec)]
  native = unique(distr_vec[grepl("native", names(unlist(distr_vec)))])
  introduced = unique(distr_vec[grepl("introduced", names(unlist(distr_vec)))])
  
  # Combine and return reults
  cat(str_glue("{spec} (native: {length(native[!is.na(native)])}, introduced: {length(introduced[!is.na(introduced)])})"), "\n")
  if(!(is.null(native) & is.null(introduced))){
    distr_df = bind_rows(tibble(species = spec, tdwg_lvl_3 = native, status = "native"),
                         tibble(species = spec, tdwg_lvl_3 = introduced, status = "introduced")) %>% 
      drop_na()
    return(distr_df)
  } else {
    return(NULL)
  }
}

# Convenience function for plotting the status of a species from the blacklist
plot_status = function(occs, 
                       species = NA, 
                       bbox = c(-180, -60, 180, 80), 
                       status = c("native", "introduced", "unknown"),
                       alpha = NA,
                       title = NA){
  if(!is.na(species)){
    df_plot = dplyr::filter(occs, species == !!species & status %in% !!status)
  } else {
    df_plot = dplyr::filter(occs, status %in% !!status)
  }
  if(!is.na(species) & is.na(title)){
    title = species
  } else {
    title = ""
  }
  
  world = map_data("world")
  if(nrow(df_plot) == 0){return("No matching occurrences")}
  if(is.na(alpha)){alpha = 1/log10(nrow(df_plot))}

  ggplot(df_plot, aes(x = lon, y = lat, color = status)) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
    geom_point(shape = 1, alpha = alpha) +
    scale_color_manual(values = c(native = "#038cfc", "introduced" = "#ff4040", unknown = "black")) +
    ggtitle(title) +
    xlim(bbox[1], bbox[3]) +
    ylim(bbox[2], bbox[4]) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_bw()
}

thin_coordinates_distance = function(coords, threshold = 5000){
  # Function for reasonably fast spatial thinning (faster than spThin, bit still O(n²)) of coordinates based on pairwise distances between coordinates
  # coords: geographic coordinates in lon/lat format
  # threshold: minimum distance between points in meters
  
  coords_dist = geosphere::distm(coords)                        # Great circle distance between points
  coords_dist[upper.tri(coords_dist, diag = T)] = NA            # Triangular distance matrix
  coords_flagged = which(coords_dist < threshold, arr.ind=TRUE) # Two-column matrix of coordinate-pairs that are below threshold
  pts_remove = c()
  while(nrow(coords_flagged) > 0){
    pts_count = sort(table(c(coords_flagged)))                  # Count number of occurrences of points in flagged coords
    pts_count_max = pts_count[pts_count == max(pts_count)]      # Points with most neighbors below threshold
    pt_target = as.integer(sample(names(pts_count_max), 1))     # Choose one randomly
    pt_ind = which(coords_flagged[,1] == pt_target | coords_flagged[,2] == pt_target) # Find all pairs that involve target point
    coords_flagged = coords_flagged[-pt_ind,,drop = F]          # Remove those pairs from coords_flagged
    pts_remove = c(pts_remove, pt_target) 
  }
  return(coords[-pts_remove,])
}

thin_coordinates_sample = function(coords, target_res = 13){
  # Function for very fast spatial thinning of coordinates based on sampling over hexagonal grid
  # coords: geographic coordinates in lon/lat format
  # target_res: target resolution of the grid as defined by dggridR, see https://github.com/r-barnes/dggridR for details
  
  hex_grid = dggridR::dgconstruct(res = target_res)
  hex_ids = dggridR::dgGEO_to_SEQNUM(hex_grid, coords[,"lat"], coords[, "lon"])$seqnum
  
  coords_thinned = bind_cols(coords, hex_id = hex_ids) %>% 
    rowid_to_column("row_id") %>% 
    group_by(hex_id) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    arrange(row_id) %>% 
    dplyr::select(lon, lat) %>% 
    as.matrix
  
  return(coords_thinned)
}

make_preds = function(model, new_data) {
  # Wrapper for making predictions on new data
  # Adapted from Zurell (2020) JBi
  
  switch(class(model)[1],
         glm = predict(model, new_data, type='response'),
         gam = predict(model, new_data, type='response'),
         randomForest = predict(model, new_data, type= 'response'), 
         gbm = predict.gbm(model, new_data, n.trees=model$gbm.call$best.trees, type="response"))
}

make_cv_preds = function(model, spec_data) {
  # Wrapper for making cross-validated predictions from a fitted model
  # Adapted from Zurell (2020) JBi
  
  # Select relevant columns and complete cases
  spec_data = spec_data %>% 
    rowid_to_column("row_id") %>% 
    drop_na() %>% 
    mutate(prediction = NA)
  
  df_fit = spec_data %>% dplyr::select(row_id, present, fold_id, starts_with("bio"))
  df_pred = spec_data %>% dplyr::select(row_id, fold_id, prediction)
  
  # Loop over folds and save predictions
  for(i in seq_len(n_distinct(df_fit$fold_id))){
    train_data = filter(df_fit, fold_id != i)
    test_data = filter(df_fit, fold_id == i)
    
    # Because we used the gbm.step() for BRTs, we need a small work-around:
    if (class(model)[1]=='gbm') {
      train_data = train_data %>% rename(y.data = present)
    }
    
    # We update the model for the new training data
    model_k = switch(class(model)[1],
                     glm = update(model, data=train_data, weights = model$weights[train_data$row_id]),
                     gam = update(model, data=train_data, weights = model$weights[train_data$row_id]),
                     randomForest = update(model, data=train_data),				
                     gbm = gbm(model$call, data = train_data, n.trees = model$gbm.call$best.trees))
    
    # Predict to left out fold
    df_pred[df_pred$fold_id == i, "prediction"] = make_preds(model_k, test_data)
  }
  return(df_pred)
}  

calc_performance = function(model, df_spec, df_pred, threshold_method = 'MaxSens+Spec'){
  # Helper functions for True Skill Statistic
  # Adapted from Zurell (2020) JBi
  
  TSS = function(cmx){
    PresenceAbsence::sensitivity(cmx, st.dev=F) + PresenceAbsence::specificity(cmx, st.dev=F) - 1
  }
  
  df_eval = df_spec %>% 
    rowid_to_column("row_id") %>% 
    inner_join(df_pred, by = "row_id") %>% 
    dplyr::select(row_id, present, prediction)
  
  threshold = PresenceAbsence::optimal.thresholds(df_eval)
  cmx_max = PresenceAbsence::cmx(df_eval, threshold=threshold[threshold$Method==threshold_method, 2])
  
  df_performance = data.frame(model = paste(class(model)[1]),
                              AUC = PresenceAbsence::auc(df_eval, st.dev=F),
                              TSS = TSS(cmx_max), 
                              Sens = PresenceAbsence::sensitivity(cmx_max, st.dev=F),
                              Spec = PresenceAbsence::specificity(cmx_max, st.dev=F),
                              threshold = threshold[threshold$Method == threshold_method, 2])
  return(df_performance)
}