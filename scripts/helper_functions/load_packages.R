# /////////////////////////////////////////////////////////////////////////
#
# Load and/or install the R packages needed for data cleaning steps and SDM.
#
# /////////////////////////////////////////////////////////////////////////


# Function to load and/or install packages.
load_install_packages <- function(pckgs){
  # Install packages (if not already installed)
  inst_pckg <- pckgs %in% installed.packages()
  if(length(pckgs[!inst_pckg]) > 0) install.packages(pckgs[!inst_pckg])
  # Load packages into session:
  print(sapply(X = pckgs, FUN = require, character.only = TRUE))
}


load_install_packages(pckgs = c("data.table", 
                                "sp",
                                "sf",
                                "geosphere",
                                "rgeos",
                                "rgdal",
                                "mapview",
                                "raster",
                                "CoordinateCleaner",
                                "countrycode", 
                                "ggplot2",
                                "parallel",
                                "doParallel"))

rm(load_install_packages)

# Helpful for data.table, rounds numbers to 11 significant figures. Very
# important when joining, grouping or ordering numeric columns like geographic
# coordinates see ?setNumericRounding for details.
setNumericRounding(2)
