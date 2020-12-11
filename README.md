Scripts for cleaning GBIF & BIEN occurrences for running species distribution modeling.

# Notes

See also the log folder and the log.rmd file.

For the data cleaning steps, see also the GitHub issues from 3 to 9, e.g. https://github.com/idiv-biodiversity/PacificInvaders/issues/3


On 10.11.20 13:23, Dylan Craven wrote:

- we have downloaded all GBIF and BIEN occurrences for both GLONAF and PIER lists. some species didn't have any occurrences.
- you started to clean the data set using 'CoordinateCleaner' and matching coordinates to country/region polygons to be able to classify them as being in native or alien range

**GloNAF** – Global Naturalized Alien Flora

https://glonaf.org/

'GloNAF (Global Naturalized Alien Flora) is a living database project about alien plant species and became a synonym for many related projects dealing with all kinds of scientific and policy relevant questions and studies about alien species (also other taxa) and related data'

See also dataset at iDiv database https://idata.idiv.de/DDM/Data/ShowData/257

**PIER** - Pacific Island Ecosystems at Risk

http://www.hear.org/pier/

'Pacific Island Ecosystems at Risk provides listings and descriptions of plant species that threaten ecosytems of the Pacific islands.  Also listed are many other invasive and potentially invasive plant species present in and around the Pacific region.'

# Scripts

In the scripts folder, order matters:

- *data_prep_gadm_to_binary.R* transforms the GADM shapefile gadm36_0.shp to binary format (for faster reading when needed). 
  - Input: "gadm36_0.shp" (file from https://gadm.org/, version 3.6, 2018-12-19);
  - Output: "gadm36_0.rds";
- *data_prep_geoentities_to_binary.R* same as above, but for the GloNAF shapefile from Patrick Weigelt.
  - Input: "geoentities.sh" (file from Dylan Craven via DropBox link sent on 2018-11-29. File created by Patrick Weigelt, version 2018-05-09);
  - Output: "geoentities_2018-05-09.rds";
- *data_prep_clean_alien_status_data.R* creates a cleaner tabular version of alien status information for each unique combination of species and location (state and island id-s). The output table will be used for merging with the cleaned occurrence data in order to assign alien status information to each record.
  - Input:
    - "Pacific_Invaders_0707.csv", this file was stored on https://github.com/idiv-biodiversity/PacificInvaders/tree/master/Original_Data , estimation of last modified date: 2018-11-28;
    - "googlenames_gift.csv", "State_ID.csv", files from Michael Wohlwend;
  - Output: "pi_alien.csv";
- *data_prep_make_botanical_garden_buffers.R* makes 10 km geodesic buffers around botanical gardens for occurence cleaning purposes (eliminate occurences around botanical gardens).
  - Input: "BGCI_garden_coordinates.csv", file from Dylan Craven sent on 2017-07-27;
  - Output: "botanical_gardens_buffers.rds";
- *data_prep_intersect_gadm.R* intersects raw occurrence data (GBIF + BIEN) with the detailed GADM shapefile in order to get country ISO and name for cleaning purposes down the line.
  - Input:
    - "PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData", file created by Dylan Craven with detailed meta-data;
    - "gadm36_0.rds", file created with *data_prep_gadm_to_binary.R* above;
  - Output:
    - "occ_gadm.rds", occurence data with corresponding country ISO and name;
    - "spp_list.rds", this is unmodified as existing in the input "PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData", it was saved separetly to avoid high loading times if ever needed;
- *data_prep_clean_occ.R* cleanes the occurence data based on certain criteria and spatial filters (see script outline).
  - Input:
    - "occ_gadm.rds", file generated with *data_prep_intersect_gadm.R* above;
    - "botanical_gardens_buffers.rds", file generated with *data_prep_make_botanical_garden_buffers.R* above;
  - Output: "occ_clean.rds";
- *data_prep_intersect_glonaf.R* intersects cleaned occurrence data with GloNAF shapefile to retreive geoentity ID-s for each record.
  - Input:
    - "occ_clean.rds", file produced with *data_prep_clean_occ.R* above;
    - "geoentities_2018-05-09.rds", file produced with *data_prep_geoentities_to_binary.R* above;
  - Output: "occ_clean_glonaf.rds";
- *data_prep_merge.R* merges cleaned occurrence data with alien status information.
  - Input:
    - "occ_clean_glonaf.rds", file produced with *data_prep_intersect_glonaf.R* above;
    - "pi_alien.csv", file produced with *data_prep_clean_alien_status_data.R* above;
  - Output: "occ_clean_glonaf.rds";
  
**Other helper scripts that are now obsolete**

- *data_prep_check_state_id.R* This script is obsolete now. The corrections were already manually implemented in the data.
- *data_prep_merge_alien_status_to_occ.R* A first attempt of merging cleaned occurrence data with alien status information and running some extra test.

