
#Run Before starting
#install.packages("targets")
library(targets)

#then to run script type tar_make(), script will run all output objects that have had scripts modified. 
#####################################
# Target Workflow for CONUS Lake pGPP
#####################################

# Date Created: 2024-03-08
# Creator: Danielle Hare // hared@caryinstitute.org
# may need to set wd if not set in project 
#setwd("G:/Shared drives/Hare and Solomon CONUS GPP")
######################################
#All functions to run script
source("R/functions_tar_pGPP.R")
source('R/helper_func_og.R') #has Huisman 
# Set target-specific options such as packages:
# tar_option_set(packages = "utils") # nolint
targets::tar_script({
# Workflow, change input to lake 
list(
  #GOAL to auto run all HUC 4 
  tarchetypes::tar_map(
    list(HUC4 = c("1301", "0202", "0401"),
         # #combine SPARROW DATA to useable format 
         #tar_target(TP_df, SPARROW_catchment()), #saves locally to access within TP_in, in base folder structure 
         tar_target(tp_poly, read_sf("2_DataProcessing/TP_Regions/SPARROW_regions.shp")),
  #read in base NHD HR file
  #tar_target(HUC4, "1301"),#right now just have to change this huc4 code to read in desired NHD files, currently needs to have the data within that Raw Folder, as of 2025-04-16 just have a few select examples
  tar_target(input_df, st_as_sf(read_in_lakes(HUC4))), #next step set up to run through all NHD files; running with head for now seems to be hitting machine memory loads
  #tar_target(input_df, head(st_as_sf(read_in_lakes(HUC4)), 100)), #if short chunk need to be run
  tar_target(lake_pt, st_centroid_within_poly(input_df)), #find center of polygon, if center is not within polygon, then selects location that is
  # 
  # Setting up modelling input 
  # # Read in Hydro Input Data 
  tar_target(water_input, waterbalance_in(input_df)),
  #Calculate Direct Precip and Air Temperature 
  #tar_target(met_df, daymet_lake(lake_pt)), #currently not used
  
  # Calculate DOC input for each lake, output is a column
  tar_target(doc_in, doc_input(water_input)),
  
  # Calculate PAR
  tar_target(par_raster, par_read()), #this will only run initally and if raster is updated, new HUC will not need new run
  tar_target(par_in, par_input(lake_pt, par_raster)),
  


  
  #Create total Phosphorus column
  tar_target(tp_in, tp_input(lake_pt, tp_poly)),
  
  #set up model input file
  tar_target(mod_in,model_df(cbind(water_input, doc_in, par_in, tp_in), HUC4)),  
  
  
  tar_target(ODE_output, ODE_huisman(mod_in, HUC4))
)
