#####################################
# Target Workflow for CONUS Lake pGPP
#####################################

# Date Created: 2024-03-08
# Creator: Danielle Hare // dhare@umass.edu

#Run Before starting
#install.packages("targets")
require(targets)
require(tibble)
library(tarchetypes)
library(readr)
#Set Data Location for Folder Directory 1_RawData and 2_DataProcessing
data_dir = "C:/Users/dhare/Dropbox/ResearchProjects/006_LakeGPP_CONUS/400_Analysis/GPP_CONUS_Lakes"
#List the HUC4 Values you have, need to match downloaded NHD files in 1_RawData> NHDPlus
HUC4_run = c("1301", "0202", "0401", "0402")
# may need to set wd if not set in project 
#setwd("G:/Shared drives/Hare and Solomon CONUS GPP")

#then to run script type tar_make(), script will run all output objects that have had scripts modified. 

######################################
#All functions to run script
source("R/functions_tar_pGPP.R")
source('R/helper_func_og.R') #has Huisman 
# Set target-specific options such as packages:
# tar_option_set(packages = "utils") # nolint

#GOAL to auto run all HUC 4 
    #set list of HUC 4 values wanted
list(
  targets::tar_target(tp_poly, read_sf(paste0(data_dir,"/2_DataProcessing/TP_Regions/SPARROW_regions.shp"))),

  targets::tar_target(par_raster, par_read()), #this will only run initially and if raster is updated, new HUC will not need new run
  
  tarchetypes::tar_map(
    values = list(HUC4 = HUC4_run), # static_config is a simple vector
    names = "HUC4",
            # #combine SPARROW DATA to useable format
            #tar_target(TP_df, SPARROW_catchment()), #saves locally to access within TP_in, in base folder structure

            #read in base NHD HR file
          #targets::tar_target(HUC4, "1301"), #right now just have to change this huc4 code to read in desired NHD files, currently needs to have the data within that Raw Folder, as of 2025-04-16 just have a few select examples
          targets::tar_target(input_df, st_as_sf(read_in_lakes(HUC4))), #next step set up to run through all NHD files; running with head for now seems to be hitting machine memory loads
            #tar_target(input_df, head(st_as_sf(read_in_lakes(HUC4)), 100)), #if short chunk need to be run
          targets::tar_target(lake_pt, st_centroid_within_poly(input_df)), #find center of polygon, if center is not within polygon, then selects location that is
            #
            # Setting up modelling input
            # # Read in Hydro Input Data
          targets::tar_target(water_input, waterbalance_in(input_df, data_dir = data_dir)),

            #Calculate Direct Input Precip and Air Temperature
          #tar_target(met_df, daymet_lake(lake_pt)), #currently not used, but may be useful/needed for ice determination

            # Calculate DOC input for each lake, output is a column
          targets::tar_target(doc_in, doc_input(water_input)),

            # Calculate PAR
          targets::tar_target(par_in, par_input(lake_pt, par_raster)),

            #Create total Phosphorus column
          targets::tar_target(tp_in, tp_input(lake_pt, tp_poly, data_dir)),
            #create df
          targets::tar_target(df_in, cbind(water_input, doc_in, par_in, tp_in)),
          targets::tar_target(df_in2, clean_df(df_in, HUC4)),

            #set up model input file
          targets::tar_target(mod_in, model_df(df_in2, HUC4)),

            #Run equibrium model and output csv to 5_ModelOutput
          targets::tar_target(ODE_output, ODE_huisman(mod_in, HUC4))
        )
      #)
)
