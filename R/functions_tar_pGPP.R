### Functions for target workflow


library(deSolve) #algal growth models
library(sf)
library(dplyr)
library(terra)

##############################
## dataframe with centroid for all lakes 
##############################

#eg_lake_centr <- st_centroid(eg_lake)%>% dplyr::select(lake_nhdID, Shape)

##############################
## Get PAR for all lakes
##############################
par_read <- function(){ #thus makes the tif a target and only needs to be run once for any of the HUCs unless the input raster is updated
  require(raster)
  PAR_Annual <- raster("3_DerivedData/usa_modis_mean_2015_2023.tif")
}
par_input <- function(input_sf, PAR_Annual) {
  require(raster)
  require(sf)
  require(dplyr)
    #raster::mean(PAR_GS)
    #raster::quantile(PAR_GS)
  
    #faster to just transform the polygon layer than the raster - just may just save and switch for batch processing
    lake_PAR <- st_transform(input_sf, crs = crs(PAR_Annual))

      par_mean <- raster::extract(PAR_Annual,             # raster layer
                                  st_zm(lake_PAR),    # spatial polygon for extraction
                                  fun=mean,   # what to value to extract spatially
                                  df=TRUE)%>%
      mutate(par_GS_umol = mean* 4.57) #convert W/m2 to umol/s/m2 *reminder W is J/s*

## add to lake input
    return(par_mean$par_GS_umol)
    }


##########################################
## MET data
##########################################
#download daymet
#extract X Y for lake polygon centroid
daymet_lake <- function(df_centroid){
  require(daymetr, sf, tidyverse, zoo)
  #require(EflowStats)
  
  #debug
  df_centroid <- tar_read(lake_pt)
  centr <- as.data.frame(st_coordinates(df_centroid))%>%
    mutate(
      site =as.factor(df_centroid$Permanent_Identifier),
      latitude = Y,
      longitude = X,
    #  area_km2 = df_centroid$AreaSqKM #remove for batch but lapply seems to be quick enough
    )%>%
    dplyr::select(-X, -Y)

#   write.csv(centr, "")
# # May 1 is the 121st day of the year (122nd in leap years)
# # October 31 is the 304th day of the year (305th in leap years) 
#   
#   ## added for ice-on determination 
#   met_lake <- download_daymet_batch(centr)
  
  # Burham and Shaman 2023
 # y = 3.3865ln(x) + 7.8032R² = 0.2623
# ##'We calculated freezing degree days (FDD) for the days preceding ice-on directly
#   from the lake’s daily air temperature time series (Figs. S2a, S3a). FDD was calculated
#   as a summation of the absolute values of all below zero air temperatures starting on
#   the date that the air temperature transitioned from being primarily positive to primarily
#   negative and ending on the date of ice-on (Figs. S2c, S3c). Similarly, we calculated
#   positive degree days (PDD) for the days preceding ice-off by summing the values of all
#   above zero air temperatures starting on the date that the air temperature transitioned
#   from being primarily negative to primarily positive and ending on the date of ice-off
#   (Figs. S2c, S3c).
##'
  

met_lake <- lapply(1:nrow(centr), function(i){
    met <- download_daymet(lat = centr$latitude[i], lon = centr$longitude[i], 
                         start = 2021, end = 2022)
    
    met_df <- met$data %>%
      dplyr::filter(year == 2021 & yday > 243 | year == 2022 & yday <244)%>% #185 days used as Growing Season before 274 is cotober 1, september 1 to make sur all freezing days are included. 
      mutate(
        date = paste0(year, "-", yday),
        date = as.Date(date, format = "%Y-%j"),
        area_km2 = centr$area_km2[i],
        WY = 2021, #add wateryear based on above calculations 
        tavg_C = (tmax..deg.c.+tmin..deg.c.)/2, #daily avg temp in C
        
        Freeze = ifelse(tavg_C < 0, 1, 0),
    #Freezing Degree Days
        FrDays = cumsum(Freeze),
        FrDD = cumsum(Freeze * abs(tavg_C)),
        DD= cumsum(tavg_C),
        DD_7 = rollmean(DD, k = 7, align = "right", fill = NA)

      )

    met_df$idx = index(met_df)
    library(Rbeast)
    library(segmented)
    #out=beast(dplyr::filter(met_df, date < as.Date("2022-04-01"))$DD, season = "none")
    total = segmented( lm(data = met_df, DD~idx), npsi = 2) #tried with more but 2 is best, slice not necessary but keeping it while methods play out
    
    #get the best two change points, tried a few different ways and this seems to be the most consistent
    psi_df <- as.data.frame(total$psi) %>%
      arrange(`St.Err`) %>%
      slice_head(n = 2)
    
    # on=segmented( lm(data = dplyr::filter(met_df, date < as.Date("2022-04-01")), DD~idx))
    # on$psi
    # off = segmented( lm(data = dplyr::filter(met_df, date > as.Date("2022-04-01")), DD~idx))
#     plot(out)
#     print(out)
#     # out$trend$cp
#     met_df$tr_sign <- out$trend$slp #probability of the sign being positive or negative - this is what we want. 
#     plot(met_df$tr_sign )
  


    #Determine Ice on and off dates based on Burham and Shuman 2023
    area_km2 = df_centroid$AreaSqKM[1] #remove for batch but lapply seems to be quick enough
    days_delay = ifelse(3.3865*log(area_km2) + 7.8032 > 0, 3.3865*log(area_km2) + 7.8032, 0)  # < 0.100 km2 ~ 0 delay small lakes 
    min_idx = round(min(psi_df$Est.) + days_delay, 0)
    #yday of the ice on and ice off
    on_date = dplyr::filter(met_df, idx ==  min_idx)$yday
    off_date = dplyr::filter(met_df, idx == round(max(psi_df$Est.), 0))$yday
    No_ice_dur = on_date - off_date
    
    #Plot
    ggplot(met_df)+
      geom_line(aes(date, DD))+
      geom_point(data = dplyr::filter(met_df, yday == on_date | yday == off_date), aes(date, DD))+
      theme_bw()
    
    ggsave(filename = paste0("4_FigureOutputs/DegreeDay_CHP_", df_centroid$Permanent_Identifier[i], ".png"))
    
    #1mm = 1mm3/m2 = 1L/m2
    met_GS <- met$data %>%
      mutate(
        date = paste0(year, "-", yday),
        date = as.Date(date, format = "%Y-%j")
      )%>%
      filter(yday >= off_date & yday <= on_date)%>%
          #dplyr::filter(between(yday, 120, 305)) %>% #185 days between - original growing season
          summarize(prcp_GS_sum_Lm2 = sum(prcp..mm.day.), #sum total for each year, later averaged. 
                    tmax_GS_max_C = max(tmax..deg.c.),
                    tmin_GS_min_C = min(tmin..deg.c.),
                    tavg_GS_avg_C = mean((tmax..deg.c.+tmin..deg.c.)/2),
                    srad_GS_avg = mean(srad..W.m.2.)
          )%>%
          ungroup()%>%
          #summarize for the years analyzed
          summarize_if(is.numeric, mean, na.rm = TRUE)#%>%
 
        #cbind(., as.data.frame(lapply(eg_lake[i,], rep, nrow(.)))) #add comid for joining if want to do for each year independantly
      }
      )%>% do.call("rbind", .)%>% #end lapply
      cbind(., df_centroid)

#Precipitation directly on lake over no ice / growing season
met_lake$prcp_in_GS <-  met_lake$prcp_GS_sum_Lm2 * met_lake$AreaSqKM * 1E6 #m2
met_lake$prcp_in_GS_d <-  met_lake$prcp_in_GS/No_ice_dur
met_lake$No_ice_dur_d <- No_ice_dur
met_lake$ice_on <- on_date
met_lake$ice_off <- off_date

    return(df_centroid)
}


##########################################
#Function light attenuation with depth based on DOC and algal 
##########################################
lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}

#Function to read in reparametrization 2025 (file provided by S.Jones)
parameter_readin <- function(filepath,perc_val = 50){
  
  #perc_val is the percentile with the default being the median
  #read in Kelly Reparametrized values
  df1 <- read.csv(filepath)%>%
    filter(X == perc_val/100) 
  
  list2env(df1, .GlobalEnv)#convert each column to global object 
  
}


### Determine DOC inputs for lakes ##
### Relationship Modified from Hararuk et al. 2022: https://doi.org/10.1029/2021GB007161
### Hydrologic Export of Soil Organic Carbon: Continental Variation and Implications

#Could be improved with soil model later, did not yet do that currently ph 6 is hardcoded 
doc_input <- function(input_df){
  require(magrittr)
# will draw distinction between lake with no stream inputs versus high 
  # Values from Table 3
  # Predictor Variables
  intercept = -2.00
  sqrt_wetlandp = 2.49
  sqrt_cropp = 0.79
  ln_pH = 1.77 #pH currently hardcoded as 6 - maybe adjust regionally? 
  ln_Q = -0.11 #used now; using total watershed flow to indicate flow in - thus incorporates GW and SW. 
  #mg/L
  #For class names see file HydroATLAS_v10_Legends.xlsx. Class grouping 1 (g1) represents all wetland classes (1-12) including lakes, reservoirs and rivers. Class grouping 2 (g2) represents all wetland classes (4-12) excluding lakes, reservoirs and rivers.
  

  
  input_df %<>%
    mutate( #Use Lake CAT data is available, if not use the HydroAtlas Data
      #Switch if Lakecat functionally in read_in_lakes can be activated. 
      # TOC_mgL = ifelse(!is.na(PCTHBWET2019CAT), exp(intercept + sqrt_wetlandp*sqrt(PCTHBWET2019CAT/100 + PCTWDWET2019CAT/100) + sqrt_cropp*(sqrt(PCTCROP2019CAT/100)) + ln_pH*log10(6)+ ln_Q* log(Flow_in_m3s)), 
      #                  exp(intercept + sqrt_wetlandp*sqrt(wet_pc_ug2/100) + sqrt_cropp*(sqrt(crp_pc_use/100)) + ln_pH*log10(6)+ ln_Q* log(Flow_in_m3s)))
      TOC_mgL = exp(intercept + sqrt_wetlandp*sqrt(wet_pc_ug2/100) + sqrt_cropp*(sqrt(crp_pc_use/100)) + ln_pH*log10(6)+ ln_Q* log(Flow_in_m3s))
      )
  

  #just return column as vector 
  return(input_df$TOC_mgL)
}

##################################
## Calculate Flow In based on Basin P - ET 

#pre_mm_uyr precipitation of HUC 10 Basin contained within 
#aet_mm_uyr actual evapotranspiration in of HUC 10 Basin contained within 

#################################################
##Read and Load HydroBasins/HydroAtlas with Lakes watersheds #################
## Modified from Solomon et al 2023
## Convert yearly Precipiation and Actual Evapor-Transpitation to Qin
## Represents Surface water and GW inputs, use the flow as input to DOC equation (instead of stream water)

waterbalance_in <- function(input_df){
  require(magrittr)
  require(readr)
  #Read in HydroBasin file
  file_path_hb10 <- "1_RawData/HydroBasin_wLakes/hyAtlas_clippedtoNorAmer.shp" #HydroAtlas database
  
  #HydroBasin HUC 10 Basin 
  hb_10 <- st_read(file_path_hb10) |> 
    st_transform(crs(input_df)) |> 
    dplyr::select(HYBAS_ID, MAIN_BAS, pre_mm_uyr, aet_mm_uyr, pet_mm_uyr, #mm = 1 litre of water per 1 m2 of ground
                  crp_pc_use, wet_pc_ug2) #HydroAtals Crop and wetland % for DOC calc
  
  #find centroid of input lake data 
  centr_lake <- st_centroid(input_df)
  
  #join center of lake to basin contained within
  list_out <- st_join(centr_lake, st_make_valid(hb_10), join = st_intersects)
  
  #determine HUC 4 for naming
  HUC04 <- substring(input_df$REACHCODE[1], 1, 4)
  
  
  list_out %<>% dplyr::mutate(
    HUC04 = HUC04,
    apr_recharge_Ly = pre_mm_uyr - aet_mm_uyr,
    Flow_in_m3s = ((apr_recharge_Ly * areasqm)/1000) / 31536000  #for Hararuk SOC model discharge needs to be in cubic meters per second,
  )
  
  write_csv(list_out, paste0("3_DerivedData/aetExtract", HUC04, "_HydroBASIN_10.csv")) #output aet
}


###################################
## SET UP Base NHD HR Lake Files 
#####################################

read_in_lakes <- function(HUC4 = "0202", 
                          NHD_path = "1_RawData/NHDPlus", 
                          data_path = file.path("1_RawData/LakeChar")){
  library(nhdplusTools)
  library(sf)
  library(tidyverse)
  library(magrittr)
  library(daymetr)
  
  theme_set(theme_bw(base_size = 9))
  library(dataRetrieval)
  sf_use_s2(FALSE) #makes intersect work    
  #Use HUC04 range to get an size distribution 
      nhdplusTools_data_dir(NHD_path)
      list.files(nhdplusTools_data_dir()) #goal is to get to a point to loop through these 
      
      #folder directory using HUC4 example
      nhd_file <- file.path(nhdplusTools_data_dir(), paste0("NHDPLUS_H_", HUC4, "_HU4_GDB"))
      hr <- get_nhdplushr(nhd_file, layers = "NHDWaterbody") #c("NHDFlowline", "NHDWaterbody", "NHDArea", "NHDPlusBurnWaterbody"))
      
      nhdhr_lake <- hr$NHDWaterbody %>%
        dplyr::filter(FCODE == 39004)%>%#Lake or Pond
        mutate(COMID = as.character(COMID))
      
      ####
      ## Spatial Join with NHD Plus (non HR data)
      ## This allows connection with LAGOS and LakeCat Datasets
      bbox <- sf::st_as_sfc(st_bbox(st_as_sf(nhdhr_lake))) #trim get data to the HR dataset
      
      #As of 4-17-2025 this is not working due to server error, tried to with example pulls and same error
      #Fixed 4-18-2024 by reloading package
      nhdp_lake <- get_waterbodies(bbox)%>% #NHDPlus DataSset 
        dplyr::filter(fcode == 39004)#Lake or Pond
        
        
      lake_df <- st_join(nhdhr_lake, nhdp_lake)%>%
        mutate(lakearea = ifelse((lakearea %in% 0) | (is.na(lakearea)), Shape_Area * 1E10, lakearea), #m #in the data anything less than 0.1 ha is labelled as zero, this conversion allows smaller lakes to be calculated by the polygon size
              #lakearea = Shape_Area * 1E10,
               areasqm = ifelse(is.na(AreaSqKM), lakearea, AreaSqKM * 1E6))
      # remove all variables except lake_df
      #rm(list=setdiff(ls(), "lake_df"))
      
      #Add Lake Volume from Ganz_2023, need other paper for links.  
      lake_net <- read.csv(file.path(data_path, "King_2021_nets_networkmetrics_medres.csv"))
      lake_vol <- read.csv(file.path(data_path, "Ganz_2023_lakesalldata_compiled_predictions.csv"))
      lake_link <- read.csv(file.path(data_path, "Smith_2021_lake_link.csv"))
      
      # #Lake Residence time based on Brooks et al. 2017 L&O figure6B
      # t = read.csv("G:/My Drive/Projects/UNDERC/200_Data/Brooks_2017_LO_Fig6B.csv")%>%
      #   dplyr::rename("cat_depth" = 2) #category depth
      
      
      #still missing a bunch - think it must be the <0.1 ha lakes. using this now and will come up with ways to fix
      lake_df <- dplyr::select(lake_net, c("lagoslakeid", "nhdplusv2_comid")) %>%
                              right_join(., lake_df, by = c( "nhdplusv2_comid" = "comid" )) %>%
        dplyr::filter(areasqm > 0)%>%
        dplyr::rename(COMID_hr = COMID)%>%
        dplyr::relocate(COMID_hr)%>%
        left_join(lake_vol)%>%
        dplyr::mutate(
      maxdepth = ifelse(!is.na(best_maxdepth), best_maxdepth, ifelse(is.na(maxdepth) | maxdepth == 0, areasqm^(0.16/2), maxdepth))
      )%>% #if no maxdepth use the hurst equation using exponent in Ganz (assuming lakes without depth are small as this should be the mismatch between lagos and NHD HR dataset)
      drop_na(maxdepth) #make sure all dataframe has depth estimate
      
        # # add Brooks ~ residence time based on max depth
        # mutate(#categorize depth based on ranges in Brooks 2017 L&O
        #   cat_depth = ifelse(best_maxdepth < 2, 2,
        #                      ifelse(best_maxdepth < 3, 3,
        #                             ifelse(best_maxdepth < 5, 5, 
        #                                    ifelse(best_maxdepth < 10, 10, 
        #                                           ifelse(best_maxdepth < 20, 20, 99999)))))
        # )%>%
        # left_join(., t)
      

      
      ggplot(nhdhr_lake)+
        geom_point(aes(areasqm * 1E-6, AreaSqKM))+
        geom_abline(slope = 1, intercept = 0)
      # it hink bcause AreaSqKM trunates too early. 
      
      # nhdp_lake <- get_waterbodies(AOI = bbox)%>% #NHDPlus DataSset 
      #   dplyr::filter(fcode == 39004)
      
      #require(StreamCatTools)
      ## Currently : HTTP 503 Service Unavailable (4-11-2025), but can use other land use char for DOC, if change, make sure to update doc_in as well 
      # comids <-StreamCatTools::lc_nlcd(comid = as.character(nhdp_lake$comid), year = "2019")
      # region_params <- lc_get_params(param='areaOfInterest')
      # 
      #catchment <- hr$NHDPlusCatchment
      
      ### NHDPLUS comid has not link to nhdHR - but the lake values match, so st_join seems like the right approach
      #test_inter = st_join(nhd_lake, nhdp_lake)
      #lcat_id <- readRDS("data/VPU_0202_NLCD19.RDS")#%>% #from Example_LakeCat_Download.R
      # rename("COMID" = "comid")
      
      # lake_df <-  st_join(nhdhr_lake, nhdp_lake)%>%
      #   mutate(lakearea = ifelse((lakearea %in% 0) | (is.na(lakearea)), Shape_Area * 1E10, lakearea)) #in the data anything less than 0.1 ha is labelled as zero, this conversion allows smaller lakes to be calculated by the polygon size
      
      # #make a complete dataframe with all HR lakes and lakecat data 
      # lake_df <-  nhdhr_lake %>% #, nhdp_lake st_join(
      #   left_join(lcat_id)%>%#lower case comid is from NHDplus (not HR)
      #   dplyr::filter(FTYPE == 390)
      
      ##################################################################
      ## PLOTS for input lake shape data
      ##################################################################
      # #Cone Volume - Only lakes > 1 ha, which seems fine. 
      # lake_df %>%
      #   mutate(areasqkm = areasqkm * 1E6) %>%
      #   pivot_longer(names_to = "area_m2", cols = c("lakearea", "areasqkm")) %>%
      #   ggplot(.)+ #plot desnity function for just lakes > 1 ha, versus all in HR LakePond dataset
      #   geom_density(aes(lakevolume, group = area_m2, color = area_m2))+
      #   scale_x_log10()
      # ggsave(paste0("4_FigureOutputs/", HUC4, "_lakeConeVolume.png"))
      
      #Cone Volume (Ganz 2023)  vs. Surface Area
      lake_df %>%
        ggplot(.)+ #plot desnity function for just lakes > 0.1 ha, versus all in HR LakePond dataset
        geom_point(aes(x = lakearea, y =lakevolume))+
        scale_x_log10()+
        scale_y_log10()
      ggsave(paste0("4_FigureOutputs/", HUC4, "_lakeareavConeVol.png"))
      
      saveRDS(lake_df, paste0("2_DataProcessing/NHD", HUC4, "_lakedf.RDS"))
      
    return(lake_df)
}


################################
### Determine TP inputs for lakes ##
## Phosphorus     ##################
################################
#Alexander, Richard B., and Gorman Sanisaca, Lillian. (2019). RSPARROW: An R system for SPARROW modeling [Software release]. U.S. Geological Survey. DOI: https://doi.org/10.5066/P9UAZ6FO 
#Geospatial data from the links provided below contain the information needed to reproduce the maps and model results shown here and described in Ator, 2019 (https://doi.org/10.3133/sir20195118).
#Midwest Sparrow: https://doi.org/10.3133/sir20195114
#fixes all SPARROW input data 
tp_munge_sparrow <- function(filepath = "1_RawData/SPARROW_output_TP/"){
  require(dplyr)
  #may need to run fix pacific if initial data from source, as pacific does not have q embedded in tp dataset
  list.tp <- lapply(list.files(filepath, full.names = TRUE), function(tp_file){
    
    df <- read.csv(tp_file)%>%
      mutate(file_region = basename(tp_file)) %>%
      dplyr::select(1:5, file_region)
    
    #streamline all naming conventions
    colnames(df) <- tolower(colnames(df))
    
    #in one file the order of these are reversed - this is to correct that inconsistency
    df <- df %>%
      relocate(cumareakm2, .after = incareakm2)%>%
      dplyr::rename(
        "comid" = 1,
        "flow_avg_cfs" = 4,
         "TP_accumload" = 5
      )
    
    df$CONCENTRAT_TPmgL = (df[,5] * 1E6) / (df[,4]* 8.93E8) #ACCL (acculmated load) * 1E6 / flowcfs (avg flow) * 8.93E8
      
    return(df)
  })
  #add rename for q
    df.tp <- do.call("rbind", list.tp)
    
    saveRDS(df.tp, "df_tp_sparrowCombined.RDS")
    write.csv(df.tp, "df_tp_sparrowCombined.csv")
    
    df.tp %<>% 
      drop_na(CONCENTRAT_TPmgL) 
    
    return(df.tp)
    
}

#fix Pacific 
# all other regions have accumlated flow in cfs baked in to the TP output, pacific does not, but reports it in wb output from same data release
# this joins together for use 
fix_pacific <- function(){
  require(dplyr)
  pac_df_q <- read.delim("1_RawData/pac_sparrow_model_output_wb.txt")
  pac_df_tp <- read.csv("1_RawData/pac_sparrow_model_output_tp.txt")

  pac_output <- left_join(pac_df_q, pac_df_tp, by = "comid")%>%
    dplyr::select(
      comid, CumAreaKM2.x, IncAreaKm2.x, al_q, al_tp
    )%>%
    dplyr::rename(
      "cumareakm2" = CumAreaKM2.x,
       "incareakm2" =  IncAreaKm2.x
    )
  
  write.csv(pac_output, "1_RawData/SPARROW_output_TP/pac_sparrow_model_output_tp_qadded.txt", row.names = FALSE)
}

#Combine catchments with TP data, from respective Science Base data releases, files already live within GIT repo, but if adjustment is needed code is here
SPARROW_catchments <- function(){
  require(stringr)
  df_tp_sparrowCombined <- readRDS("df_tp_sparrowCombined.RDS")
  print("Midwest")
  #SPARROW MIDWEST
  dt_tp <- read_sf("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/mw_cats/mw_cats.shp")
  dt_tp$HUC04 <- str_sub(dt_tp$HUC8, 1,4) #add huc 4
  dt <- left_join(dt_tp, df_tp_sparrowCombined, by = c("COMID" = "comid"))%>%
    mutate(
      region = "MW"
    )

  saveRDS(dt, "2_DataProcessing/SPARROW_sf_MW.RDS")

  #clean up and set up output for model
  output <- dplyr::select(dt, region, COMID, HUC04, CONCENTRAT_TPmgL)


  #SPARROW SOUTHEAST
  print("Southeast")
  dt_tp <- read_sf("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/se_catchments/se_catchments.shp")
  dt_tp$HUC04 <- str_sub(dt_tp$HUC8, 1,4) #add huc 4
  dt <- left_join(dt_tp, df_tp_sparrowCombined, by = c("COMID" = "comid"))%>%
    mutate(
      region = "SE"
    )

  saveRDS(dt, "2_DataProcessing/SPARROW_sf_SE.RDS")


  #clean up and set up output for model
  output <- rbind(output, dplyr::select(dt, region, COMID, HUC04, CONCENTRAT_TPmgL))

  # output <- rbind(readRDS("2_DataProcessing/SPARROW_sf_MW.RDS"),
  #                 dplyr::select(readRDS("2_DataProcessing/SPARROW_sf_MW.RDS"), region, COMID, HUC04, CONCENTRAT_TPmgL))
  # 
  ## SPARROW NORTHEAST
  print("Northeast")
  dt <- read_sf("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/northeast_cats_results_tp/ne_cats_tp.shp")%>%
    mutate(CONCENTRAT_TPmgL = CONCENTRAT,
           COMID = COMID_ORIG,
           HUC04 = NA)%>%
    mutate(
      region = "NE"
    )%>%
    dplyr::select(
      region, COMID, HUC04, CONCENTRAT_TPmgL
    )
  
  #clean up and set up output for model
  output <- rbind(output, dplyr::select(dt, region, COMID, HUC04, CONCENTRAT_TPmgL))
  
  saveRDS(dt, "2_DataProcessing/SPARROW_sf_NE.RDS")
  
  ## SPARROW Southwest
  dt_tp <- read_sf("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/sw_cats/sw_cats/SW_CATS.shp")
  dt_tp$HUC04 <- str_sub(dt_tp$HUC8, 1,4) #add huc 4
  dt <- left_join(dt_tp, df_tp_sparrowCombined, by = c("COMID" = "comid"))%>%
    mutate(
      region = "SW"
    )%>%
    dplyr::select(
      region, COMID, HUC04, CONCENTRAT_TPmgL
    )
  
  saveRDS(dt, "2_DataProcessing/SPARROW_sf_SW.RDS")
  
  #clean up and set up output for model
  output <- rbind(output, dplyr::select(dt, region, COMID, HUC04, CONCENTRAT_TPmgL))
  
  ## SPARROW Pacific
  dt_tp <- read_sf("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/PAC_CATS/PAC_CATS.shp")
  #dt_tp$HUC04 <- str_sub(dt_tp$HUC8, 1,4) #add huc 4
  dt <- left_join(dt_tp, df_tp_sparrowCombined, by = c("COMID" = "comid"))%>%
    mutate(
      HUC04 = NA,
      region = "SW"
    )%>%
    dplyr::select(
      region, COMID, HUC04, CONCENTRAT_TPmgL
    )
  
  saveRDS(dt, "2_DataProcessing/SPARROW_sf_PAC.RDS")
  
  #clean up and set up output for model
  #output <- rbind(output, dplyr::select(dt, region, COMID, HUC04, CONCENTRAT_TPmgL))
  
  return(output)
}

#Tar function to find TP concentration for all the files. 
tp_input <- function(input_centroid, tp_poly){
  
  require(readr)
  #plan joining the outputs together 
  lake_centr_TP <- st_transform(input_centroid, crs = crs(tp_poly))
  
  # Spatial join and output a polygon with the joined attributes, stuck here....
  lake_region = st_join(lake_centr_TP, tp_poly) 
  #Download proper region SPARROW files, then join for TP concentration, right now assumes that all are in the same refgion, which seems correct if looping through HUC04s
  region = first(lake_region$region)
  sparrow_df <- read_rds(paste0("2_DataProcessing/SPARROW_sf_", region ,".RDS")) 

  #Link inputs with catchment TP concentration 
  TP <- st_join(x = lake_centr_TP, y = sparrow_df)

#Add P to input file 
  TP_mgL = TP$CONCENTRAT_TPmgL
}

###
#Concentration
# The Accumulated Load divided by the Accumulated Flow. This should be interpreted as the mean-annual flow-weighted concentration. Units are milligrams per liter.
# 
# average flow is in cfs
# accumlated load is kg/yr

# cfs -----> L/yr = cfs * 8.93e+8
# kg/yr ---> mg/yr = kg/yr * 1E6
# # mg/L 
# ACCL (acculmated load) * 1E6 / flowcfs (avg flow) * 8.93E8
# (31.6840 * 1E6) /  (1.5189 * 8.93E8) #result = 0.0233 mg/L; ne results 0.0229
# (53.8492 * 1E6) /  (3.1402 * 8.93E8) #result = 0.0192 mg/L; ne results 0.0189
# (80.7354 * 1E6) /  (4.5220 * 8.93E8) #result = 0.01999 mg/L; ne results 0.0196


#https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}

##############################
## Model Dataframe Set up

model_df <- function(mod_in, HUC4){
  require(tidyr)
  mod_in %>%
  dplyr::filter(maxdepth > 0)%>%
  drop_na(doc_in, par_in, tp_in, maxdepth)
  
  write_csv(mod_in, paste0("3_DerivedData/DF_MODin_", HUC4, ".csv"))
  
  return(mod_in)
}

########################################
## ODE modelling functions 

# ## Kelly et al. "Equilibrium areal lake GPP estimates were the product of equilibrium phytoplankton growth rate, phytoplankton
# #volumetric density, and mixed-layer depth (rAzmix).
# ## Static inputs - determine how to use ranges from Olesky Paper

ODE_huisman <- function(mod_in, HUC4){
  require(deSolve)
  require(dplyr)
  require(pbapply)
  #for percent output
  i = 0 
  total = nrow(mod_in)
  
  #ODE Run through columns 
  mod_output <-pbapply(mod_in, 1, function(input_df, parameter_filepath = "3_DerivedData/KellyModel_reparameterized_2025-02-04.csv"){

  #print(input_df$Permanent_Identifier)#for debugging if necessary 
    
  #Set up input variables 
  I0 <- input_df$par_in#umol photons m-2 s-1; Incoming Light
  SA <- input_df$areasqm #m2
  inputDOCin <- input_df$doc_in #10 # g/m3 #(not mg) DOC concentration total (not divided)
  zmax= ifelse(input_df$maxdepth < 0.1, input_df$maxdepth_m_prediction_khazaei, input_df$maxdepth) #there are some 0 in maxdepth, Khazaei is most comparable to Ganz
  Pin= input_df$tp_in * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]
  DOCin = input_df$doc_in
  
  #Read in all the parameterized values
  parameter_readin(parameter_filepath, perc_val = 50) #Median Parameterized Values

  # kDOC <- 0.00042 #m2mgC-1 Kelly 2018 Light attenuation coefficient of DOC
  # kA <- 0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
  # lA=0.1  #day-1 Loss rate of phytoplankton
  # pA=1.2  #day-1 Maximum production rate of phytoplankton
  # hA=55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
  # mA=2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
  # decay=0.001 #day-1 #DOC mineralization rate

  # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P

  #?? they used max 20 to calculate lim
  #Qin=input_df$flow_GS_inlet/365/1000 #185/1000 #calculate flow in m3 per day
  #Qin_precip = input_df$prcp_in_GS/1000 #m3 per  #* 1000 / 185 if growing season
  Qin = input_df$Flow_in_m3s * 86400 # m3/ day  #watershed flux, removed daymet precip in calculated for growing season currently
  ####Light availability

  tryCatch( {zmix=10^(-0.515*log10(DOCin)+0.115*log10(2*sqrt(SA/pi))+0.991)
  if(zmix>zmax){zmix=zmax}
  }, error = function(e) {zmix = 5})#"modal [max] depth for all size classes is  5 m" (Ganz 2023)

  #Volume only ratio needed for calculations, so the general shape equation is less important
  V_epi = ((4/3) * SA*zmix)/2 #m3 Epilim  Volume #uses total DOC input (not epi)
  V_lake = 	((4/3) * SA*zmax)/2 #m3  Volume input_df$lakevolume #((4/3) * SA * zmax)/2 ##Total Lake Volume
  V_epiRatio = V_epi/V_lake
  
  # A is what we are solving for
  A = 100 # initial values
  kD <- kDOC * DOCin + kA * A
  I_zm <- lightAtten(zmix, I0, kD)

  # water in should be mean precip * areal
  #
  times=1:10000

  #huis_run <-  lapply(1:nrow(df), function(i){
  parms=c(I0 = I0,
          SA=SA,# m2
          zmix = zmix,
          zmax= zmax,
          Pin= Pin * V_epiRatio,#min $P_mgL #Total that enters epi
          DOCin= DOCin * V_epiRatio,##Total that enters epi
          V_lake = V_lake,
          V_epi = V_epi,
          V_ratio = V_epiRatio,
          Qin = Qin * V_epiRatio,#Total that enters epi, as Qin is for dilution amount
          #parameters
          kDOC=kDOC,
          kA=kA,
          lA=lA,
          pA=pA,
          hA=hA,
          mA=mA,
          decay=decay,
          cA=cA,
          v=v,
          rec=rec)

  # starting state variables
  n<-c(A=100,P=Pin,DOC=DOCin)
  # simulate with ode
  run=ode(y=n,times=times,func=huisman,parms=parms)%>%
    dplyr::last()%>%
    cbind(., as.data.frame(t(parms)))%>%
    mutate(Permanent_Identifier = input_df$Permanent_Identifier)#add name
  
  
  
  #return(run[nrow(run)])
  #})%>% do.call("rbind",.)

})%>% do.call("rbind",.)
  
  write.csv(mod_output, paste0("5_ModelOutputs/HuisModelRun_", HUC4, "_", Sys.Date(),".csv"))
  
  return(mod_output)
}

#Attribute Label: wbm_meanq
#Attribute Definition:
#Mean streamflow, cubic feet per second derived from the attribute al_q in the 2012 SPARROW streamflow model, table mw_sparrow_model_output_FLOW.txt and modified by reach decay or source attributes in the mw_sparrow_model_input.txt table.
# #Attribute:
# Attribute Label: al_tp
# Attribute Definition:
#   Accumualated total phosphorus load, kg/yr



# #Attribute Label: al_q
# Attribute Definition:
#   Accumualated streamflow, cfs
# Attribute Definition Source:
#   Producer defined
# Attribute Domain Values:
#   Range Domain:
#   Range Domain Minimum: 0
# Range Domain Maximum: 2325417.296
# Attribute Units of Measure: cubic feet per second
# Attribute:
#   Attribute Label: al_q_paet
# Attribute Definition:
#   Accumulated streamflow from precipitation minus actual evapostranspiration, cfs
# Attribute Definition Source:
#   Producer defined
# Attribute Domain Values:
#   Range Domain:
#   Range Domain Minimum: 0
# Range Domain Maximum: 657254.6948
# Attribute Units of Measure: cubic feet per second
# Attribute Label: flux_665_final
# Attribute Definition:
# #   Mean annual total phosphorus load related to station_id_tp for water years 2000-2014 detrended to water year 2012, kg/yr
# Attribute Label: al_tp
# Attribute Definition:
#   Accumulated total phosphorus load, kg/yr
# Attribute Definition Source:
#   Producer defined
# Attribute Domain Values:
#   Range Domain:
#   Range Domain Minimum: 0
# Range Domain Maximum: 14886582.1
# Attribute Units of Measure: kilograms per year
