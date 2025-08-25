#########################
## Example Lake GPP CONUS Analysis
## Subset Example Lakes
## 2023-01-23
## D.Hare
#########################
#https://rdrr.io/github/USGS-R/lakeattributes/man/get_area.html # need to look at !
#Libraries
library(nhdplusTools)
library(sf)
library(tidyverse)
library(magrittr)
library(daymetr)

theme_set(theme_bw(base_size = 9))
library(dataRetrieval)
sf_use_s2(FALSE) #makes intersect work

#input data
#nhd
###https://cran.r-project.org/web/packages/nhdplusTools/vignettes/nhdplushr.html
#(hr_urls <- download_nhdplushr(work_dir, "06", download_files = FALSE))
nhdplusTools_data_dir("C:\\Users/hared/Dropbox/ResearchProjects/_GeneralData_/NHDPlus")
list.files(nhdplusTools_data_dir()) #goal is to get to a point to loop through these 

#folder directory using HUC4
nhd_file <- file.path(nhdplusTools_data_dir(), "NHDPLUS_H_0202_HU4_GDB")
hr <- get_nhdplushr(nhd_file, layers = c("NHDFlowline", "NHDWaterbody", "NHDArea", "NHDPlusBurnWaterbody", 
                                         "NHDPlusCatchment"))
# sapply(hr, nrow) #number of flowlines, waterbodies and areas
# print(sapply(hr, nrow)[2])

## Make Lake Dataframe 
nhdhr_lake <- hr$NHDWaterbody %>%
  dplyr::filter(FCODE == 39004)%>%#Lake or Pond
  mutate(area_m2 = Shape_Area * 1E10,
         COMID_hr = as.character(COMID)) #unit conversion as 0 area is AreaSqKm column 1E6 * 1E-4

ggplot(nhdhr_lake)+
  geom_point(aes(area_m2 *1E-6, AreaSqKM))+
  geom_abline(slope = 1, intercept = 0)
# it hink bcause AreaSqKM trunates too early. 
#get just the nhdPlus (not HR) for the same area as the hr dataset - the comid are different!
bbox <- sf::st_as_sfc(st_bbox(st_as_sf(nhdhr_lake))) #trim get data to the HR dataset

plot(bbox)

nhdp_lake <- get_waterbodies(AOI = bbox)%>% #NHDPlus DataSset 
  dplyr::filter(fcode == 39004)

catchment <- hr$NHDPlusCatchment

### NHDPLUS comid has not link to nhdHR - but the lake values match, so st_join seems like the right approach
#test_inter = st_join(nhd_lake, nhdp_lake)
lcat_id <- readRDS("data/VPU_0202_NLCD19.RDS")#%>% #from Example_LakeCat_Download.R
  # rename("COMID" = "comid")

# lake_df <-  st_join(nhdhr_lake, nhdp_lake)%>%
#   mutate(lakearea = ifelse((lakearea %in% 0) | (is.na(lakearea)), Shape_Area * 1E10, lakearea)) #in the data anything less than 0.1 ha is labelled as zero, this conversion allows smaller lakes to be calculated by the polygon size

#make a complete dataframe with all HR lakes and lakecat data 
lake_df <-  nhdhr_lake %>% #, nhdp_lake st_join(
  left_join(lcat_id)%>%#lower case comid is from NHDplus (not HR)
  dplyr::filter(FTYPE == 390)

#Example Subset DataSet
eg_lake <- drop_na(lake_df, comid)#[1:100,]

saveRDS(eg_lake, "data/eg_lake_0202.RDS")

eg_lake <- readRDS("data/eg_lake_0202.RDS")

#test tp 
ne_tp <- read.csv("G:/My Drive/Projects/UNDERC/200_Data/SPARROW/ne_sparrow_model_output_tp.txt")

test <- left_join(eg_lake, ne_tp, by = "comid")
#Find centroid for some analysis (met)
eg_lake_centr <- st_centroid(eg_lake)%>% dplyr::select(Permanent_Identifier, Shape)

#nhd_lakeB  <- hr$NHDPlusBurnWaterbody

#flines <- hr$NHDFlowline
streams <- hr$NHDFlowline%>%
  dplyr::filter(FTYPE == 460)#River/Stream

catchment_conn <- st_intersects(y = catchment, x = eg_lake)

streams <- st_make_valid(streams)
flines_conn <- st_intersects(y = streams, x = eg_lake)

## Extract all flines that intersect with 
l_conn <- lapply(flines_conn, function(i){
  streams[i,]
})

eromma <- st_read(file.path(nhdplusTools_data_dir(), "NHDPLUS_H_0202_HU4_GDB", "NHDPLUS_H_0202_HU4_GDB.gdb"),
             layer = "NHDPlusEROMMA")%>%
  dplyr::select(1:2)

#QAMA: FlowEstARunoffMA: Mean annual flow from runoff

lake_SW <- lapply(1:length(l_conn), function(i){
        
        eg_f <- st_as_sf(as.data.frame(l_conn[i][1]))%>%
          tibble::rowid_to_column(., var = "L1")# add index as colname to join with points
        
        if(nrow(eg_f) != 0){
              lake <- st_cast(lake_df[i,], "MULTILINESTRING")
              
              sw <- as.data.frame(st_coordinates(eg_f))%>%
                group_by(L1)%>%
                slice(c(n()))%>% #keep last of the multilinestring
                st_as_sf(., coords = c("X", "Y"), crs = st_crs(eg_f))%>%
                tibble::rowid_to_column(., var = "p_ID")%>%
                left_join(., st_drop_geometry(eg_f), by = "L1")%>%
                #dplyr::filter(FTYPE == 460)%>%#keep only streams and rivers
                left_join(., eromma, by = c("COMID" = "NHDPlusID"))
              
              sw_in <- sw %>%# add average annual flow. 
                st_join(., eg_lake[i,]) %>%
                mutate(sw_contr = if_else(TotDASqKM < WSAREASQKM, #if the stream outlet area is less than lake area 
                                          "inlet", "outlet"))
        
        return(sw_in)}else(return(NA)
          )

}) #end lapply


names(lake_SW) <- eg_lake$Permanent_Identifier[1:100]

# for(j in seq(1,100)){
#   noSW <- tryCatch(
#     st_as_sf(as.data.frame(lake_SW[j])),
#     error=function(e) e
#   )
#   
#   if(!inherits(noSW, "error")){
#     p <- ggplot()+
#       geom_sf(data = st_as_sf(eg_lake[j,]), color = "grey", fill = "blue")+
#       #geom_sf(data = st_as_sf(as.data.frame(l_conn[j])))+
#       geom_sf(data = st_as_sf(as.data.frame(lake_SW[j])), aes(
#         color = sw_contr,
#         size = TotDASqKM))
#     ggsave(p, path = "plots/lake_indiv_img/", filename = paste0("lake_", unique(st_as_sf(as.data.frame(lake_SW[j]))$Permanent_Identifier), ".jpg"))
#   }
# }

#CReate output with all the lake input and outputs  


fl_output <- do.call("rbind", lake_SW[!is.na(lake_SW)]) %>% #remove empty list (no inflow or outflow)
  mutate(lake_nhdID = Permanent_Identifier.y,
         sw_contr = as.factor(sw_contr))

##List what lakes do not have inlet or outlet - isolated lakes
eg_lake$lake_nhdID <- eg_lake$Permanent_Identifier
df_iso <- st_drop_geometry(eg_lake) %>%
  dplyr::filter(!lake_nhdID %in% st_drop_geometry(fl_output)$lake_nhdID)

#For lakes with inlet(s) and/or outlet(s) sum growing season water input and cummlative surface area
## May 1 is the 121st day of the year (122nd in leap years)
# October 31 is the 304th day of the year (305th in leap years)
# 185 full days of growing season analyzed

df_sw <- st_drop_geometry(fl_output)%>%
  group_by(lake_nhdID, sw_contr) %>%
  summarize(  
              flow_Lsavg = QAMA * 28.3168,
              flow_GS = sum(QAMA)* 28.3168* 185 * 24*60*60, #cfs to L per growing season 
              num = n(),
              sqkm = sum(TotDASqKM)
  )%>%
  pivot_wider(names_from = sw_contr, values_from = c(flow_Lsavg, flow_GS, sqkm, num))

# Add all rows without a sw connection
df_output <- bind_rows(
                      df_sw, data.frame(lake_nhdID = df_iso$lake_nhdID))%>%
  mutate_if(is.numeric , replace_na, replace = 0)
  #replace(is.na(.), 0) #all NAs as 0

#download daymet
#extract X Y for lake polygon centroid
centr <- as.data.frame(st_coordinates(eg_lake_centr))
# May 1 is the 121st day of the year (122nd in leap years)
# October 31 is the 304th day of the year (305th in leap years) 


met_lake <- lapply(1:nrow(centr), function(i){
                    met <- download_daymet(lat = centr$Y[i], lon = centr$X[i], 
                                           start = 2021, end = 2022)
                    #1mm = 1mm3/m2 = 1L/m2
                    met$data %>%
                      dplyr::filter(between(yday, 120, 305)) %>% #185 days between
                      group_by(year)%>%
                      summarize(prcp_GS_sum_Lm2 = sum(prcp..mm.day.),
                                tmax_GS_max_C = max(tmax..deg.c.),
                                tmin_GS_min_C = min(tmin..deg.c.),
                                tavg_GS_avg_C = mean((tmax..deg.c.+tmin..deg.c.)/2),
                                srad_GS_avg = mean(srad..W.m.2.)
                      )%>%
                      ungroup()%>%
                      #summarize for the years analyzed
                      summarize_if(is.numeric, mean, na.rm = TRUE)%>%
                      cbind(., eg_lake[i,])
  #cbind(., as.data.frame(lapply(eg_lake[i,], rep, nrow(.)))) #add comid for joining if want to do for each year independantly
}
)%>% do.call("rbind", .)#end lapply

#Precipiation directly on lake over growing season
met_lake$prcp_in_GS <-  met_lake$prcp_GS_sum_Lm2 * met_lake$area_m2 # met_lake$AreaSqKM * 1e6




# Need to calc ET - using Hargraves but want to use Penman
# https://qed.epa.gov/hms/hydrology/evapotranspiration/algorithms/
df_output <- left_join(df_output, dplyr::select(met_lake, lake_nhdID, COMID, prcp_in_GS, 
                                                srad_GS_avg,
                                                tmax_GS_max_C,
                                                tmin_GS_min_C,
                                                tavg_GS_avg_C))%>%
  mutate(
    COMID = as.character(COMID),
    IN_GS_ML = sum(prcp_in_GS + flow_GS_inlet)/1E6,
    IN_pct_sw = (flow_GS_inlet/1E6)/IN_GS_ML,
    # uses 1 W m-2 = 0.0864 MJ m-2 day-1 as a direct multiplier to get from W/m2/d to MJ/m2/Hour.
    # output is in/day, thus 1in3/in2 == 25.4 L/m2/day
    E_GS_harg_ML = (25.4 *(0.0023 * srad_GS_avg * 0.0864 * (tmax_GS_max_C - tmin_GS_min_C)^0.5 * (tavg_GS_avg_C +17.8)) * 185)/1E6, #for growing season
    OUT_GS_ML = sum(flow_GS_outlet/1E6, E_GS_harg_ML),
    NET_GS_ML = OUT_GS_ML - IN_GS_ML #(- lake net loss, + lake net gain)
  )
  

input_lake <- st_sf(left_join(df_output, eg_lake, by = c( "lake_nhdID" = "Permanent_Identifier"))) #m3



saveRDS(input_lake, "data/eg_lake_0202.RDS")


#3 This is moved to function
###############
## PAR Inputs
################
PAR_GS <- raster("G:/My Drive/PAR_growingseason_2021_v5-0000000000-0000000000.tif")

#for 
raster::mean(PAR_GS)
raster::quantile(PAR_GS)
# plot(PAR_GS,
#      xlim=c(-127, -65),  ## with c()
#      ylim=c(25, 50)  ## with c())
# )
#PAR_GS <- projectRaster(PAR_GS, crs=crs(eg_lake))
#faster to just transform the polygon layer than the raster - just may just save and switch for batch processing
eg_lake_PAR <- st_transform(input_lake, crs = crs(PAR_GS))

par_mean <- raster::extract(PAR_GS,             # raster layer
                            st_zm(eg_lake_PAR),    # spatial polygon for extraction
                            fun=mean,   # what to value to extract spatially
                            df=TRUE)%>%
  mutate(par_GS_umol = layer * 4.57) #convert W/m2 to umol/s/m2 *reminder W is J/s*

## add to lake input
input_lake$par_GS_umol <- par_mean$par_GS_umol

#create table 
tbl_df <- st_drop_geometry(head(input_lake, n = 15))%>%
  dplyr::select(VPUID, lake_nhdID, IN_GS_ML,IN_pct_sw, OUT_GS_ML, NET_GS_ML, sqkm_inlet, sqkm_outlet, area_m2, par_GS_umol, tavg_GS_avg_C, num_inlet, num_outlet, )

library(kableExtra)
kbl(tbl_df, escape = F, caption="Table 1: Growing Season Hydrologic Fluxes and Meterological Inputs (ML = Megaliter; GS = Growing Season (185 days))",
    digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  # pack_rows("Watershed 1", 1, 4) %>%
  # pack_rows("Watershed 5", 5, 8) %>%
  # pack_rows("Watershed 6", 9, 12)
save_kable(file ="Table_HydrologicFluxes.png")




###################33
### ADD VOLUME
data_path <- file.path("G:/My Drive/Projects/UNDERC/200_Data/LakeChar")

lake_net <- read.csv(file.path(data_path, "King_2021_nets_networkmetrics_medres.csv"))
lake_vol <- read.csv(file.path(data_path, "Ganz_2023_lakesalldata_compiled_predictions.csv"))
lake_link <- read.csv(file.path(data_path, "Smith_2021_lake_link.csv"))

#still missing a bunch - think it must be the <0.1 ha lakes. using this now and will come up with ways to fix
lake_vol <- left_join(lake_vol, dplyr::select(lake_net, c("lagoslakeid", "nhdplusv2_comid")))%>%
  dplyr::mutate(COMID = as.character(nhdplusv2_comid))

#### NEED TO SORT OUT LINKING THESE DATA TOGETHER
#using mean verus max depth for calculations 
wb_lake <- left_join(df_output, lake_link, by = c("lake_nhdID" = "lake_nhdid")) #m3

input_lake$nhdplusv2_comid <- as.integer(input_lake$lake_nhdID)
wb_lake1 <- left_join(input_lake, lake_net, by = "nhdplusv2_comid") #m3

wb_lake_clean <- inner_join(wb_lake, lake_vol, by = "lagoslakeid")%>%
  mutate(HRT = cone_volume/ IN_GS *2) # Volume divided input for 1 year





#FeatureType (FType) categories are commonly used for symbolization (Estuary =493, LakePond = 390, SwampMarsh = 466, Constructed Reservoir =436, Ice Mass =378, and Playa =361).
lith <-  read.csv("G:/My Drive/Projects/UNDERC/200_Data/LakeCat_Lithology.csv",
                  colClasses=c("COMID"="character"))

nlcd19 <- read.csv("C:/Users/hared/Dropbox/ResearchProjects/006_LakeGPP_CONUS/400_Analysis/GPP_CONUS_Lakes/data/NLCD2019/NLCD2019.csv",
              colClasses=c("COMID"="character"))

# weighted avg. #total flux/ sum flow# 


## Only Lake/Ponds

wb_lake <- nhd_lake %>%
  mutate(COMID = as.character(COMID))%>%
  left_join(., nlcd19, by = c("Permanent_Identifier" = "COMID"))

plot(wb_lake[9:10,6])


#Subset Lakes to Practice
eg_lake <- wb_lake[9:10,]
saveRDS(eg_lake, "data/eg_lake_02.RDS")

###########################
## Which flowlines contribute
start_COMID <- eg_lake$COMID[1]
UM_COMIDs <- get_UT(dt, start_COMID)




library(plotly)
map_nhd <- ggplot()+
  geom_sf(data = nhd_lake, color = "grey", fill = "blue")+
  geom_sf(data = df_output, aes(color = lake_nhdID))
ggplotly(map_nhd)

# 
# https://www.usgs.gov/national-hydrography/value-added-attributes-vaas
# This is what you need to know about FROMNODE and TONODE:
#   
#   Endpoints of flowlines are nodes and each node is identified by a unique number
# 
# Nodes are conceptual and not expressed in a feature class
# 
# FROMNODE is the upstream end of a flowline
# 
# TONODE is the downstream end of a flowline
# 
# The relationship between from- and to-nodes can be used to navigate the network 


#####################
# #Lake Volume 
# #https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1387.1
# 
# ##Ganz et al. 2023: need to link LAGOS dataset with NHD, and will use King et al. 2021
# #King, K.B., Q. Wang, L.K. Rodriguez, M. Haite, L. Danila, P. Tan, J. Zhou, and K.S. Cheruvelil. 2021. LAGOS-US NETWORKS v1.0: Data module of surface water networks characterizing connections among lakes, streams, and rivers in the conterminous U.S ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/98c9f11df55958065985c3e84a4fe995 (Accessed 2024-01-24).
# data_path <- file.path("G:/My Drive/Projects/UNDERC/200_Data/LakeChar")
# 
# lake_net <- read.csv(file.path(data_path, "King_2021_nets_networkmetrics_medres.csv"))
# lake_vol <- read.csv(file.path(data_path, "Ganz_2023_lakesalldata_compiled_predictions.csv"))
# 
# #still missing a bunch - think it must be the <0.1 ha lakes. using this now and will come up with ways to fix
# lake_vol <- left_join(lake_vol, dplyr::select(lake_net, c("lagoslakeid", "nhdplusv2_comid")))%>%
#   dplyr::mutate(COMID = as.character(nhdplusv2_comid))
# 
# #using mean verus max depth for calculations 
# wb_lake <- left_join(wb_lake, lake_vol) #m3
# 
# wb_lake_clean <- inner_join(wb_lake, lake_vol)