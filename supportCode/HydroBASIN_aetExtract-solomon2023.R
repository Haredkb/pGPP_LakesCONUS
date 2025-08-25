library(tidyverse)
library(sf)
library(raster)
################# Read and Load HydroBasins watersheds #################
## Modified from Solomon et al 2023
## 
# # set your own working directory to downloaded 

read_hydroinput <- function(df_lake){

file_path_hb10 <- "C:/Users/hared/Dropbox/ResearchProjects/_GeneralData_/HydroBasin_wLakes/hyAtlas_clippedtoNorAmer.shp"

hb_10 <- st_read(file_path_hb10) |> 
  st_transform(crs(df_lake)) |> 
  dplyr::select(HYBAS_ID, MAIN_BAS, pre_mm_uyr, aet_mm_uyr, pet_mm_uy)

#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average

# Extract over raster
aet.extract = tibble(
  Permanent_ID = rep(NA, nrow(df_lake)),
  nws_zoneid = rep(NA, nrow(df_lake)),
  ws_zoneid = rep(NA, nrow(df_lake)),
  OBJECTID = rep(NA, nrow(df_lake)),
  HYBAS_ID = rep(NA, nrow(df_lake)),
  pre_mm_uyr = rep(NA, nrow(df_lake)),
  aet_mm_uyr = rep(NA, nrow(df_lake)),
  pet_mm_uyr = rep(NA, nrow(df_lake))
)

extract = list()
# extract by 10 watersheds at once
usei = seq(0,nrow(hb_10), by = 10)
for (i in usei) {
  print(i)
  
  extract[[((i+10)/10)]] = st_intersects( hb_10, df_lake) |> 
    as_tibble() |> 
    dplyr::select(!geometry)
} 

centr_lake <- st_centroid(df_lake)

list_out <- st_join(centr_lake, st_make_valid(hb_10), join = st_intersects)

extract.df = do.call(rbind.data.frame, extract)
HUC04 <- substr(first(df_lake$REACHCODE), 1,4)
write_csv(list_out, 'data/aetExtract_HydroBASIN_10.csv')

return(extract.df)
}

