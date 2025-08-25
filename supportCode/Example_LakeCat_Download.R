#############
## Lake Cat 
##############

library(remotes)
#install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
library(StreamCatTools)



bbox <- sf::st_as_sfc(st_bbox(st_as_sf(nhd_lake)))
#nhd <- nhdplusTools::get_nhdplus(bbox, realization = "all")#get NHDPlusV2 (not HR)
nhdp_lake <- get_waterbodies(bbox)
nhdp_lake <- st_make_valid(nhdp_lake)
nhdhr_lake <- st_make_valid(nhdhr_lake)
# hr from NHDPlus HR
# nhd_lake <- hr$NHDWaterbody
comid_lake <- as.character(nhdp_lake$comid)

lcat_id <- lapply(comid_lake, function(id) {
  lc_nlcd(year = "2019", comid = id)})%>%
    do.call("rbind",.)
lcat_id %<>% rename("comid" = "COMID")

saveRDS(lcat_id, "data/VPU_0202_NLCD19.RDS")

lake_df <-  st_join(nhdhr_lake, nhdp_lake)%>%
  left_join(lcat_id)


saveRDS(lake_df , "data/lake_df_hrLakeCat.RDS")
#####################################



