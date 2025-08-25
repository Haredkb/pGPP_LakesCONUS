########
# Explore if streamstats have be used to delinate lake catchment, not on stream network
##########
devtools::install_github("markwh/streamstats")
library(streamstats)

eg_lake <- st_sf(head(lake_df))
#centroid of lake. 

# #this approach does not work for lakes. you can't use lat/long of lake to force stream cat to calc basin. THere are "excluded zones" which 
# # I believe is one of the issues; may be able to override but not worth the exploration right now. 
# st_write(st_centroid(eg_lake)%>% dplyr::select(1:3, Shape), "data/output/test_lake.shp")
# eg_lake_centr <- st_centroid(eg_lake)%>% dplyr::select(1:3, Shape)%>%
#   tidyr::extract(Shape, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) 
# 
# ws1 <- lapply(1: nrow(eg_lake_centr),function(x){
#   df <- eg_lake_centr[x,]
#   ws <- delineateWatershed(xlocation = df$lon, ylocation = df$lat, crs = 4326,
#                           includeparameters = "true")
#   chars_ws <- computeChars(workspaceID = ws$workspaceID, rcode = "MA")
#   return(t(chars_ws$parameters))
#   })
# 
# writeShapefile(watershed = ws1, layer = "layer_name", dir = "data/output", what = "boundary")

## trial 2 
## try to calc Qin based on Qout - direct precip 