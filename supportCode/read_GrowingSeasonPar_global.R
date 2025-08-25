####################
## Download PAR for specfic lat/long
## pGPP model
####################
source('R/functions_tar_pGPP.R')
library(sf)
library(tidyverse)

#Original Run if output raster
# df <- st_as_sf(read.csv("G://My Drive/Projects/UNDERC/200_Data/Olesyk_LakeMetadata.csv"),
#                coords = c("Longitude..decimal.degrees.", "Latitude..decimal.degrees."))
# df$par_GS_umol <-  par_input(df)


PAR_df <- rbind(
  read.csv("G://My Drive/oleksy_pts_PAR_growingseason_NH/oleksy_pts_PAR_growingseason_NH.csv"),
  read.csv("G://My Drive/oleksy_pts_PAR_growingseason_SH/oleksy_pts_PAR_growingseason_SH.csv")) %>%
  dplyr::select(lakeID, year, hemisphere, mean)

PAR_df <- PAR_df %>%
  group_by(lakeID, hemisphere)%>%
  summarize(
    mean = mean(mean, na.rm = TRUE)
  ) %>%
  mutate(
    year = "avg",
  )%>%
  rbind(PAR_df, .)%>%
  mutate(PAR_siteyear = mean,
    par_GS_umol = PAR_siteyear * 4.57)

#just average
avg <- dplyr::filter(PAR_df, year == "avg") %>%
  dplyr::rename("avg_PAR" = PAR_siteyear)%>%
  dplyr::select(-year, -par_GS_umol, -mean) %>% #clean up dataframe for combining
  mutate(avg_par_GS_umol = avg_PAR * 4.57)

#add to the 
df <- read.csv("G://My Drive/Projects/UNDERC/200_Data/Olesyk_LakeMetadata.csv") %>%
  mutate(hemisphere = ifelse(Latitude..decimal.degrees. > 0, "North Hemisphere", "South Hemisphere"),
         year = ifelse( !is.na(year.included), year.included, "avg")) 

df <- left_join(df, PAR_df, by = c("lakeID", "hemisphere", "year"))%>%
  left_join(., avg, by = c("lakeID", "hemisphere")) # add column for avg PAR for 2010-2018

write.csv(df, "G://My Drive/Projects/UNDERC/200_Data/Olesyk_LakeMetadata_wPAR.csv")

ggplot(df)+
  geom_point(aes(abs(df$Latitude..decimal.degrees.), avg_par_GS_umol, color = hemisphere))+
  theme_bw()+
  labs(x = "Latitude as Absolute Value")

ggsave("G://My Drive/Projects/UNDERC/200_Data/Olesyk_LakeMetadata_wPAR.png")
