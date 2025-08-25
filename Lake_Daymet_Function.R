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
    )%>%
    dplyr::select(-X, -Y)
  
  #   write.csv(centr, "")
  # # May 1 is the 121st day of the year (122nd in leap years)
  # # October 31 is the 304th day of the year (305th in leap years) 
  #   
  #   ## added for ice-on determination 
  #   met_lake <- download_daymet_batch(centr)
  
  # Burham and Shaman 2023
  # y = 3.3865ln(x) + 7.8032R² = 0.2623 (y= delay days, x = area)
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
      dplyr::filter(year == 2021 & yday > 243 | year == 2022 & yday <244)%>% #september 1 to make sur all freezing days are included. 
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