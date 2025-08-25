###############################################################
### Examining Effect of HRT Epi verus Whole Lake for pGPP model
###############################################################

source("R/global_HRT.R")

#########
# Example Lakes
#########

#Use HUC04 range to get an size distribution 
nhdplusTools_data_dir("C://Users/hared/Dropbox/ResearchProjects/_GeneralData_/NHDPlus")
list.files(nhdplusTools_data_dir()) #goal is to get to a point to loop through these 

#folder directory using HUC4 example
nhd_file <- file.path(nhdplusTools_data_dir(), "NHDPLUS_H_0202_HU4_GDB")
hr <- get_nhdplushr(nhd_file, layers = "NHDWaterbody") #c("NHDFlowline", "NHDWaterbody", "NHDArea", "NHDPlusBurnWaterbody"))

nhdhr_lake <- hr$NHDWaterbody %>%
  dplyr::filter(FCODE == 39004)%>%#Lake or Pond
  mutate(COMID = as.character(COMID))

####
## Spatial Join with NHD Plus (non HR data)
## This allows connection with LAGOS and LakeCat Datasets
bbox <- sf::st_as_sfc(st_bbox(st_as_sf(nhdhr_lake))) #trim get data to the HR dataset
nhdp_lake <- get_waterbodies(bbox) #NHDPlus DataSset 

lake_df <-  st_join(nhdhr_lake, nhdp_lake)%>%
  mutate(lakearea = ifelse((lakearea %in% 0) | (is.na(lakearea)), Shape_Area * 1E10, lakearea)) #in the data anything less than 0.1 ha is labelled as zero, this conversion allows smaller lakes to be calculated by the polygon size

# remove all variables except lake_df
rm(list=setdiff(ls(), "lake_df"))

#distribution of lake sizes within the dataset
lake_df %>%
  mutate(areasqkm = areasqkm * 1E6) %>%
  pivot_longer(names_to = "area_m2", cols = c("lakearea", "areasqkm"))%>%
ggplot(.)+ #plot desnity function for just lakes > 1 ha, versus all in HR LakePond dataset
  geom_density(aes(value, group = area_m2, color = area_m2))+
  scale_x_log10()

#Lake Volume 
data_path <- file.path("G:/My Drive/Projects/UNDERC/200_Data/LakeChar")

lake_net <- read.csv(file.path(data_path, "King_2021_nets_networkmetrics_medres.csv"))
lake_vol <- read.csv(file.path(data_path, "Ganz_2023_lakesalldata_compiled_predictions.csv"))
lake_link <- read.csv(file.path(data_path, "Smith_2021_lake_link.csv"))

#Lake Residence time based on Brooks et al. 2017 L&O figure6B
t = read.csv("G:/My Drive/Projects/UNDERC/200_Data/Brooks_2017_LO_Fig6B.csv")%>%
  dplyr::rename("cat_depth" = 2) #category depth


#still missing a bunch - think it must be the <0.1 ha lakes. using this now and will come up with ways to fix
lake_df <- dplyr::select(lake_net, c("lagoslakeid", "nhdplusv2_comid")) %>%
                        right_join(., lake_df, by = c( "nhdplusv2_comid" = "comid" )) %>%
  dplyr::rename(COMID_hr = COMID)%>%
  dplyr::relocate(COMID_hr)%>%
  left_join(lake_vol)%>%
  # add Brooks ~ residence time based on max depth
  mutate(#categorize depth based on ranges in Brooks 2017 L&O
    cat_depth = ifelse(best_maxdepth < 2, 2,
                       ifelse(best_maxdepth < 3, 3,
                              ifelse(best_maxdepth < 5, 5, 
                                     ifelse(best_maxdepth < 10, 10, 
                                            ifelse(best_maxdepth < 20, 20, 99999)))))
  )%>%
  left_join(., t)

# remove all variables except lake_df
rm(list=setdiff(ls(), "lake_df"))  

##################################################################
## PLOTS for input lake shape data
##################################################################

#plots all lakes we have area data for
lake_df %>%
  mutate(areasqm = ifelse(is.na(areasqkm), Shape_Area, areasqkm * 1E6),
         ) %>%
  pivot_longer(names_to = "area_m2", cols = c("lakearea", "areasqm")) %>%
  ggplot(.)+ #plot desnity function for just lakes > 1 ha, versus all in HR LakePond dataset
  geom_density(aes(value, group = area_m2, color = area_m2))+
  scale_x_log10()

#Cone Volume - Only lakes > 1 ha, which seems fine. 
lake_df %>%
  mutate(areasqkm = areasqkm * 1E6) %>%
  pivot_longer(names_to = "area_m2", cols = c("lakearea", "areasqkm")) %>%
  ggplot(.)+ #plot desnity function for just lakes > 1 ha, versus all in HR LakePond dataset
  geom_density(aes(lakevolume, group = area_m2, color = area_m2))+
  scale_x_log10()

#Cone Volume (Ganz 2023)  vs. Surface Area
lake_df %>%
  ggplot(.)+ #plot desnity function for just lakes > 0.1 ha, versus all in HR LakePond dataset
  geom_point(aes(x = lakearea, y =lakevolume))+
  scale_x_log10()+
  scale_y_log10()


saveRDS(lake_df, "data/output/NHD0202_lakedf.RDS")
##################################################################
## pGPP model
##################################################################

###################
## Kelly et al. "Equilibrium areal lake GPP estimates were the product of equilibrium phytoplankton growth rate, phytoplankton
#volumetric density, and mixed-layer depth (rAzmix).
## Static inputs - determine how to use ranges from Olesky Paper - using maximum likelihood initially

####Input Bounds to consider
# PAR_GS <- raster("G:/My Drive/PAR_growingseason_2021_v5-0000000000-0000000000.tif")
# raster::mean(PAR_GS)
# raster::quantile(PAR_GS)

# PAR = 76.75465, 148.171  ##(min, max) * 4.57) #convert W/m2 to umol/s/m2 *reminder W is J/s*


# P_in = 5-150

df <- drop_na(lake_df, cone_volume)%>% #will drop any non lagos lakes, could use the relationship with lake area to approx. 
      dplyr::filter(meandepth > 0)%>%
  head(10)

df$PAR = 76.75 * 4.57 #stable for now. 

mod_output <- apply(df, 1, function(input_df){
  I0 <- PAR #input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
  SA <- input_df$lakearea
  kDOC <-0.0116/1000 #Olesky 2024 #0.00042 #m2mgC-1 Kelly 2018 Light attenuation coefficient of DOC
  kA <- 0.000563/1000 #Olesky 2024 #0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
  zmax= input_df$best_maxdepth
  lA= 0.316 #0.1  #day-1 Loss rate of phytoplankton
  pA=4.01 #1.2  #day-1 Maximum production rate of phytoplankton
  hA=8.07 #55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
  mA=2.08 #2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
  decay=0.00097 #0.001 #day-1 #DOC mineralization rate
  Pin= 10 #input_df$P_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
  # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P
  cA= 0.0489 #0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
  v= 0.000478#0.05  #m day-1 Sinking rate of phytoplankton
  rec=0.991 #Efficiency of phosphorus recycling from lost phytoplankton
  
  #(Perez-Fuentetaja and others 1999)
  zmix=10^(-0.515*log10(DOCin)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991)
  if(zmix>zmax){zmix=zmax}
  
  #Volume 
  V_epi = ((4/3) * SA*zmix)/2 #m3 Epilim  Volume
  V_lake = input_df$lakevolume #((4/3) * SA * zmax)/2 ##Total Lake Volume
  #?? they used max 20 to calculate lim 
  #Qin=input_df$flow_GS_inlet/185 #calculate flow per day 
  #Qin = 1243 * 1E3 #ML to m3, this was just a random choice from the input data
  HRT_med = input_df$t_median_yr *365 #day -1
  Qin = V_lake/HRT_med
  # water in should be mean precip * areal 
  # 
  times=1:1000
  
  
  #scenarios with variable DOC in
sc_DOC <- lapply(c(1, 5, 10, 20), function(DOCin){
  parms=c(I0 = I0,
          SA=SA,
          zmix = zmix,
          zmax=zmax, #df$best_maxdepth[i],
          Pin= Pin,#min $P_mgL
          DOCin= DOCin,#
          V_lake = input_df$lakevolume, #((4/3) * SA * zmax)/2,#area of ellipsoid/2(with ab pi replace with SA) #SA *1e6 * (zmax/3), #df$cone_volume[i],
          V_cone = input_df$cone_volume,
          V_epi = ((4/3) * SA*zmix)/2, #m3 Epilim  Volume - hare added ellipsoid as often epi volume was larger than total volume
          #Qin = Qin,  
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
          rec=rec,
          HRT = HRT_med,#day-1)
          Qin = Qin
          ) 
  
  # starting state variables
  n<-c(A=100,P=Pin,DOC=DOCin)
  # simulate with ode
  run_epi=ode(y=n,times=times,func=huisman_HRT,parms=parms, vol = "spilt")%>%
    dplyr::last()%>%
    cbind(., as.data.frame(t(parms)))%>%
    mutate(HRT_calc = "spilt_V")
  
  n<-c(A=100,P=Pin,DOC=DOCin) #, Qin = V_lake/HRT
  run_whole=ode(y=n,times=times,func=huisman_HRT,parms=parms, vol = "epi")%>%
    dplyr::last()%>%
    cbind(., as.data.frame(t(parms)))%>%
    mutate(HRT_calc = "epi_V")
  
  
  
  
  run = rbind(run_epi, run_whole)%>%
    mutate(V_ratio = V_epi/V_lake,
           COMID_hr = as.factor(input_df$COMID_hr))}
  )%>% do.call("rbind",.)#bind 5 DOC scenarios
  
})%>% do.call("rbind",.) %>%
  mutate(zmix_f =10^(-0.515*log10(DOC)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991)) #output final zmix

#%>%
 # mutate(onNet = as.factor(ifelse(onoffnet == 0, "offNetwork", "onNetwork"))) #Qin == 0



saveRDS(mod_output, "HRTsc_DOCsc_p20_20240226.RDS")


#####################################
## PLOTS ############################
#####################################
library(viridis)
library(ggpubr)

mod_output %>%
  dplyr::select(COMID_hr, DOCin, Qin, V_lake, HRT_calc, A)%>%
  pivot_wider(names_from = HRT_calc, values_from = A)%>%
  ggplot(.)+
  geom_point(aes(x = epi_V, y = spilt_V, color = V_lake))+
  geom_abline(intercept = 0, slope = 1)+
  labs(x = "A based on epiV", y = "A based on spilt_V")

ggsave("plots/HRT_output/Vlake_spiltVepi_modcompare.png")

mod_output %>%
  ggplot(.)+
  geom_point(aes(x = zmax, y = A, color = HRT_calc))

mod_output %>%
  dplyr::filter(A != max(A))%>%
  ggplot(.)+
  geom_point(aes(x = zmix, y = A, color = HRT_calc))

mod_output %>%
  dplyr::filter(A != max(A))%>%
  ggplot(.)+
  geom_point(aes(x = V_lake, y = A, color = HRT_calc))+
  scale_x_log10()

mod_output %>%
  ggplot(.)+
  geom_point(aes(x = P, y = A, color = log(DOC),  shape = HRT_calc))

mod_output %>%
  dplyr::filter(A != max(A))%>%
  ggplot(.)+
  geom_point(aes(x = zmix_f, y = A, color = log(DOC),  shape = HRT_calc))



mod_output %>%
  ggplot(.)+
  geom_point(aes(x = V_lake/HRT, y = A, color = HRT_calc))+
  scale_x_log10()

mod_output %>%
  ggplot(.)+
  geom_point(aes(x = V_epi/HRT, y = A, color = HRT_calc))+
  scale_x_log10()


mod_output %>%
  dplyr::filter(A != max(A))%>%
  ggplot(.)+
  geom_point(aes(x = V_ratio, y = A, color = HRT_calc))+
  scale_x_log10()


mod_output %>%
  dplyr::filter(A != max(A))%>%
  ggplot(.)+
  geom_point(aes(x = HRT/365, y = A, color = HRT_calc))+
  scale_x_log10()

mod_output1  <- mod_output %>%
  mutate(HRT_f = as.factor(HRT),
         HRT_yr = HRT/365) 

ggboxplot(mod_output1, x = "HRT_yr", y = "A", fill = "HRT_calc", group = interaction("HRT_f", "HRT_calc"))+
    geom_pwc(
      aes(group = HRT_calc), tip.length = 0,
      method = "t_test", label = "{p.adj.format}{p.adj.signif}",
      p.adjust.method = "bonferroni", p.adjust.by = "panel",
      hide.ns = TRUE
    )+
  scale_fill_viridis_d()+
  facet_wrap(~DOCin, scales = "free")

  ggsave("plots/HRT_output/Vlake_Qin_spiltVepi.png")
  
  
  ggboxplot(mod_output1, x = "DOCin", y = "A", fill = "HRT_calc", group = interaction("DOCin", "HRT_calc"))+
    # geom_pwc(
    #   aes(group = DOCin), tip.length = 0,
    #   method = "t_test", label = "{p.adj.format}{p.adj.signif}",
    #   p.adjust.method = "bonferroni", p.adjust.by = "panel",
    #   hide.ns = TRUE
    # )+
    scale_fill_viridis_d()
  
  ggsave("plots/HRT_output/Vlake_Qin_spiltVepi_DOCx.png")
  
