#########################
## Example Lake GPP CONUS Analysis
## Few Lake Exploration 
## 2024-01-23
## D.Hare
#########################
source('R/helper_func_og.R')
source('R/functions_tar_pGPP.R')
library(terra)
library(tidyverse)

lake_df <- readRDS("data/eg_lake_0202.RDS")
lake_sf <- st_sf(lake_df) #from NHD_data

df <- drop_na(lake_sf, lakevolume)%>% #will drop any non lagos lakes, could use the relationship with lake area to approx. 
  dplyr::filter(meandepth > 0)#%>%
  #head(., 10) #trim for dedugging 

#centroid of lake dataframe
lake_centr <-st_centroid(st_make_valid(df))#%>% dplyr::select(lake_nhdID, Shape)

df$tp_mgL <- tp_input(lake_centr)
df$par_GS_umol <-  par_input(df)
df$doc_mgL <- doc_input(df)

#have to use centroid data, polygon data will "work" but extracts all points in shape column. 
#daymet_output <- daymet_lake(lake_centr)

#Lake Residence time based on Brooks et al. 2017 L&O figure6B
t = read.csv("G:/My Drive/Projects/UNDERC/200_Data/Brooks_2017_LO_Fig6B.csv")%>%
  dplyr::rename("cat_depth" = 2) #category depth

df$SA = df$AreaSqKM * 1e6 #
df$zmix = 10^(-0.515*log10(df$doc_mgL)+0.115*log10(2*sqrt(df$SA/pi))+0.991)

df <- df %>%
  dplyr::filter(SA != 0)

rm(list=setdiff(ls(), "df"))
source('R/helper_func_og.R')
source('R/functions_tar_pGPP.R')
###################
## Kelly et al. "Equilibrium areal lake GPP estimates were the product of equilibrium phytoplankton growth rate, phytoplankton
#volumetric density, and mixed-layer depth (rAzmix).
## Static inputs - determine how to use ranges from Olesky Paper
mod_output <- apply(df, 1, function(input_df, parameter_filepath){
  
  
                      I0 <- input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
                      SA <- input_df$AreaSqKM * 1e6 #m2
                      DOC <- input_df$doc_mgL #10 # g/m3 #(not mg) DOC concentration in mixed layer
                      DOCin= input_df$doc_mgL #10 #DOC load concentration, make two so original values can be compared, remove later?
                      zmax= input_df$maxdepth
                      Pin= input_df$tp_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
                      
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
                      # cA=0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
                      # v=0.05  #m day-1 Sinking rate of phytoplankton
                      # rec=0.99 #Efficiency of phosphorus recycling from lost phytoplankton

            #(Perez-Fuentetaja and others 1999)
            # 
            # #categorize depth based on ranges in Brooks 2017 L&O, noted here we are using res time based on epi depth Brooks uses max lake
            # res_t = ifelse(zmix < 2, 0.3,
            #                      ifelse(zmix < 3, 0.3,
            #                             ifelse(zmix < 5, 0.4, 
            #                                    ifelse(zmix < 10, 0.9, 
            #                                           ifelse(zmix < 20, 1.6, 1.6)))))
            
            #?? they used max 20 to calculate lim 
            #Qin=input_df$flow_GS_inlet/365/1000 #185/1000 #calculate flow in m3 per day 
            #Qin_precip = input_df$prcp_in_GS/1000 #m3 per
            Qins = input_df$IN_GS_ML * 1000 / 185 # m3/ day  #QAMA and daymet precip in calculated for growing season
            ####Light availability 
            
            tryCatch( { zmix = 10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
            if(zmix>zmax){zmix==zmax}
            }, error = function(e) {zmix = 10})
            
            # A is what we are solving for 
            A = 100 # initial values 
            kD <- kDOC * DOC + kA * A
            I_zm <- lightAtten(zmix, I0, kD)


        # water in should be mean precip * areal 
        # 
        times=1:5000
          
        #huis_run <-  lapply(1:nrow(df), function(i){
              parms=c(I0 = I0,
                      SA=SA,
                      zmix = zmix,
                      zmax= df$best_maxdepth[i],
                      Pin= Pin,#min $P_mgL
                      DOCin= DOCin,#
                      V_lake = df$cone_volume[i],
                      Qins = Qins,  
                      #parameters 
                      kDOC=0.42,
                      kA=0.00022,
                      lA=0.1,
                      pA=1.2,
                      hA=55,
                      mA=2,
                      decay=0.001,
                      cA=0.015,
                      v=0.05,
                      rec=0.99)
              
              # starting state variables
              n<-c(A=100,P=Pin,DOC=DOCin)
              # simulate with ode
              run=ode(y=n,times=times,func=huisman,parms=parms)%>%
                dplyr::last()%>%
                cbind(., as.data.frame(t(parms)))
              
              #return(run[nrow(run)])
        #})%>% do.call("rbind",.)

})%>% do.call("rbind",.)
#   #cbind(df)%>%
# 
# mod_output <- mod_output %>%
#   mutate(onNet = as.factor(ifelse(num_inlet == 0, "offNetwork", "onNetwork")))


# add residence time as a column 
# mod_output$zmix=10^(-0.515*log10(mod_output$DOC)+0.115*log10(2*sqrt((mod_output$SA/1E6)/pi))+0.991)
# mod_output$V_epi = mod_output$V_lake * (mod_output$zmix/mod_output$zmax)
# mod_output$Qin.dt = mod_output$Qin * (mod_output$V_epi/mod_output$V_lake)
# mod_output$resTime = mod_output$V_epi/mod_output$Qin.dt

mod_output %>%
#  dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
ggplot()+
  geom_point(aes(x = P, y = A))#+
  # scale_x_log10()+
  # scale_y_log10()+
  #facet_grid(~onNet)


mod_output %>%
  #  dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = Qins, y = A))#+
# scale_x_log10()+
# scale_y_log10()+
#facet_grid(~onNet)



mod_output %>%
  #  dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = V_lake, y = A))

mod_output %>%
#  dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = DOC, y = A))#+
  # scale_x_log10()+
  # scale_y_log10()+
  #facet_grid(~onNet)

mod_output %>%
 # dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = resTime, y = A))+
   scale_x_log10()+
   scale_y_log10()+
  facet_grid(~onNet)

# mod_output %>%
#   # dplyr::filter(Pin > 1)%>% #filter out very low values, need to address
#   ggplot()+
#   geom_point(aes(x = res_t, y = A))+
#   scale_x_log10()+
#   scale_y_log10()+
#   facet_grid(~onNet)
# 

mod_output$res_t =mod_output$V_lake/ mod_output$Qins

mod_output %>%
  #dplyr::filter(Pin > 2)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = SA, y = A, color = Qins))+ #have to filter out off-network, as in Qin streamflow = 0 
  scale_x_log10()+
  scale_y_log10()#+
 # facet_grid(~onNet)


mod_output %>%
  #dplyr::filter(Pin > 2)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = res_t, y = A, color = Qin))+ #have to filter out off-network, as in Qin streamflow = 0 
  
  scale_y_log10()#+
# facet_grid(~onNet)

mod_output %>%
#  dplyr::filter(Pin > 2)%>% #filter out very low values, need to address
  ggplot()+
  geom_point(aes(x = I0, y = A, color = P)) #have to filter out off-network, as in Qin streamflow = 0 

library(kableExtra)
kbl(mod_output,    digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  # pack_rows("Watershed 1", 1, 4) %>%
  # pack_rows("Watershed 5", 5, 8) %>%
  # pack_rows("Watershed 6", 9, 12)
  save_kable(file ="Table_ModOutput_Test.png")


#create table 
tbl_df <- st_drop_geometry(head(mod_output, n = 15))%>%
  dplyr::select(lake_nhdID, IN_GS_ML,IN_pct_sw, OUT_GS_ML, NET_GS_ML, sqkm_inlet, sqkm_outlet, SA, par_GS_umol, tavg_GS_avg_C, num_inlet, num_outlet, )

library(kableExtra)
library(webshot2)
kbl(tbl_df, escape = F, caption="Table 1: Growing Season Hydrologic Fluxes and Meterological Inputs (ML = Megaliter; GS = Growing Season (185 days))",
    digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") #%>%
  # pack_rows("Watershed 1", 1, 4) %>%
  # pack_rows("Watershed 5", 5, 8) %>%
  # pack_rows("Watershed 6", 9, 12)
  save_kable(file ="Table_HydrologicFluxes.png")
