#########

######################################
## Evaluting Pelgaic GPP in lakes ###
######################################


## here volume of epilim is determined by the ratio of zmix to zmax, then applying to volume
#https://stat.ethz.ch/pipermail/r-help/2012-April/309308.html

# Libraries 
library(deSolve)
library(FME)
library(rgdal)
#################
# start with df from pGPP_ExamineHRTeffect #
lake_df <- readRDS("data/output/NHD0202_lakedf.RDS")
#get 10 examples 
df <- drop_na(lake_df, cone_volume)%>% #will drop any non lagos lakes, could use the relationship with lake area to approx. 
  dplyr::filter(meandepth > 0)%>%
  head(10)


rm(list=setdiff(ls(), "df"))  
source("R/global_HRT.R")
df$PAR = 76.75 * 4.57 #stable for now. 


mod_output <- apply(df, 1, function(input_df){
  #input values
                    I0 <- input_df$PAR #input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
                    SA <- input_df$lakearea
                    kDOC <- 0.0116      #0.0116/1000 #Olesky 2024 #0.00042 #m2 mgC-1 Kelly 2018 Light attenuation coefficient of DOC
                    #due to water alone can be  0.038 m-1,
                    #morris et al. 1995
                    # 0.22 * [DOC in g/m3] +0.07 [Chl a g/m3] -0.05
                    kA <- 0.000563 #0.07/75/1000 #0.000563/1000 #Olesky 2024 #0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
                    zmax= input_df$best_maxdepth
                    lA= 0.316 #0.1  #day-1 Loss rate of phytoplankton
                    pA=4.01 #1.2  #day-1 Maximum production rate of phytoplankton
                    hA=55 #55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
                    mA=2.08 #2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
                    decay=0.00097 #0.001 #day-1 #DOC mineralization rate
                    Pin= 10 #input_df$P_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
                    # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P
                    cA= 0.0489 #0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
                    v= 0.000478#0.05  #m day-1 Sinking rate of phytoplankton
                    rec=0.991 #Efficiency of phosphorus recycling from lost phytoplankton
                    #DOCin - as scenarios
                    

                    
                    #Estimated Lake Volume from Ganz et al. 2023
                    V_lake = input_df$lakevolume #((4/3) * SA * zmax)/2 ##Total Lake Volume
                    #?? they used max 20 to calculate lim 
                    #Qin=input_df$flow_GS_inlet/185 #calculate flow per day 
                    #Qin = 1243 * 1E3 #ML to m3, this was just a random choice from the input data
                    HRT_med = input_df$t_median_yr *365 #day -1
                    Qin = V_lake/HRT_med
######################################
# mod parameter test
                    # run four DOC scenerios for each lake
      sc_DOC <- lapply(c(1, 5, 10, 20), function(DOCin){
        
        #defined seperately for each doc input
        zmix=10^(-0.515*log10(DOCin)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991)#not constained to zmax within the input
        V_epi_in = V_lake * (zmix/zmax) #((4/3) * SA*zmix)/2 #m3 Epilim  Volume
        
              parms=c(I0 = I0,
                      SA=SA,
                      zmax=zmax, #df$best_maxdepth[i],
                      Pin= Pin,#min $P_mgL
                      #(Perez-Fuentetaja and others 1999)
                      DOCin = DOCin,
                      V_lake = V_lake, #((4/3) * SA * zmax)/2,#area of ellipsoid/2(with ab pi replace with SA) #SA *1e6 * (zmax/3), #df$cone_volume[i],
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

              times=1:5000
              n<-c(A=100,P=Pin,DOC=DOCin)
              out_epi=ode(y=n,times=times,func=huisman_HRT,parms=parms, vol = "epi")%>%
                dplyr::last()%>%
                cbind(., as.data.frame(t(parms)))%>%
                mutate(HRT_calc = "epi_V",
                       parm = "med")
              
              plower <- c(I0 = I0,
                          SA=SA,
                          #zmix = zmix,
                          zmax=zmax, #df$best_maxdepth[i],
                          Pin= Pin,#min $P_mgL
                          DOCin= DOCin,#
                          V_lake = V_lake, #((4/3) * SA * zmax)/2,#area of ellipsoid/2(with ab pi replace with SA) #SA *1e6 * (zmax/3), #df$cone_volume[i],
                          #Qin = Qin,  
                          #parameters 
                          kDOC=kDOC,
                          kA=kA,
                          lA=lA,
                          pA=pA,
                          hA=hA,
                          mA=mA,
                          decay=decay,
                          cA=0.0489,
                          v=v,
                          rec=0.976,
                          HRT = HRT_med,#day-1)
                          Qin = Qin
              ) 
              
              pupper <- c(I0 = I0,
                          SA=SA,
                          #zmix = zmix,
                          zmax=zmax, #df$best_maxdepth[i],
                          Pin= Pin,#min $P_mgL
                          DOCin= DOCin,#
                          V_lake = V_lake, #((4/3) * SA * zmax)/2,#area of ellipsoid/2(with ab pi replace with SA) #SA *1e6 * (zmax/3), #df$cone_volume[i],
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
                          rec=0.996,
                          HRT = HRT_med,#day-1)
                          Qin = Qin
              ) 
              
              
              out_spilt_m =ode(y=n,times=times,func=huisman_HRT,parms=parms, vol = "spilt")%>%
                dplyr::last()%>%
                cbind(., as.data.frame(t(parms)))%>%
                mutate(HRT_calc = "spilt_V",
                       parm = "med")
              
              out_spilt_l =ode(y=n,times=times,func=huisman_HRT,parms=plower, vol = "spilt")%>%
                dplyr::last()%>%
                cbind(., as.data.frame(t(plower)))%>%
                mutate(HRT_calc = "spilt_V",
                       parm = "lower")
              
              out_spilt_h =ode(y=n,times=times,func=huisman_HRT,parms=pupper, vol = "spilt")%>%
                dplyr::last()%>%
                cbind(., as.data.frame(t(pupper)))%>%
                mutate(HRT_calc = "spilt_V",
                       parm = "upper")
              
              out_l = rbind(out_epi, out_spilt_l, out_spilt_m, out_spilt_h)%>%
                mutate(COMID_hr = as.factor(input_df$COMID_hr),
                       zmix_calc = 10^(-0.515*log10(DOC)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991),
                       zmix_f = ifelse(zmix_calc > zmax, zmax, zmix_calc),
                       V_epi = V_lake * (zmix_f/zmax), #ifelse(((4/3) * SA*zmix_f)/2 > V_lake, V_lake, ((4/3) * SA*zmix_f)/2),
                       V_ratio = V_epi/V_lake,
                       DOCin = DOCin
                       )
              
              }) %>% #end docin range scenarios
                      do.call( "rbind", .)
})#end apply
                    

mod_output <- do.call("rbind", mod_output)
### make a plot

ggplot(mod_output)+
  geom_point(aes(x = V_epi, y = A, color = as.factor(kDOC), shape = HRT_calc))+
  facet_wrap(~DOCin)

mod_output2 <- mod_output %>%
  #dplyr::filter(parm == "med")%>% #know med performs well, but well enough...? 
  dplyr::select(COMID_hr, DOCin, Qin, V_lake, HRT_calc, A, parm)%>%
  pivot_wider(names_from = c(HRT_calc, parm), values_from = A)%>%
  mutate(resid_l = spilt_V_lower - epi_V_med,
         resid_h = spilt_V_upper - epi_V_med,
         resid_0 = spilt_V_med - epi_V_med) %>%
  pivot_longer(9:11, names_to = "parm1", values_to = "resid")%>%
  mutate(HRT_calc = "spilt_V",
         parm = if_else(str_detect(parm1, "_h"), "upper", if_else(str_detect(parm1, "_l"), "lower", "med")))%>%
  left_join(mod_output, .)



         # improve_l = resid_l - resid_0,
         # improve_h = resid_h - resid_0)
#saveRDS(mod_output2, "HRTsc_DOCsc_ca_0304range.RDS")

saveRDS(mod_output2, "HRTsc_DOCsc_ca_0304range_update_ha55.RDS")

ggplot(mod_output2)+
  geom_point(aes(x = V_ratio, y = resid, color = COMID_hr, shape = interaction(as.factor(rec), parm)))+
  geom_hline(yintercept = 0)

ggsave("plots/HRT_output/Vlake_spiltVepi_mod_rec.png", width = 5, height = 6, units = "in")
#   
# ### showing strong sensitive to cA range 0.03 : 0.0489
kDOC_eval <- mod_output2 %>%
  na.omit()%>%#removes epi values
  group_by(COMID_hr, DOCin)%>%
  summarize(min_resid = which.min(abs(resid)))%>%
  left_join(., mod_output2, by = c("COMID_hr", "DOCin", "min_resid" = "resid"))
  #filter(resid == min(abs(resid)))


ggplot(mod_output2)+
  #geom_segment(aes(x = spilt_V_med, xend = epi_V_med + resid, y = V_ratio, yend = V_ratio))+

  geom_point(aes(x = epi_V_med, y = V_ratio), fill = "orange", size=3, shape = 21) +
  geom_point(aes(x = spilt_V_med, y = V_ratio), color = "blue", size=2, shape = 3) +
  geom_point(aes(x = epi_V_med + resid, y = V_ratio, color = kDOC), size=2) +
    labs(x = "A based on epiV", y = "A based on spilt_V")+
  facet_wrap(~DOCin)

  mod_output3 <- mod_output2 %>%
    dplyr::select(COMID_hr, V_lake, epi_V_med, spilt_V_med, spilt_V_upper)
  
  
#clean to just compare median parameters
mod_output1 <- mod_output %>%
  dplyr::filter(parm == "med")%>%
  dplyr::select(COMID_hr, DOCin, Qin,V_lake, HRT_calc, A)%>%
  pivot_wider(names_from = HRT_calc, values_from = A)%>%
  mutate(resid = spilt_V - epi_V)%>%
  left_join(., dplyr::filter(mod_output, parm == "med" & HRT_calc == "spilt_V"))

ggplot(mod_output1)+
  geom_point(aes(x = V_ratio, y = resid))

ggplot(mod_output1)+
  geom_point(aes(x = zmix_calc, y = resid, color = DOC))
  



