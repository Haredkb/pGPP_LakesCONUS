###########################
## Basic Sensivity analysis 
###########################
input_df <- df[1,]

parms_df <- data.frame(
  #input values
  I0 = input_df$PAR, #input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
  SA = input_df$lakearea,
  kDOC = 0.0116,   #0.0116/1000 #Olesky 2024 #0.00042 #m2 mgC-1 Kelly 2018 Light attenuation coefficient of DOC
  #due to water alone can be  0.038 m-1,
  #morris et al. 1995
  # 0.22 * [DOC in g/m3] +0.07 [Chl a g/m3] -0.05
  kA = 0.000563, #0.07/75/1000 #0.000563/1000 #Olesky 2024 #0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
  zmax= input_df$best_maxdepth,
  lA= 0.316, #0.1  #day-1 Loss rate of phytoplankton
  pA=4.01, #1.2  #day-1 Maximum production rate of phytoplankton
  hA=55, #55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
  mA=2.08, #2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
  decay=0.00097, #0.001 #day-1 #DOC mineralization rate
  Pin= 10, #input_df$P_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
  # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P
  cA= 0.0489, #0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
  v= 0.000478,#0.05  #m day-1 Sinking rate of phytoplankton
  rec=0.991, #Efficiency of phosphorus recycling from lost phytoplankton
  #DOCin - as scenarios
  
  #Estimated Lake Volume from Ganz et al. 2023
  V_lake = input_df$lakevolume, #((4/3) * SA * zmax)/2 ##Total Lake Volume
  #?? they used max 20 to calculate lim 
  #Qin=input_df$flow_GS_inlet/185 #calculate flow per day 
  #Qin = 1243 * 1E3 #ML to m3, this was just a random choice from the input data
  HRT_med = input_df$t_median_yr *365, #day -1
  Qin = input_df$lakevolume/(input_df$t_median_yr *365),
  parms = "Oleksy_M"
)


parms_OleL <- data.frame(
  #input values
  I0 = input_df$PAR, #input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
  SA = input_df$lakearea,
  kDOC = 0.000142,   #0.0116/1000 #Olesky 2024 #0.00042 #m2 mgC-1 Kelly 2018 Light attenuation coefficient of DOC
  #due to water alone can be  0.038 m-1,
  #morris et al. 1995
  # 0.22 * [DOC in g/m3] +0.07 [Chl a g/m3] -0.05
  kA = 0.000112, #0.07/75/1000 #0.000563/1000 #Olesky 2024 #0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
  zmax= input_df$best_maxdepth,
  lA= 0.159, #0.1  #day-1 Loss rate of phytoplankton
  pA=0.487, #1.2  #day-1 Maximum production rate of phytoplankton
  hA=0.6, #55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
  mA=0.185, #2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
  decay=0.000288, #0.001 #day-1 #DOC mineralization rate
  Pin= 10, #input_df$P_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
  # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P
  cA= 0.0279, #0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
  v= 0.000111,#0.05  #m day-1 Sinking rate of phytoplankton
  rec=0.976, #Efficiency of phosphorus recycling from lost phytoplankton
  #DOCin - as scenarios
  
  #Estimated Lake Volume from Ganz et al. 2023
  V_lake = input_df$lakevolume, #((4/3) * SA * zmax)/2 ##Total Lake Volume
  #?? they used max 20 to calculate lim 
  #Qin=input_df$flow_GS_inlet/185 #calculate flow per day 
  #Qin = 1243 * 1E3 #ML to m3, this was just a random choice from the input data
  HRT_med = input_df$t_median_yr *365, #day -1
  Qin = input_df$lakevolume/(input_df$t_median_yr *365),
  parms = "Oleksy_L"
)


parms_OleH <- data.frame(
  #input values
  I0 = input_df$PAR, #input_df$par_GS_umol#umol photons m-2 s-1; Incoming Light
  SA = input_df$lakearea,
  kDOC = 1,   #0.0116/1000 #Olesky 2024 #0.00042 #m2 mgC-1 Kelly 2018 Light attenuation coefficient of DOC
  #due to water alone can be  0.038 m-1,
  #morris et al. 1995
  # 0.22 * [DOC in g/m3] +0.07 [Chl a g/m3] -0.05
  kA = 0.00676, #0.07/75/1000 #0.000563/1000 #Olesky 2024 #0.00022   #m2mgC-1 Kelly 2018 Light attenuation coefficient of pelagic phytoplankton
  zmax= input_df$best_maxdepth,
  lA= 0.576, #0.1  #day-1 Loss rate of phytoplankton
  pA=25.7, #1.2  #day-1 Maximum production rate of phytoplankton
  hA=224, #55   #umol photons m-2 s-1 Half-saturation constant for light-limited production of phytoplankton
  mA=19.8, #2    #mg P m-3 Half-saturation constant for nutrient-limited production of phytoplankton
  decay=0.00167, #0.001 #day-1 #DOC mineralization rate
  Pin= 10, #input_df$P_mgL * 1e3 * 0.3261 #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]		
  # how to convert TP in mg L to mgL as P? assume its all PO4? 1 µg/l PO4 = 30.973762 µg/l P[MW P]/94.971482 µg/l[MW PO4] = 0.326138 µg/l P
  cA= 0.0745, #0.015 #mg P mg C-1  #Phosphorus to carbon quota of phytoplankton
  v= 0.00374,#0.05  #m day-1 Sinking rate of phytoplankton
  rec=0.996, #Efficiency of phosphorus recycling from lost phytoplankton
  #DOCin - as scenarios
  
  #Estimated Lake Volume from Ganz et al. 2023
  V_lake = input_df$lakevolume, #((4/3) * SA * zmax)/2 ##Total Lake Volume
  #?? they used max 20 to calculate lim 
  #Qin=input_df$flow_GS_inlet/185 #calculate flow per day 
  #Qin = 1243 * 1E3 #ML to m3, this was just a random choice from the input data
  HRT_med = input_df$t_median_yr *365, #day -1
  Qin = input_df$lakevolume/(input_df$t_median_yr *365),
  parms = "Oleksy_H"
)

parms_df <- rbind(parms_df, parms_OleH, parms_OleL)

saveRDS(parms_df, "parms_df_Olesky.RDS")
