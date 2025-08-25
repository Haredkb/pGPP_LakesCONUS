### Run I0 with default parameters

df <- tar_read(mod_in)


#ODE_huisman <- function(mod_in, HUC4){
  require(deSolve)
  require(dplyr)
  require(pbapply)

mod_in <- data.frame(
  par_in = rep(seq(from = 1, to = 500, by = 2),3),
  areasqm = rep(c(1000, 5000, 10000), each = 250),
  doc_in = 6,
  tp_in = 0.03,
  maxdepth_m_prediction_khazaei = NA,
  Flow_in_m3s = 8.498224e-06
) %>%
  mutate(maxdepth = areasqm^(0.16/2),
         Permanent_Identifier = row_number())

  #for percent output
  i = 0 
  total = nrow(mod_in)
  
  
  #ODE Run through columns 
  mod_output <- lapply( 1:nrow(mod_in), function(x, parameter_filepath = "3_DerivedData/KellyModel_reparameterized_2025-02-04.csv"){
    
    input_df <- mod_in[x, ]
    #Set up input variables 
    I0 <- input_df$par_in    #umol photons m-2 s-1; Incoming Light
    SA <- input_df$areasqm    #m2
    inputDOCin <- input_df$doc_in #10 # g/m3 #(not mg) DOC concentration total (not divided)
    zmax= ifelse(input_df$maxdepth < 0.1, input_df$maxdepth_m_prediction_khazaei, input_df$maxdepth) #there are some 0 in maxdepth, Khazaei is most comparable to Ganz
    Pin=  input_df$tp_in * 1e3 * 0.3261  #(L to m-3) and mg 0.3261 convert PO4 to PO4-P #Phosphorus concentration in mixed layer Pin=5 # mg P m-3 ### (0 - 0.0176; > 0.104 (saw 0.3) mg/L)  (0.005 0.150 g P m-3); 5-150 #### Kelly 2018	Similation just used ratio DOCs[j]/CPs[i]
    DOCin = input_df$doc_in
    
    #Read in all the parameterized values
    parameter_readin(parameter_filepath, perc_val = 50) #Median Parameterized Values
    
    Qin = input_df$Flow_in_m3s * 86400 # m3/ day  #watershed flux, removed daymet precip in calculated for growing season currently
    ####Light availability
    
    tryCatch( {zmix=10^(-0.515*log10(DOCin)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zmix>zmax){zmix=zmax}
    }, error = function(e) {zmix = 5})#"modal [max] depth for all size classes is  5 m" (Ganz 2023)
    
    #Volume only ratio needed for calculations, so the general shape equation is less important
    V_epi = ((4/3) * SA*zmix)/2 #m3 Epilim  Volume #uses total DOC input (not epi)
    V_lake = 	((4/3) * SA*zmax)/2 #m3  Volume input_df$lakevolume #((4/3) * SA * zmax)/2 ##Total Lake Volume
    V_epiRatio = V_epi/V_lake
    
    # A is what we are solving for
    A = 100 # initial values
    kD <- kDOC * DOCin + kA * A
    I_zm <- lightAtten(zmix, I0, kD)
    
    # water in should be mean precip * areal
    #
    times=1:10000
    
    #huis_run <-  lapply(1:nrow(df), function(i){
    parms=c(I0 = I0,
            SA=SA,# m2
            zmix = zmix,
            zmax= zmax,
            Pin= Pin * V_epiRatio,#min $P_mgL #Total that enters epi
            DOCin= DOCin * V_epiRatio,##Total that enters epi
            V_lake = V_lake,
            V_epi = V_epi,
            V_ratio = V_epiRatio,
            Qin = Qin * V_epiRatio,#Total that enters epi, as Qin is for dilution amount
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
            rec=rec)
    
    # starting state variables
    n<-c(A=100,P=Pin,DOC=DOCin)
    # simulate with ode
    run=ode(y=n,times=times,func=huisman,parms=parms)%>%
      dplyr::last()%>%
      cbind(., as.data.frame(t(parms)))%>%
      mutate(Permanent_Identifier = input_df$Permanent_Identifier)#add name
    
    
    
    #return(run[nrow(run)])
    #})%>% do.call("rbind",.)
    
  })%>% do.call("rbind",.)
  
  write.csv(mod_output, paste0("5_ModelOutputs/HuisIORun_testrange_", Sys.Date(),".csv"))
  
  library(magrittr)
  library(ggplot2)
  theme_set(theme_bw())
  
  mod_output %<>%
    mutate(
      GPP_mgCm2day_med = ifelse(A > 0.9, A, 0) #all values less than 0.9 are effectively 0. 
    )

  #PAR
  ggplot(  mod_output )+
    geom_point(aes(x = I0, GPP_mgCm2day_med, color = SA), alpha = 0.5)+
    scale_y_log10()+
    labs(x = "PAR input umol/s/m2/day")
