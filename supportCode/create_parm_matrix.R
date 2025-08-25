###################################
### Basic Sensivity ###############
library(dplyr)
library(ggplot2)
library(deSolve)
source("R/global_HRT.R")



#Oleksy Parameter Ranges 
parms_df <- readRDS("C:/Users/hared/Dropbox/ResearchProjects/006_LakeGPP_CONUS/400_Analysis/GPP_CONUS_Lakes/parms_df_Olesky.RDS")
row_n <- seq(1, nrow(parms_df))
col_n <- seq(3, 14)

#Create parameter matrix for each run, changing parameter indepedantly. 
parms_l <- lapply(col_n, function(col){
    
    #make a dataframe with three rows all reps of initial input
    base_parm <-  parms_df[1,]%>% slice(rep(1, each = 3))
    c_name <- names(base_parm[col])
    base_parm$parms[1] <- paste0(c_name, "_med")
    #change the second row and third to be the range specified
    base_parm[2, col] <- parms_df[2, col]
    base_parm$parms[2] <- paste0(c_name, "_high")
    base_parm[3, col] <- parms_df[3, col]
    base_parm$parms[3] <- paste0(c_name, "_low")
    
    return(base_parm)
    
  }
)%>% do.call("rbind",.)%>%
  dplyr::distinct(., .keep_all = TRUE)%>%
  #add two base runs using the Oleksy median for all the parameters
  rbind(., parms_df[1,]%>% slice(rep(1, each = 2)))%>%
  mutate(mod_vol = "spilt")

#change last mod_vol to epi to get the non-spilt run
parms_l$mod_vol[nrow(parms_l)] <- "epi"


output <- lapply(seq(1,nrow(parms_l)), function(x){
    
  parms = parms_l[x,]
  
  sc_DOC <- lapply(c(1, 10, 20), function(DOCin){
  
  #defined seperately for each doc input
  parms$zmix=10^(-0.515*log10(DOCin)+0.115*log10(2*sqrt((parms$SA/1E6)/pi))+0.991)#not constained to zmax within the input
  parms$V_epi_in = parms$V_lake * (parms$zmix/parms$zmax) #((4/3) * SA*zmix)/2 #m3 Epilim  Volume
  parms$DOCin = DOCin
  
  
  times=1:5000
  n<-c(A=100,P=parms$Pin,DOC=parms$DOCin)
  out_epi=ode(y=n,times=times,func=huisman_HRT,parms=parms, vol = parms_l$mod_vol)%>%
    dplyr::last()%>%
    cbind(., parms)%>%
    mutate(zmix_calc = 10^(-0.515*log10(DOC)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991),
           zmix_f = ifelse(zmix_calc > zmax, zmax, zmix_calc),
           V_epi = V_lake * (zmix_f/zmax), #ifelse(((4/3) * SA*zmix_f)/2 > V_lake, V_lake, ((4/3) * SA*zmix_f)/2),
           V_ratio = V_epi/V_lake,
           DOCin = DOCin
    )
  
}) %>% #end docin range scenarios
  do.call( "rbind", .)

})%>% #end parmeter range test 
  do.call( "rbind", .)%>%
  mutate(parm_name = str_extract(parms, "[^_]+"),
         CI_level = str_extract(parms, "[_^]+"))

output$A_epi <- rep(output$A[112: 114], nrow(output)/3) #add a column with the A of the epi run. 
output$resid <- output$A_epi - output$A

#make a column with the CI of each paramter used
CI_level <- data.frame(do.call("rbind", strsplit(output$parms, "_")))
output <- output %>%
  mutate(CI_level = CI_level[,2])

########################################
## Plots
ggplot(output)+
  geom_boxplot(aes(y = A, x = as.factor(parm_name), color = mod_vol)) +
  facet_wrap(~DOCin)+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

ggsave("lake1_Oleksy_parameter_range.png", width = 7, height = 5, units = "in")

output %>%
  dplyr::filter(DOCin > 2)%>%
  ggplot(.)+
  geom_point(aes(y = A, x = as.factor(parm_name), shape = mod_vol, fill = CI_level), shape = 21)+
  geom_hline(data = dplyr::filter(output, parm_name == "Oleksy" & DOCin > 2), aes(yintercept = A, color = mod_vol))+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))+
  facet_wrap(~ DOCin)

ggsave("plots/HRT_output/lake1_Oleksy_paramrange_Vol.png", width = 7, height = 5, units = "in")




ggplot(output)+
  geom_point(aes(y = resid, x = as.factor(parm_name), shape = mod_vol, color = V_ratio))+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

ggplot(output)+
  geom_col(aes(y = resid, x = as.factor(parm_name), fill = DOCin), position = "dodge2")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))+
  geom_hline(yintercept = 14.35)

ggsave("lake1_Oleksy_parameter_range_points_resid.png")

