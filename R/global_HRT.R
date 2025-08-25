###################################
# Global Library for pGPP HRT evaluation 
###################################

#Libraries

##Input data 
library(nhdplusTools)
library(sf)
library(tidyverse)
library(magrittr)
theme_set(theme_bw(base_size = 9))
library(dataRetrieval)
sf_use_s2(FALSE) #makes intersect work


library(StreamCatTools) #for lake cat

####
# Model Runs
#########################
source('R/helper_func_og.R')
library(deSolve) #algal growth models
library(sf)
library(dplyr)
library(raster)
library(terra)



##############################
lightAtten<-function(z,I0,kD){
  Iz=I0*exp(-kD*z)
  return(Iz)
}

# function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman_HRT<-function(t,y,params, vol){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    kD = kA*A + kDOC*DOC -0.05	#m-1; based on Morris paper
    
    # from a published paper -> I'll refind the paper eventually
    zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt((SA/1E6)/pi))+0.991)
    if(zmix>zmax){zmix=zmax}
    
    V_epi = V_lake * (zmix/zmax) #((4/3) * SA*zmix)/2
    #if(V_epi>V_lake){V_epi=V_lake}
    
    if(vol == "spilt"){
      Qin.dt = Qin * (V_epi/V_lake) #Only a percent of Qin goes into the epilimnion, defined by how much of the total lake volume is V_epi 
      }else(
      Qin.dt = Qin #all the Qin goes into the mixed layer. 
    )
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-(Qin.dt/V_epi)*A	        # mg C m-3
    dP.dt=(Qin.dt/V_epi)*(Pin-P)+cA*lA*A*rec-cA*A*prod	# mg P m-3 (in epi)
    dDOC.dt=(Qin.dt/V_epi)*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}

