###########################
## GPP Model Original Helper Functions
###########################
# function for simulationg lake primary productivity as a function of DOC and P supply, lake surface area, and residence time
huisman<-function(t,y,params){
  with(as.list(c(y,params)),{
    
    # using Morris paper relationship, but symbols from Huisman
    kD=kA*A+kDOC*DOC	-0.05	#m-1; based on Morris paper
    
    
    
    # from a published paper 
    tryCatch( {zmix=10^(-0.515*log10(DOC)+0.115*log10(2*sqrt(SA/pi))+0.991)
    if(zmix>zmax){zmix=zmax}
    }, error = function(e) {zmix = 5})#"modal [max] depth for all size classes is  5 m" (Ganz 2023)
    
    
    Izmix=lightAtten(z=zmix,I0=I0,kD=kD)
    
    # biomass specific growth integrated across mixed layer
    prod=(pA/(kD*zmix))*log((hA+I0)/(hA+Izmix))*(P/(P+mA))	# d-1
    
    dA.dt=A*prod-lA*A-v/zmix*A-Qin/(zmix*SA)*A	# mg C m-3
    dP.dt=Qin/(zmix*SA)*(Pin-P)+cA*lA*A*rec-cA*A*prod		# mg P m-3 (in epi)
    dDOC.dt=(Qin/(zmix*SA))*(DOCin-DOC)-decay*DOC				# g C m-3 (in epi)
    
    return(list(c(dA.dt,dP.dt,dDOC.dt)))
  })
}
