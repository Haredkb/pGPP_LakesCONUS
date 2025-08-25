##################
## Calculating DOC from TOC ( ) 
#################

#Values from Table 3
# Predictor Variables
intercept = -2.00
sqrt_wetlandp = 2.49
sqrt_cropp = 0.79
ln_pH = 1.77
ln_Q = -0.11

ln_TOC = intercept + sqrt_wetlandp*sqrt(0.6) + sqrt_cropp*(sqrt(0.02)) + ln_pH*log(6) # + ln_Q
 exp(ln_TOC) #mg/L

 
 #using eg_lake from NHD_data
 
eg_lake$TOC_mgL = exp(intercept + sqrt_wetlandp*sqrt(eg_lake$PCTHBWET2019CAT/100 + eg_lake$PCTWDWET2019CAT/100) + sqrt_cropp*(sqrt(eg_lake$PCTCROP2019CAT/100)) + ln_pH*log(6))

range(eg_lake$TOC_mgL)
