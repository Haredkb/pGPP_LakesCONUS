###

setwd("G:/Shared drives/Hare and Solomon CONUS GPP")
library(tidyverse)
theme_set(theme_bw(base_size = 8))

options(ggplot2.continous.colour="viridis")
options(ggplot2.continous.fill = "viridis")

scale_colour_discrete <- function(...) {
    scale_colour_manual(..., values = viridis::viridis(4, end = 0.85))
}
###

dt <- read.csv("5_ModelOutputs/HuisModelRun_0202_2025-04-18.csv")%>%
  mutate(HUC04 = "0202")%>%
  rbind(read.csv("5_ModelOutputs/HuisModelRun_0203_2025-04-26.csv")%>% mutate(HUC04 = "0203"))%>%
  rbind(read.csv("5_ModelOutputs/HuisModelRun_0401_2025-04-30.csv")%>% mutate(HUC04 = "0401"))%>%
  rbind(read.csv("5_ModelOutputs/HuisModelRun_1301_2025-05-01.csv")%>% mutate(HUC04 = "1301"))%>%
  dplyr::filter(DOC/P < 10)%>%
  mutate(
    GPP_mgCm2day_med = ifelse(A > 0.9, A, 0) #all values less than 0.9 are effectively 0. 
  )

###### Plots ###########

#DOC P Ratio
ggplot(dt)+
  geom_point(aes(DOC/P, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
scale_y_log10()

#PAR
ggplot(dt)+
  geom_point(aes(x = I0, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
    scale_y_log10()+
  labs(x = "PAR input umol/s/m2/day")


#P Concentration
ggplot(dt)+
  geom_point(aes(P, GPP_mgCm2day_med, color = HUC04), alpha = 0.5, size = 0.5)+
  scale_x_log10()+
  scale_y_log10()

#Surface Area
ggplot(dt)+
  geom_point(aes(x = SA, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Surface Area (m2)")

#Epi Volume
ggplot(dt)+
  geom_point(aes(x = V_epi, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
  scale_x_log10()+
scale_y_log10()

#Epi to Total Volume Ratio
ggplot(dt)+
  geom_point(aes(x = V_ratio, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)

#Compare Input P to Output P
ggplot(dt)+
  geom_point(aes(P, Pin, color = HUC04), alpha = 0.5)


ggplot(dt)+
  geom_point(aes(DOC, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()


ggplot(dt)+
  geom_point(aes(DOCin, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()


#Flow In
ggplot(dt)+
  geom_point(aes(Qin, GPP_mgCm2day_med, color = HUC04), alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()
