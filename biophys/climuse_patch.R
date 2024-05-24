library(tidyverse)
library(TrenchR)


#library(rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
## ^use this or set wd to source file location (which should be in biophys)

climateuse <- readRDS("climateuse.rds")

#Getting surface roughness estimate from C1 2022
C1wsprofile <- read_csv("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022/C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf_r <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))

#set z, right now it's 5cm off the ground
climateuse <- climateuse %>% mutate(T_lowuse=air_temp_profile(T_r=T_1.00use, u_r=.5, zr=2.5, z0=surf_r, z=.05, T_s=T_soilest)) 

#now climateuse is free to use!


ggplot(climateuse, aes(x=dt_noyr, y=T_lowuse)) + 
  geom_line(color="blue") +
  facet_grid(site~year(dtuse)) + ggtitle("Closer-to-ground air temps")
