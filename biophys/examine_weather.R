library(tidyverse)
library(gsubfn)
library(lubridate)
library(TrenchR)
library(ggnewscale)
library(tibbletime)

#library(rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
## ^use this or set wd to source file location

climateuse<- readRDS("climateuse.rds")

ggplot(climateuse) + geom_density(aes(x=ws)) + 
  geom_density(aes(x=ws_src), color="red") +
  facet_grid(site~year(dtuse)) 

climateuse %>% group_by(site, year(dtuse)) %>% summarize(mean_ours=mean(ws, na.rm=TRUE), 
                                                         median_ours=median(ws, na.rm=TRUE),
                                                         mean_theirs=mean(ws_src, na.rm=TRUE),
                                                         median_theirs=median(ws_src, na.rm=TRUE))



#Getting surface roughness estimate from C1 2022
C1wsprofile <- read_csv("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022/C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf_r <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))


# #this is the code I used to generate ws_src for:
# ##Eldo (nrel)
# nrel <- nrel %>% mutate(ws=wind_speed_profile_neutral(u_r=WS_2.00, zr=2, z0=surf_r, z=1)) 
# 
# ##B1 (hydro)
# hydroS <- hydroS %>% mutate(ws=wind_speed_profile_neutral(u_r=WS_2.50, zr=2.5, z0=surf_r, z=1)) 
# 
# ##C1 (lter)
# #ws profile down to 1.7m and 1m (note: zr=7m is a guess)
# dlter <- dlter %>% mutate(ws_1.7=wind_speed_profile_neutral(u_r=wshigh, zr=7, z0=surf_r, z=1.7),
#                           ws=wind_speed_profile_neutral(u_r=wshigh, zr=7, z0=surf_r, z=1))

#could perhaps undo it (may be easier than working with my slow code)
El <- climateuse %>% filter(site=="Eldo")
El <- El %>% mutate(ws_src_og=wind_speed_profile_neutral(u_r=ws_src, zr=1, z0=surf_r, z=2))

B1 <- climateuse %>% filter(site=="B1")
B1 <- B1 %>% mutate(ws_src_og=wind_speed_profile_neutral(u_r=ws_src, zr=1, z0=surf_r, z=2.5))

C1 <- climateuse %>% filter(site=="C1")
C1 <- C1 %>% mutate(ws_src_og=wind_speed_profile_neutral(u_r=ws_src, zr=1, z0=surf_r, z=7)) #a guess


climateuse <- plyr::rbind.fill(climateuse %>% filter(site=="A1"), El, B1, C1)

ggplot(climateuse) + geom_density(aes(x=ws)) + 
  geom_density(aes(x=ws_src_og), color="red") +
  facet_grid(site~year(dtuse)) 

climateuse %>% group_by(site, year(dtuse)) %>% summarize(mean_ours=mean(ws, na.rm=TRUE), 
                                                         median_ours=median(ws, na.rm=TRUE),
                                                         mean_theirs=mean(ws_src_og, na.rm=TRUE),
                                                         median_theirs=median(ws_src_og, na.rm=TRUE))
