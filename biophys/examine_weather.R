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

wind_speed_profile_neutral(u_r=.5, zr=1, z0=surf_r, z=.1)


#time for looking at soil
El23 <- El %>% filter(year(dtuse)==2023)
mod <- lm(T_soil~T_soilest, El23)
summary(mod)

ggplot(El23, aes(x=T_soilest, y=T_soil)) + geom_point(alpha=.3) + geom_smooth(method="lm", color="green") + 
  geom_abline(intercept=0, slope=1, color="purple") 

mean(abs(El23$T_soil-El23$T_soilest))
mean(abs(El23$T_soil-(El23$T_soilest*mod$coefficients[2]+mod$coefficients[1])))

ggplot(El23 %>% filter(dtuse>"2023-07-20 19:31:08 MST" & dtuse<"2023-07-25 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue") + theme_bw() + ggtitle("2023 Eldo soil zoom Jul")

#interesting... what disconnect is happening here
ggplot(El23 %>% filter(dtuse>"2023-08-13 19:31:08 MST" & dtuse<"2023-08-18 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue") + theme_bw() + ggtitle("2023 Eldo soil zoom Aug")

B123 <- B1 %>% filter(year(dtuse)==2023)
mod <- lm(T_soil~T_soilest, B123)
summary(mod)

ggplot(B123, aes(x=T_soilest, y=T_soil)) + geom_point(alpha=.3) + geom_smooth(method="lm", color="green") + 
  geom_abline(intercept=0, slope=1, color="purple") 

B123_filt <- B123 %>% filter(!is.na(T_soil))
mean(abs(B123_filt$T_soil-B123_filt$T_soilest))
mean(abs(B123_filt$T_soil-(B123_filt$T_soilest*mod$coefficients[2]+mod$coefficients[1])))

ggplot(B123 %>% filter(dtuse>"2023-06-20 19:31:08 MST" & dtuse<"2023-06-25 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue") + theme_bw() + ggtitle("2023 B1 soil zoom Jun")

ggplot(B123 %>% filter(dtuse>"2023-08-13 19:31:08 MST" & dtuse<"2023-08-18 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue") + theme_bw() + ggtitle("2023 B1 soil zoom Aug")

C123 <- C1 %>% filter(year(dtuse)==2023)
mod <- lm(T_soil~T_soilest, C123)
summary(mod)

ggplot(C123, aes(x=T_soilest, y=T_soil)) + geom_point(alpha=.3) + geom_smooth(method="lm", color="green") + 
  geom_abline(intercept=0, slope=1, color="purple") 

C123_filt <- C123 %>% filter(!is.na(T_soil))
mean(abs(C123_filt$T_soil-C123_filt$T_soilest))
mean(abs(C123_filt$T_soil-(C123_filt$T_soilest*mod$coefficients[2]+mod$coefficients[1])))

ggplot(C123 %>% filter(dtuse>"2023-06-20 19:31:08 MST" & dtuse<"2023-06-25 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue") + theme_bw() + ggtitle("2023 C1 soil zoom Jun")

ggplot(C123 %>% filter(dtuse>"2023-08-05 19:31:08 MST" & dtuse<"2023-08-10 19:31:08 MST"), aes(x=dtuse)) + 
  geom_line(aes(y=T_soil), color="red") +
  geom_line(aes(y=T_soilest), color="blue")+ theme_bw() + ggtitle("2023 C1 soil zoom Aug")


climateuse$dt_noyr <- climateuse$dtuse
year(climateuse$dt_noyr) <- 2024

ggplot(climateuse, aes(x=dt_noyr, y=T_soilest)) + 
  geom_line(color="blue") +
  facet_grid(site~year(dtuse)) + ggtitle("Estimated soil temps")