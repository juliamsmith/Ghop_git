library(readr)
library(tidyverse)


#read in A1 climate data
wsda1 <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/A1/A1_08162023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)
wsda1$site <- "A1"
wsd_less <- wsda1 %>% group_by(dt, site) %>% summarize(sr=mean(sr), 
                                                     ws=mean(ws), 
                                                     T_soil=mean(T_soil), 
                                                     T_0.25=mean(T_0.25),
                                                     T_0.50=mean(T_0.50),
                                                     T_0.75=mean(T_0.75),
                                                     T_1.00=mean(T_1.00),
                                                     T_1.25=mean(T_1.25))

wsd_less$dt <- as.POSIXct(wsd_less$dt, format="%m/%d/%Y %H:%M", tz = "MST" ) + 60*60 #plus 1hr

get_psi <- function(dt, site){
  if(site=="Eldo"){
    lat <- 39.9436
    lon <- -105.262
  } else if(site=="A1") {
    lat <- 40.015
    lon <- -105.376
  } else if(site=="B1") {
    lat <- 40.019
    lon <- -105.455
  } else {
    lat <- 40.0301
    lon <- -105.541
  }
  date <- as.POSIXct(dt, format= "%Y-%m-%d")
  hour <- as.numeric(format(as.POSIXct(dt), format="%H"))
  doy <- day_of_year(day=as.POSIXct(dt, format= "%Y-%m-%d"), format ="%Y-%m-%d")
  zenith <- zenith_angle(doy, 
                         lat, 
                         lon, 
                         hour)
  return(zenith)
}

#solve for surface roughness with a sampling of 2022 wind profile data -- COULD BE IMPROVED
surf <- mean(surface_roughness(u_r=c(0.46,	0.59,	1.18), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.03,	0,	0.95), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(1.21,	1.54,	1.57), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.82,	1.34,	1.37), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.69,	1.18,	1.37), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.52,	1.28,	1.41), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(2.81,	3.08,	3.17), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.36,	0.46,	0.82), zr=c(.57, .82, 1.05)))
#plot it


#read in physical model data
