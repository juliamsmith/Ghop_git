library(readr)
library(tidyverse)
library(TrenchR)

wsdzoom <- read_csv("zoomwsmidAug.csv")

wsdzoom_s <- wsdzoom %>% spread(clim_var, value)

get_psi <- function(dt, site){
  if(site=="Eldo"){
    lat <- 39.9436
    lon <- -105.262
  # } else if(site=="A1") {
  #   lat <- 40.015
  #   lon <- -105.376   
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

#call Tb_grasshopper
wsdtb_s <- wsdzoom_s %>% rowwise() %>%
  mutate(Tb_pred=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .01, # .001,
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ws, 1, surf, .001), #wind speed... needs work #.001
    S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.25 #0.25 -> 0 made a huge difference
  ))

#Tbs seem potentially plausible BUT why NAs / Infs starting at row 382? 

wsdzoomtb <- wsdtb_s %>% gather("clim_var", "value", -dt, -site)

#comparing soil temp and predicted Tb
ggplot(wsdzoomtb %>% filter((clim_var=="Tb_pred" | clim_var=="T_soil") & value<100), aes(x=dt, y=value, color=site, lty=clim_var)) + geom_line()

