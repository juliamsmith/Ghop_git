library(readr)
library(tidyverse)
library(TrenchR)

#wsdzoom <- read_csv("biophys/zoomwsmidAug.csv") #something weird happened here

#doing this instead
wsdzoom <- readRDS("biophys/zoomwsmidAug.RDS")

wsdzoom_s <- wsdzoom %>% spread(clim_var, value)

#issue arises when there is some sr in the reading but it the angle is 90
#gonna filter by time of day for now but should perhaps revisit readings on sr
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
#surf <- .5 #may be more realistic... eyeballed from eldo surface roughness data online (nrel)
#call Tb_grasshopper


#add time of day as a column
wsdzoom_s2 <- wsdzoom_s %>% mutate(time=format(as.POSIXct(dt), format = "%H:%M"))

##this gives us only the daytime climate data
#wsdzoomday <- wsdzoom_s2 %>% filter(as.POSIXct(time, format = "%H:%M") >as.POSIXct("6:30", format = "%H:%M") & as.POSIXct(time, format = "%H:%M")<as.POSIXct("20:00", format = "%H:%M"))




wsdtb_s <- wsdzoom_s2 %>% rowwise() %>%
  mutate(Tb_pred=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .01, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) #%>% #note -- many more warnings if Acond=0
  # mutate(Tb_pred0.50=Tb_grasshopper(
  #   T_a=air_temp_profile_neutral(T_r = T_0.50,
  #                                zr  = 0.5,
  #                                z0  = surf,
  #                                z   = .01, # .001,
  #                                T_s = T_soil), #not positive abt T_s
  #   T_g=T_soil, #air, #soil, #ground temp
  #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
  #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
  #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
  #   psi=get_psi(dt, site), #solar zenith angle... needs work
  #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
  #   Acondfact=0 #0.25 -> 0 made a huge difference
  # )) %>%
  # mutate(Tb_pred0.75=Tb_grasshopper(
  #   T_a=air_temp_profile_neutral(T_r = T_0.75,
  #                                zr  = 0.75,
  #                                z0  = surf,
  #                                z   = .01, # .001,
  #                                T_s = T_soil), #not positive abt T_s
  #   T_g=T_soil, #air, #soil, #ground temp
  #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
  #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
  #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
  #   psi=get_psi(dt, site), #solar zenith angle... needs work
  #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
  #   Acondfact=0 #0.25 -> 0 made a huge difference
  # )) %>%
  # mutate(Tb_pred1.00=Tb_grasshopper(
  #   T_a=air_temp_profile_neutral(T_r = T_1.00,
  #                                zr  = 1,
  #                                z0  = surf,
  #                                z   = .01, # .001,
  #                                T_s = T_soil), #not positive abt T_s
  #   T_g=T_soil, #air, #soil, #ground temp
  #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
  #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
  #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
  #   psi=get_psi(dt, site), #solar zenith angle... needs work
  #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
  #   Acondfact=0 #0.25 -> 0 made a huge difference
  # )) %>%
  # mutate(Tb_pred1.25=Tb_grasshopper(
  #   T_a=air_temp_profile_neutral(T_r = T_1.25,
  #                                zr  = 1.25,
  #                                z0  = surf,
  #                                z   = .01, # .001,
  #                                T_s = T_soil), #not positive abt T_s
  #   T_g=T_soil, #air, #soil, #ground temp
  #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
  #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
  #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
  #   psi=get_psi(dt, site), #solar zenith angle... needs work
  #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
  #   Acondfact=0 #0.25 -> 0 made a huge difference, not true with .05
  # ))


wsdtb_s <-wsdtb_s[-c(1087, 1088, 1090), ] #remove a few strange rows
wsdzoomtb <- wsdtb_s %>% gather("clim_var", "value", -dt, -site, -time)
wsdzoomtb$value <- as.numeric(wsdzoomtb$value)

##comparing soil temp and predicted Tb
#ggplot(wsdzoomtb %>% filter((clim_var=="Tb_pred" | clim_var=="T_soil") & value<100), aes(x=dt, y=value, color=site, lty=clim_var)) + geom_line()

#plot some data -- Eldo is most extreme
coeff=30
ggplot(wsdtb_s %>% filter(site=="Eldo"), aes(x=dt)) + 
  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
#  geom_line(aes(y=mbsoil, color="Soil model temp")) +
#  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=T_0.25, color="Air temp")) + 
  geom_line(aes(y=T_soil, color = "Soil temp")) +
  geom_line(aes(y=ws, color="Wind speed")) +
  geom_line(aes(y=sr/coeff, color = "Solar rad")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C) / Wind speed (m/s)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Radiation (W/m2)")
  ) +
  theme(axis.title.y.right = element_text(color = "red")) +
  scale_color_manual(name='Climate vars',
                     breaks=c('Ghop temp', 'Soil model temp', 'Veg model temp', 'Air temp', 'Soil temp', 'Wind speed', 'Solar rad'),
                     values=c('Ghop temp'='green',
                              'Soil model temp' = 'darkseagreen',
                              'Veg model temp' = 'darkgreen',
                              'Air temp' = 'blue', 
                              'Soil temp'='brown', 
                              'Wind speed'='gray', 
                              'Solar rad'='red')) +
  ggtitle("Eldo climate")  
  
  
