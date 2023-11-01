library(readr)
library(tidyverse)
library(TrenchR)

wsdzoom <- read_csv("biophys/zoomwsmidAug.csv")

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

#ZOOMING IN ON NAs by adding components of energy budget -- not resolved... still need to do more 

Ts <- wsdtb_s[382,]

T_a=air_temp_profile_neutral(T_r = Ts$T_0.25,
                              zr  = 0.25,
                              z0  = surf,
                              z   = .01, # .001,
                              T_s = Ts$T_soil)
T_g, 
u, 
S, 
K_t, 
psi, 
l, 
Acondfact = 0.25, 
z = 0.001, 
          
abs = 0.7, 
r_g = 0.3 

  stopifnot(u >= 0, S >= 0, K_t >= 0, K_t <= 1, psi >= -90, 
            psi <= 90, l >= 0, Acondfact >= 0, Acondfact <= 1, z >= 
              0, abs >= 0, abs <= 1, r_g >= 0, r_g <= 1)
  T_a <- celsius_to_kelvin(T_a)
  T_g <- celsius_to_kelvin(T_g)
  sigma <- stefan_boltzmann_constant()
  epsilon <- 1
  Kf <- 0.025
  v <- 15.68 * 10^-6
  c <- l/2
  a <- (0.365 + 0.241 * l * 1000)/1000
  e <- sqrt(1 - a^2/c^2)
  A <- 2 * pi * a^2 + 2 * pi * a * c/e * asin(e)
  kd <- 1 - 0.09 * K_t
  kd[K_t > 0.22 & K_t <= 0.8] <- 0.9511 - 0.1604 * K_t + 4.388 * 
    K_t^2 - 16.638 * K_t^3 + 12.336 * K_t^4
  kd[K_t > 0.8] <- 0.165
  Sttl <- S
  Sdir <- Sttl * (1 - kd)
  Sdif <- Sttl * kd
  psi_r <- degrees_to_radians(psi)
  Re <- u * l/v
  Nu <- 0.41 * Re^0.5
  h_c <- Nu * Kf/l
  hc_s <- h_c * (-0.007 * z/l + 1.71)
  Thick <- 6 * 10^(-5)
  hcut <- 0.15
  Acond <- A * Acondfact
  sa <- 0.19 - 0.00173 * psi
  Adir <- A * sa
  Aref <- Adir
  Qdir <- abs * Adir * Sdir/cos(psi_r)
  Qdif <- abs * Aref * Sdif
  Qref <- r_g * Aref * Sttl
  Qabs <- Qdir + Qdif + Qref
  T_sky <- 0.0552 * (T_a)^1.5
  a <- A * epsilon * sigma
  b <- hc_s * A + hcut * Acond/Thick
  d <- hc_s * A * T_a + 0.5 * A * epsilon * sigma * (T_sky^4 + 
                                                       T_g^4) + hcut * Acond * T_g/Thick + Qabs
  T_b <- 1/2 * sqrt((2 * b)/(a * sqrt((sqrt(3) * sqrt(256 * 
                                                        a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1/3)/(2^(1/3) * 
                                                                                                            3^(2/3) * a) - (4 * (2/3)^(1/3) * d)/(sqrt(3) * sqrt(256 * 
                                                                                                                                                                   a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1/3))) - 
                      (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 
                         9 * a * b^2)^(1/3)/(2^(1/3) * 3^(2/3) * a) + (4 * 
                                                                         (2/3)^(1/3) * d)/(sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * 
                                                                                                            a^2 * b^4) + 9 * a * b^2)^(1/3)) - 1/2 * sqrt((sqrt(3) * 
                                                                                                                                                             sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1/3)/(2^(1/3) * 
                                                                                                                                                                                                                            3^(2/3) * a) - (4 * (2/3)^(1/3) * d)/(sqrt(3) * sqrt(256 * 
                                                                                                                                                                                                                                                                                   a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1/3))
  T_b[which(is.na(T_b))] <- NA
  kelvin_to_celsius(T_b)

