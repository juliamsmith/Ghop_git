#load packages, probably overkill
library(tidyverse)
library(gsubfn)
library(lubridate)
library(TrenchR)
library(ggnewscale)
library(tibbletime)


d <- readRDS("biophys/pairedcagews.RDS")

#apply a filter for now (may be able to suss things out later)
d <- d %>% filter(!is.na(Activity))

## Looking at activity with climate data (rather than Tb) just to play around

#temperatures at which stat is observed
ggplot(d, aes(x=T_soil, color=factor(STAT))) +
  geom_density() + facet_grid(vars(Species), vars(Site))

l <- lm(T_soil~STAT, d) #significant
summary(l)



#temperatures at which feed is observed
ggplot(d, aes(x=T_soil, color=factor(FEED))) +
  geom_density() + facet_grid(vars(Species), vars(Site))

# l2 <- lm(T_soil~FEED+Species+Site, d) 
# summary(l2)
# note: this was silly -- ofc site will explain most of the temperature
l2 <- lm(T_soil~FEED, d) #not significant -- interesting
summary(l2)

#modeling notes: x and y kind of flipped rn just for ease, but should do a better job


#new... get Tbs for several scenarios
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

dTb <- d %>% rowwise() %>%
  mutate(bTbsoilsun_.01=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .001, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .001), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, Site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) %>%
  mutate(bTbsoilshade_.01=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .001, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .001), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, Site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) %>%
  mutate(bTbcagesun=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .3), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, Site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) %>%
  mutate(bTbcageshade=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .3), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, Site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  ))


#now doing it with Tbs
ggplot(dTb, aes(x=bTbcagesun, color=factor(FEED))) +
  geom_density() + facet_grid(vars(Species), vars(Site))


#now let's make a histogram of activities with Tb

#something from before
ggplot(dTb %>% filter(!is.na(Activity)), aes(fill=Activity, x=Site)) + 
  geom_bar(position="fill") + facet_wrap(~Species)

#new idea -- kinda works!

#could be misleading wrt temperatures not commonly observed (0?) -- bTbsoilsun gives a more reasonable range
ggplot(dTb %>% filter(Activity!="EGGL" & Activity!="MATE"), aes(x=bTbsoilsun_.01, y=after_stat(count), group=Activity, fill=Activity)) +
  geom_density(position="fill") + facet_grid(vars(Species), vars(Site))

#idea -- lump CLMB and WALK?

#let's try a stacked bar plot
ggplot(data=dTb, aes(x=bTbsoilsun_.01, fill = Activity)) + geom_histogram(bins=15)  + facet_grid(vars(Species), vars(Site))


dTb_lumped <- dTb %>% mutate(Activity2=recode(Activity, WALK="MOVE", CLMB="MOVE"))

ggplot(dTb_lumped %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO"), aes(x=bTbsoilsun_.01, y=after_stat(count), group=Activity2, fill=Activity2)) +
  geom_density(adjust=1.5, position="fill") + facet_grid(vars(Species), vars(Site))

