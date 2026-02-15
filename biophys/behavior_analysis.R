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


dTb_lumped <- dTb %>% mutate(Activity2=dplyr::recode(Activity, WALK="MOVE", CLMB="MOVE"))

ggplot(dTb_lumped %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO"), aes(x=bTbsoilsun_.01, y=after_stat(count), group=Activity2, fill=Activity2)) +
  geom_density(adjust=2, position="fill") + facet_grid(vars(Species), vars(Site)) #adjust varies it


#stacked barplot version
dTblb <- dTb_lumped %>% mutate(tb_range = 
                        case_when(
                          bTbsoilsun_.01 >= 9 & bTbsoilsun_.01 < 15 ~ "[09-15)",
                          bTbsoilsun_.01 >= 15 & bTbsoilsun_.01 < 20 ~ "[15,20)",
                          bTbsoilsun_.01 >= 20 & bTbsoilsun_.01 < 25 ~ "[20,25)",
                          bTbsoilsun_.01 >= 25 & bTbsoilsun_.01 < 30 ~ "[25,30)",
                          bTbsoilsun_.01 >= 30 & bTbsoilsun_.01 < 35 ~ "[30,35)",
                          bTbsoilsun_.01 >= 35 & bTbsoilsun_.01 < 40 ~ "[35,40)",
                          bTbsoilsun_.01 >= 40 & bTbsoilsun_.01 < 45 ~ "[40,45)",
                          bTbsoilsun_.01 >= 45 & bTbsoilsun_.01 < 50 ~ "[45,50)",
                          bTbsoilsun_.01 >= 50 & bTbsoilsun_.01 < 55 ~ "[50,55)",
                          bTbsoilsun_.01 >= 55 & bTbsoilsun_.01 < 60 ~ "[55,60)"
                        ))
fdTblb <- dTblb %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO")

cutoff <- 10 #picking the cutoff

su <- fdTblb %>% group_by(Species, Site, bTbsoilsun_.01) %>% summarize(under=n()<cutoff)

unders <- su %>% filter(under==TRUE)


f_dat <- fdTblb
for(i in 1:length(unders$Species)){
  f_dat <- f_dat %>% filter(!(Species==unders$Species[i] & Site==unders$Site[i] & bTbsoilsun_.01==unders$bTbsoilsun_.01[i]))
}


fdTblb %>%
mutate(Site=factor(Site, levels=c("Eldo", "B1", "C1")),
       Activity2=factor(Activity2, levels=c("STAT", "FEED", "MOVE"))) %>%  
ggplot(aes(x=tb_range, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


ggplot(dTb_lumped %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO"), aes(x=T_0.25, y=after_stat(count), group=Activity2, fill=Activity2)) +
  geom_density(adjust=2, position="fill") + facet_grid(vars(Species), vars(Site)) #adjust varies it



dTlb <- dTb_lumped %>% mutate(airtemp = 
                                 case_when(
                                   T_0.25 >= 5 & T_0.25 < 10 ~ "[05-10)",
                                   T_0.25 >= 10 & T_0.25 < 15 ~ "[10,15)",
                                   T_0.25 >= 15 & T_0.25 < 20 ~ "[15, 20)",
                                   T_0.25 >= 20 & T_0.25 < 25 ~ "[20,25)",
                                   T_0.25 >= 25 & T_0.25 < 30 ~ "[25,30)",
                                   T_0.25 >= 30 & T_0.25 < 35 ~ "[30,35)",
                                   T_0.25 >= 35 & T_0.25 < 40 ~ "[35,40)",
                                   T_0.25 >= 40 & T_0.25 < 45 ~ "[40,45)"
                                 ))

fdTlb <- dTlb %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO")

ggplot(fdTlb, aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


ggplot(fdTlb, aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count") +
  facet_grid(vars(Species), vars(Site))


#combining with past data (using air temps at 1m)
d_allold <- readRDS("biophys/pairedcagewsbothyrs.RDS") 
d_allold <- d_allold %>% filter(!is.na(Activity))
d_all_lumped <- d_allold %>% mutate(Activity2=dplyr::recode(Activity, WALK="MOVE", CLMB="MOVE"))


dalb <- d_all_lumped %>% mutate(airtemp = 
             case_when(
               T_1.00 >= 5 & T_1.00 < 10 ~ "[05-10)",
               T_1.00 >= 10 & T_1.00 < 15 ~ "[10,15)",
               T_1.00 >= 15 & T_1.00 < 20 ~ "[15, 20)",
               T_1.00 >= 20 & T_1.00 < 25 ~ "[20,25)",
               T_1.00 >= 25 & T_1.00 < 30 ~ "[25,30)",
               T_1.00 >= 30 & T_1.00 < 35 ~ "[30,35)",
               T_1.00 >= 35 & T_1.00 < 40 ~ "[35,40)",
               T_1.00 >= 40 & T_1.00 < 45 ~ "[40,45)"
             ))

dalb <- d_all_lumped %>% mutate(airtemp = 
                                  case_when(
                                    T_1.00 >= 7.5 & T_1.00 < 12.5 ~ "10",
                                    T_1.00 >= 12.5 & T_1.00 < 17.5 ~ "15",
                                    T_1.00 >= 17.5 & T_1.00 < 22.5 ~ "20",
                                    T_1.00 >= 22.5 & T_1.00 < 27.5 ~ "25",
                                    T_1.00 >= 27.5 & T_1.00 < 32.5 ~ "30",
                                    T_1.00 >= 32.5 & T_1.00 < 39 ~ "35"
                                  ))



fdalb <- dalb %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO")
fdalb %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>%
ggplot(aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


ggplot(fdalb, aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count") +
  facet_grid(vars(Species), vars(Site))

#now filter out any bin with less than 10(?) observations (or 20?)
cutoff <- 15 #picking the cutoff

su <- fdalb %>% group_by(Species, Site, airtemp) %>% summarize(under=n()<cutoff)

unders <- su %>% filter(under==TRUE)


dat <- fdalb
for(i in 1:length(unders$Species)){
  dat <- dat %>% filter(!(Species==unders$Species[i] & Site==unders$Site[i] & airtemp==unders$airtemp[i]))
}


issu <- dat %>% group_by(Species, Site, airtemp) %>% summarize(under=n()<cutoff)


#the main plot!!!!!!

dat$elev = dplyr::recode(dat$Site, Eldo="1740m", A1="2195m", B1="2915m", C1="3048m")

dat %>%
  mutate(#Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1")),
         Activity2=factor(Activity2, levels=c("FEED", "STAT","MOVE"))) %>% 
  ggplot(aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(Species ~ elev) + 
  ylab("Proportion of observations") + 
  xlab("Air temperature at 1m (°C)") +
  scale_fill_viridis_d(name="Activity", labels=c("Feeding", "Stationary", "Moving")) +
  theme(strip.text.y = element_blank(), legend.position = c(0.12, 0.755))

#split apart 2022 and 2023
ggplot(dat %>% filter(year=="2022"), aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


ggplot(dat %>% filter(year=="2023"), aes(x=airtemp, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


dat %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(Activity2=="STAT") %>%
  ggplot(aes(x=airtemp, fill=Location)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


dat_lumped <- dat %>% mutate(Location2=dplyr::recode(Location, ROCK="LOW", SOIL="LOW", VEGL="LOW",
                                              CAGH="HIGH", CAGL="HIGH", CAGM="HIGH", VEGH="HIGH"))

dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(Activity2=="STAT" & !is.na(Location2)) %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


dat_lumped2 <- dat %>% mutate(Location2=dplyr::recode(Location,CAGH="CAGE", CAGL="CAGE", CAGM="CAGE"))


#position when stationary
dat_lumped2 %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(Activity2=="STAT" & !is.na(Location2) & Location2!="ROCK") %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))

# position at any time / activity
dat_lumped2 %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(!is.na(Location2) & Location2!="ROCK") %>%
  mutate(Location2=factor(Location2, levels=c("CAGE", "VEGH", "VEGL", "SOIL"))) %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))

#shaded or not
dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1")), 
         Exposure=factor(Exposure, levels=c("ASUN", "MSUN", "SHSU", "MSHA", "SHAD"))) %>% 
  filter(!is.na(Exposure)) %>%
  ggplot(aes(x=airtemp, fill=Exposure)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))


dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(Activity2=="STAT") %>%
  filter(!is.na(Location2)) %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site)) # look into filtering tho!

dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  #  filter(Activity2=="STAT") %>%
  filter(!is.na(Location2)) %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))

#likely not enough data?
dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1"))) %>% 
  filter(Activity2=="MOVE") %>%
  filter(!is.na(Location2)) %>%
  ggplot(aes(x=airtemp, fill=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))

#tried to do hatching within the stacked activities barplot to show location
#but prob not worth it
dat_lumped %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1")),
         Activity2=factor(Activity2, levels=c("STAT", "FEED", "MOVE"))) %>% 
  ggplot(aes(x=airtemp, fill=Activity2, color=Location2, pattern=Location2, pattern_type=Location2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site))

d_all_lumped <- d_all_lumped %>% mutate(newt=format(dt, format="%H:%M:%S"))
#plotting by time of day
dalbtime <- d_all_lumped %>% mutate(TOD = 
                                      case_when( 
                                        newt >= "06:30:00" & newt < "09:00:00" ~ "08:00*",
                                        newt >= "09:00:00" & newt < "11:00:00" ~ "10:00",
                                        newt >= "11:00:00" & newt < "13:00:00" ~ "12:00",
                                        newt >= "13:00:00" & newt < "15:00:00" ~ "14:00",
                                        newt >= "15:00:00" & newt < "17:00:00" ~ "16:00"
                                      ))
#now filter out any bin with less than 10(?) observations (or 20?)
fdalbtime <- dalbtime %>% filter(Activity!="EGGL" & Activity!="MATE" & Activity!="GROO")

cutoff <- 15 #picking the cutoff

su <- fdalbtime %>% group_by(Species, Site, TOD) %>% summarize(under=n()<cutoff)

unders <- su %>% filter(under==TRUE)


dat <- fdalbtime
for(i in 1:length(unders$Species)){
  dat <- dat %>% filter(!(Species==unders$Species[i] & Site==unders$Site[i] & TOD==unders$TOD[i]))
}


issu <- dat %>% group_by(Species, Site, TOD) %>% summarize(under=n()<cutoff)

dat %>%
  mutate(Site=factor(Site, levels=c("Eldo", "A1", "B1", "C1")),
         Activity2=factor(Activity2, levels=c("FEED", "STAT","MOVE"))) %>% 
  ggplot(aes(x=TOD, fill=Activity2)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(vars(Species), vars(Site)) +
  scale_fill_discrete(name = "Activity") + 
  ylab("Proportion of observations") + 
  xlab("Time of day")


#### https://www.geeksforgeeks.org/multinomial-logistic-regression-in-r/

library(VGAM) 

fdalb$Activity2 <- as.factor(fdalb$Activity2)

fit <- vglm(Activity2 ~ T_1.00+Species+Site,
            fdalb,
            family = multinomial)

summary(fit)
 

#### https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
library(nnet)

fdalb$Activity2 <- relevel(fdalb$Activity2, ref = "STAT")
test <- multinom(Activity2 ~ T_1.00+Species+Site, data = fdalb)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1)) * 2
#note: the odds of move vs stationary varies with T_1.00... favoring move more at higher temps


#other: https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html

fdalb$Activity2 <- relevel(fdalb$Activity2, ref = "STAT")
test <- multinom(Activity2 ~ T_1.00+Species+Site, data = fdalb)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1)) * 2



#for just MS
fdalb$Activity2 <- relevel(fdalb$Activity2, ref = "STAT") #think about the justification for "STAT" bc it matters
testMS <- multinom(Activity2 ~ T_1.00+Site, data = fdalb %>% filter(Species=="MS"))
summary(testMS)

zMS <- summary(testMS)$coefficients/summary(testMS)$standard.errors

pMS <- (1 - pnorm(abs(zMS), 0, 1)) * 2
#NOTE: when it's T_1.00*Site then nothing is significant but AIC is a bit higher
#but there is potentially reason to want to have temp*Site given what we know
pMS

#just for MB
testMB <- multinom(Activity2 ~ T_1.00+Site, data = fdalb %>% filter(Species=="MB"))
summary(testMB)

zMB <- summary(testMB)$coefficients/summary(testMB)$standard.errors

pMB <- (1 - pnorm(abs(zMB), 0, 1)) * 2
#NOTE: same situation for with T_1.00*Site as for the other
pMB