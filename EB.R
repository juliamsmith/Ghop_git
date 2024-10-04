# ####
library(tidyverse)
library(rTPC)

clim <- read.csv("biophys/climateuse.csv")

#NOTES ON APPROXIMATELY WHAT TO DO

# SET GRASSHOPPER VARIABLES ####

#pick mass or range of masses for all populations

#pick metabolic rates (resting and active) and feeding TPCs for all populations

#pick PBTs for all populations

#pick behavior rules for all populations

#pick start and end dates for adulthood for all populations


# SET WEATHER VARIABLES (?) #### 

# get climate data for each site

# get range of Tbs with a range of sun/shade

# select the Tb in some way (PBT?)

# DEFINE FUNCTION(S?) ####

## Let's make a function for just one time point ####



## And a function to wrap around that for a given duration ####


# JUST AN EXAMPLE WITH ESTIMATED NUMBERS ####

## Hopper attributes

pops <- data.frame(spp=c(rep("MB", 6), 
                         rep("MS", 6)), 
                   site=c(rep("A1",2), 
                          rep("B1",2),
                          rep("C1",2),
                          rep("Eldo",2),
                          rep("A1",2), 
                          rep("B1",2)),
                   elev=c(rep(2185,2), rep(2591,2), rep(3014,2), 
                          rep(1740,2), rep(2185,2), rep(2591,2)), # elevation corresponding to site
                   sex=rep(c("M", "F"), 6),
                   mass=c(.4,.7,.35,.65,.3,.6,#in g
                          .5,.5,.5,.5,.5,.5),
                   rmr_b0=c(rep(17.1,6), #b0, b1, b2, b3 are from JAE 2014 eq 6 --
                            rep(16.5,6)), 
                   rmr_b1=c(rep(.98,6), 
                            rep(.85,6)), 
                   rmr_b2=c(rep(-.48, 6), 
                            rep(-.47, 6)),
                   rmr_b3=c(rep(1.24*10^(-4), 6), 
                            rep(1.08*10^(-4), 6)),
                   tpc_q10=c(2.63, 8.18, 3.27, 3.48, 4.09, 6.50, #for now doing rezende /hr but no mass
                             4.06, 3.49, 5.04, 3.77, 5.04, 3.52),
                   tpc_a=c(.0235, .00279, .0118, .0265, .00742, .00546,
                           .0103, .0242, .00713, .0266, .0101, .0241),
                   tpc_b=c(20.3, .000260, 17.77, 24.2, 19.5, 0,
                           19.2, 27.1, 0, 24.9, .000108, 30.9),
                   tpc_c=c(.00117, .000521, .00109, .00241, .00166, .000519,
                           .00143, .00269, .00047, .00245, .000491, .00487),
                   #amr_coeff=rep(2, 12), #multiply by rmr to get amr, 2 is a completely made-up value
                   #tpc= #let them be the same for now?
                   pbt=c(rep(35,6), rep(37,6)), #made-up
                   #behav=c(), #?
                   start_date_22=as.Date(c("7/1/2022", "7/3/2022", "7/3/2022", "7/5/2022", "7/7/2022", "7/9/2022",
                                "7/13/2022", "7/15/2022", "7/15/2022", "7/17/2022","7/17/2022", "7/19/2022"), "%m/%d/%Y"), #estimated
                   end_date_22=as.Date(c("8/1/2022", "8/3/2022", "8/3/2022", "8/5/2022", "8/7/2022", "8/9/2022",
                                         "8/13/2022", "8/15/2022", "8/15/2022", "8/17/2022","8/17/2022", "8/19/2022"), "%m/%d/%Y"), #even more estimated... actually we'll just give them all a month for simplicity
                   start_date_23=as.Date(c("7/1/2023", "7/3/2023", "7/3/2023", "7/5/2023", "7/7/2023", "7/9/2023",
                                           "7/13/2023", "7/15/2023", "7/15/2023", "7/17/2023","7/17/2023", "7/19/2023"), "%m/%d/%Y"), #estimated
                   end_date_23=as.Date(c(NA, NA, "8/3/2023", "8/5/2023", "8/7/2023", "8/9/2023",
                                         "8/13/2023", "8/15/2023", "8/15/2023", "8/17/2023","8/17/2023", "8/19/2023"), "%m/%d/%Y")) #even more estimated... actually we'll just give them all a month for simplicity

                   #note: for this example assuming no need for activity/behavior
                   #note: start and end dates could vary year to year (2022, 2023)


#View(pops)

#make the csv of temps a global variable

## FUNCTIONS ####

#for a second we can just do air temp
get_temps <- function(sitei, start_date, end_date, clim) { #more things potentially (i.e. behavior rule or range of sunny-ness), spp
  clim_temps <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% 
    select(T_1.00use) #check exact column names
  # we would usually get an estimate of Tbs here -- I'm just doing things super simple for a sec
  return(clim_temps$T_1.00use)
}

get_mrs <- function(tb, mass, elev, b0, b1, b2, b3, k=8.62*10^-5) {
  rmr <- exp(b0+b1*log(mass) + b2*(1/(k*(tb+273.15))) + b3*elev)  #leaving b0 out for now since the article doesn't share it
  #could have option of active mr, but leaving out for now
  return(rmr)
}

#vCO2= function(M, Tb, elev_m, b0, b1, b2, b3, k=8.62*10^-5) exp(b0 +b1*log(M)+b2*(1/(k*(Tb+273.15)))+b3*elev_m)
##^ this gives the same as get_mrs

get_mr_losses <- function(mrs){
  o2.ml <- 1/.7*mrs
  lipid.mg <- o2.ml/2
  loss.kJ <- lipid.mg*39/1000 #assume 39kJ/g
  # lipid.g <- .7*mrs/2
  # loss.kJ <- lipid.g*39 #assume that 39 kJ*g^-1
  return(loss.kJ)
}

## From Lauren's code / notes:
# #   Metabolic rate, accounting for elevation due to activity?
# # temperature dependence of MR (ml CO2/hr): Buckley et al. 2013 JAE, https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.12083
# # converted into lipid consumed based on a 2L O2 consumed for every 1g of lipid consumed
# #assume respiratory quotient of 0.7 for lipids update
# #converted to energy use assuming 39 kJ g^−1 lipid
# vCO2= function(M, Tb, elev_m, b0, b1, b2, b3, k=8.62*10^-5) exp(b0 +b1*log(M)+b2*(1/(k*(Tb+273.15)))+b3*elev_m)
# lipid.g= function(vCO2) (0.7*vCO2/2)

# get_tpc_gains <- function(tbs, q10, a, b, c, assim.rate=.40){ #assim assumption
#   dry.fec.mg.hr <- rezende_2019(tbs,q10,a,b,c)
#   #print(dry.fec.mg.hr)
#   dry.wga.mg.hr <- dry.fec.mg.hr*assim.rate/(1-assim.rate) #dry wg assimilated per hr, see eq 2 Youngblood https://login.offcampus.lib.washington.edu/login?qurl=https://esajournals.onlinelibrary.wiley.com%2fdoi%2ffull%2f10.1002%2fecm.1550
#   wet.wga.mg.hr <- dry.wga.mg.hr*(1/.1489) #linear relationship between wet and dry
#   kcal.hr <- 329/100000 * wet.wga.mg.hr #329 kcal/100g source: https://www.microgreensilo.com/microgreen-types/wheatgrass/#nutrition-facts
#   kJ.hr <- kcal.hr*4.184
#   return(kJ.hr)
# }

get_tpc_gains <- function(tbs, q10, a, b, c, assim.rate=.40){ #assim assumption
  dry.fec.mg.hr <- rezende_2019(tbs,q10,a,b,c)
  #print(dry.fec.mg.hr)
  dry.fec.mg.hr[dry.fec.mg.hr<0]=0
  dry.wga.mg.hr <- dry.fec.mg.hr*12.8/14.8*assim.rate/(1-assim.rate) #see harrison & fewell
  kcal.hr <- dry.wga.mg.hr*14.8/1000 #from Fewell & Harrison
  kJ.hr <- kcal.hr*4.184
  return(kJ.hr)
}


get_energy_gains <- function(sppi, sitei, sexi, tbs, pops){
  pop_dat <- pops %>% filter(spp==sppi & site==sitei & sex==sexi)
  #print(pop_dat)
  gains <- get_tpc_gains(tbs, 
                        pop_dat$tpc_q10, 
                        pop_dat$tpc_a, 
                        pop_dat$tpc_b, 
                        pop_dat$tpc_c)
  mrs <- get_mrs(tbs, pop_dat$mass[1], pop_dat$elev[1], pop_dat$rmr_b0[1], pop_dat$rmr_b1[1], pop_dat$rmr_b2[1], pop_dat$rmr_b3[1])
  losses <- get_mr_losses(mrs)
  net_gains <- gains - losses
  df <- data.frame(gains, losses, net_gains)
  return(df) 
}
                   
                   
                   
pop_energy <- function(sppi, sitei, sexi,start_date, end_date, pops, clim){
  tbs <- get_temps(sitei, start_date, end_date, clim)
  #print(tbs)
  eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
  dts <- clim %>% filter(spp==sppi & site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
  df <- cbind(eb, tbs, dts)
  return(df)
}         
     

## RUN IT AND VISUALIZE ####                   

#pick a time range and try it                   
#thing <- pop_energy("MB", "A1", "M", "2022-06-23", "2022-07-05", pops, clim)


#now do it for all populations and look over a range of temps
dall <- data.frame(temps=NA, meas=NA, vals=NA, spp=NA, site=NA, sex=NA)
for(p in 1:12){ #can show 1:12
  temps = 1:60
  d<- get_energy_gains(pops[p,]$spp,pops[p,]$site,pops[p,]$sex,temps, pops)
  d$temps <- temps
  d2 <- d %>% pivot_longer(cols=c(gains, losses, net_gains), names_to="meas", values_to="vals")
  d2$spp <- pops[p,]$spp
  d2$site <- pops[p,]$site
  d2$sex <- pops[p,]$sex 
  dall <- rbind(dall, d2)
  # pl <- ggplot(d2, aes(x=temps, vals, color=meas)) + 
  #   labs(x="temperature (C)", y="energy (kJ/hr)") +
  #   geom_line() +
  #   ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  # print(pl)
  
  # #See TPC with TPC units
  # tpcpl <- ggplot() + geom_line(aes(1:40, 
  #                                   rezende_2019(1:40, 
  #                                                pops$tpc_q10[p], 
  #                                                pops$tpc_a[p], 
  #                                                pops$tpc_b[p], 
  #                                                pops$tpc_c[p]))) + 
  #   labs(x="temperature (C)", y="dried feces/time (mg/hr)")  +
  #   ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  # print(tpcpl)
  # mrs <- 
  # rmrpl <- ggplot() + geom_line(aes(1:40,
  #                               get_mrs(1:40, 
  #                                       pop_dat$mass[p], 
  #                                       pop_dat$elev[p], 
  #                                       pop_dat$rmr_b0[p], 
  #                                       pop_dat$rmr_b1[p], 
  #                                       pop_dat$rmr_b2[p], 
  #                                       pop_dat$rmr_b3[p])/(pop_dat$mass[p]^.9))) +
  #   labs(x="temperature (C)", y="CO2/time (ml/(hr*(g^.9))") + #putting it in /g but doesn't have to be
  #   ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  # print(rmrpl)
}

dall <- dall %>% mutate(site=fct_relevel(site, "Eldo", "A1", "B1", "C1"))
MB <- dall %>% filter(spp=="MB")
MS <- dall %>% filter(spp=="MS")

ggplot(MB, aes(x=temps, y=vals, color=meas)) + geom_line() + 
  labs(x="temperature (C)", y="energy (kJ/hr)")  + facet_grid(site~sex)+
  ggtitle("MB populations")

ggplot(MS, aes(x=temps, y=vals, color=meas)) + geom_line() + 
  labs(x="temperature (C)", y="energy (kJ/hr)")  + facet_grid(site~sex)+
  ggtitle("MS populations")


plot(1:45, rezende_2019(1:45, 
                        pops$tpc_q10[1], 
                        pops$tpc_a[1], 
                        pops$tpc_b[1], 
                        pops$tpc_c[1]))


#JUST JULIA'S SCRATCH PAD
# ###
# spec.dat=read.csv("SpecData.csv")
# 
# vCO2= function(M, Tb, elev_m, b0, b1, b2, b3, k=8.62*10^-5) exp(b0 +b1*log(M)+b2*(1/(k*(Tb+273.15)))+b3*elev_m)
# lipid.g= function(vCO2) (0.7*vCO2/2)
# 
# plot(1:60, vCO2(M=spec.dat[1,"Massg_C1"], 1:60, elev_m=3048, b0=spec.dat[1,"b0"], b1=spec.dat[1,"b1"], b2=spec.dat[1,"b2"], b3=spec.dat[1,"b3"]), type="l", ylab="MR (ml CO2/hr)", xlab="body temperature (C)", cex.lab=1.5)
# 
# plot(1:60, vCO2(M=spec.dat[1,"Massg_C1"], 1:60, elev_m=3048, b0=pop_dat$rmr_b0[1], b1=pop_dat$rmr_b1[1], b2=pop_dat$rmr_b2[1], b3=pop_dat$rmr_b3[1]), type="l", ylab="MR (ml CO2/hr)", xlab="body temperature (C)", cex.lab=1.5)

tbs=35
q10=pops$tpc_q10[1]
a=pops$tpc_a[1]
b=pops$tpc_b[1] 
c=pops$tpc_c[1]
dry.fec.mg.hr <- rezende_2019(tbs,q10,a,b,c)
#print(dry.fec.mg.hr)
assim.rate=.4
dry.wga.mg.hr <- dry.fec.mg.hr*assim.rate/(1-assim.rate) #dry wg assimilated per hr
wet.wga.mg.hr <- dry.wga.mg.hr*(1/.1489) #linear relationship between wet and dry
kcal.hr <- 329/100000 * wet.wga.mg.hr #329 kcal/100g 
kJ.hr <- kcal.hr*4.184

#kcal.hr.d <- dry.wga.mg.hr*25/8000 #from powder https://www.verywellfit.com/the-benefits-of-wheatgrass-88680
kcal.hr.d <- dry.wga.mg.hr*14.8/1000 #from Fewell & Harrison




### I think it may be MR actually:
p=1
mrs <- get_mrs(35, 
        pop_dat$mass[p], 
        pop_dat$elev[p], 
        pop_dat$rmr_b0[p], 
        pop_dat$rmr_b1[p], 
        pop_dat$rmr_b2[p], 
        pop_dat$rmr_b3[p])




  lipid.mg <- (1/.7)*mrs/2
  loss.kJ <- lipid.mg*39/1000 #assume that 39 kJ*g^-1
print(loss.kJ)

print(mrs*1/.7*.437)


#first let's run it through a full-ish range of dates
pop_energy <- function(sppi, sitei, sexi,start_date, end_date, pops, clim){
  tbs <- get_temps(sitei, start_date, end_date, clim)
  #print(tbs)
  eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
  dts <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
  df <- cbind(eb, tbs, dts)
  return(df)
}  

pop_energy("MB", "C1", "M", "")


dall2 <- data.frame(tbs=NA, dtuse=NA, meas=NA, vals=NA, spp=NA, site=NA, sex=NA)
for(p in 3:12){ #can show 1:12
  dthing <- pop_energy(pops[p,]$spp,pops[p,]$site,pops[p,]$sex, pops[p,]$start_date_23, pops[p,]$end_date_23, pops, clim)
  d2 <- dthing %>% pivot_longer(cols=c(gains, losses, net_gains), names_to="meas", values_to="vals")
  d2$spp <- pops[p,]$spp
  d2$site <- pops[p,]$site
  d2$sex <- pops[p,]$sex 
  dall2 <- rbind(dall2, d2)
  
}  

dall2 <- dall2 %>% filter(!is.na(spp))
dall2_wide <- dall2 %>% group_by(meas) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = meas, values_from = vals) %>%
  select(-row)

dall2_wide <- dall2_wide %>% mutate(yr = year(dtuse)) %>% mutate(site=fct_relevel(site, "Eldo", "A1", "B1", "C1"))

#energy gain
s <- dall2_wide %>% group_by(spp, site, sex, yr) %>% 
  summarize(sum_netgains=sum(net_gains))

#a plot based on air temps
ggplot(s, aes(x=site, y=sum_netgains, color=sex)) + geom_col() + facet_wrap(spp~yr)



get_temps <- function(sitei, start_date, end_date, clim) { #more things potentially (i.e. behavior rule or range of sunny-ness), spp
  clim_temps <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% 
    select(T_1.00use) #check exact column names
  # we would usually get an estimate of Tbs here -- I'm just doing things super simple for a sec
  return(clim_temps$T_1.00use)
}

library(TrenchR)
C1wsprofile <- read_csv("C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))

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

clim <- clim %>% rowwise() %>% mutate(psi=get_psi(dtuse, site))


clim2 <- clim %>% rowwise() %>%
  mutate(bTbsoilsun_.01=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .001, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .001), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(bTbsoilshade_.01=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .001, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .001), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(bTbcagesun=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .3), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(bTbcageshade=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .3), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>% mutate(altTb= Tb_grasshopper(
  T_a=air_temp_profile_neutral(T_r = T_1.00use,
                               zr  = 0.25,
                               z0  = surf,
                               z   = .01, # .001, #had .01 at some point
                               T_s = T_soilest), #not positive abt T_s
  T_g=T_soilest, #air, #soil, #ground temp
  u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .01), #wind speed... needs work #.001 -- make wsuse=0 -> .001
  S=ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
  K_t=.7, #clearness index (???)... needs work, but for now just guessing
  psi=psi, #solar zenith angle... needs work
  l=.03, #grasshopper length, rn just guessing 3cm and not varying
  Acondfact=.05 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
))

clim2long <- clim2 %>% pivot_longer(cols=c(Tbsoilsun, Tbsoilshade, Tbcagesun, Tbcageshade, T_1.00use), names_to="meas", values_to="vals")

#isolate just the date from a datetime
clim2long <- clim2long %>% mutate(dtuse=as.Date(dtuse))


#use ggplot to plot distributions of each body temperature (the columns I just created, plus T_1.00use)
ggplot(clim2long, aes(x=vals, color=meas)) + geom_density() + facet_wrap(~site)

#make a plot of the body temperatures over time on 7/18/2022 over the course of the day
clim2_7_18 <- clim2long %>% filter(as.Date(dtuse)=="2022-07-18") #doesn't work because of the times


library(scales)
library(lubridate)


clim2_7_18 <- clim2_7_18 %>%
  mutate(dtuse = ymd_hms(dtuse))

clim2_7_18 <- clim2_7_18 %>% 
  arrange(dtuse)

ggplot(clim2_7_18, aes(x=dtuse, y=vals, color=meas)) + 
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M") +
  facet_wrap(~site)

#for the moment, let's do a range of 10%-90% sunlight and have cage and soil
clim2 <- clim %>% rowwise() %>%
  mutate(Tbsoilsun=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .01, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .001), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=.9*ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    z=.025,
    Acondfact=0.25 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(Tbsoilshade=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .01, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .001), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=.1*ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    z=.025,
    Acondfact=0.25 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(Tbcagesun=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .3), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=.9*ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    z=.325,
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  )) %>%
  mutate(Tbcageshade=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_1.00use,
                                 zr  = 1,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soilest), #not positive abt T_s
    T_g=T_soilest, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(wsuse==0, .001, wsuse), 1, surf, .3), #wind speed... needs work #.001 -- make wsuse=0 -> .001
    S=.1*ifelse(psi!=90 & psi!= -90, sruse, 0), #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=psi, #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    z=.325,
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sruse takes over and get really hot tbs
  ))

#now we want to run through dates with Tb estimates
#note -- this is written inefficiently... don't need to recalculate body temperatures (yet!) for the same site
#tho in principle different sizes and colors could matter slightly


dall2 <- data.frame(tbs=NA, tbtype=NA, dtuse=NA, meas=NA, vals=NA, spp=NA, site=NA, sex=NA)
tbs <- c("Tbcagesun", "Tbcageshade", "Tbsoilsun", "Tbsoilshade")#, "Tbcagesun", "Tbcageshade")
for(p in 1:12){ #can show 1:12
  dth <- data.frame(gains=NA, losses=NA, net_gains=NA, tbs=NA, tbtype=NA, dtuse=NA)
  for(tb in tbs) {
    dthing <- pop_energy_complex(pops[p,]$spp,pops[p,]$site,pops[p,]$sex, pops[p,]$start_date_22, pops[p,]$end_date_22, pops, clim, tb)    
    dthing <- rbind(dth, dthing)
  }
  d2 <- dthing %>% pivot_longer(cols=c(gains, losses, net_gains), names_to="meas", values_to="vals")
  d2$spp <- pops[p,]$spp
  d2$site <- pops[p,]$site
  d2$sex <- pops[p,]$sex 
  dall2 <- rbind(dall2, d2)
  
}

for(p in c(3:8, 11:12)){ #can show 1:12 #c(3:8, 11:12) generally -- had to do a weird thing
  dth <- data.frame(gains=NA, losses=NA, net_gains=NA, tbs=NA, tbtype=NA, dtuse=NA)
  for(tb in tbs) {
    dthing <- pop_energy_complex(pops[p,]$spp,pops[p,]$site,pops[p,]$sex, pops[p,]$start_date_23, pops[p,]$end_date_23, pops, clim, tb)    
    dthing <- rbind(dth, dthing)
  }
  d2 <- dthing %>% pivot_longer(cols=c(gains, losses, net_gains), names_to="meas", values_to="vals")
  d2$spp <- pops[p,]$spp
  d2$site <- pops[p,]$site
  d2$sex <- pops[p,]$sex 
  dall2 <- rbind(dall2, d2)
  
}  



#Claude's code
library(tidyverse)

# Initialize an empty list to store results
all_results <- list()

tbs <- c("Tbcagesun", "Tbcageshade", "Tbsoilsun", "Tbsoilshade")

# Function to process data for a single population and year
process_pop_year <- function(p, year) {
  results <- map_dfr(tbs, function(tb) {
    if (year == 22) {
      dthing <- pop_energy_complex(pops[p,]$spp, pops[p,]$site, pops[p,]$sex, 
                                   pops[p,]$start_date_22, pops[p,]$end_date_22, 
                                   pops, clim, tb)
    } else {
      dthing <- pop_energy_complex(pops[p,]$spp, pops[p,]$site, pops[p,]$sex, 
                                   pops[p,]$start_date_23, pops[p,]$end_date_23, 
                                   pops, clim, tb)
    }
    dthing$tbtype <- tb
    return(dthing)
  })
  
  results %>%
    pivot_longer(cols = c(gains, losses, net_gains), names_to = "meas", values_to = "vals") %>%
    mutate(spp = pops[p,]$spp,
           site = pops[p,]$site,
           sex = pops[p,]$sex)
}

# Process 2022 data
all_results <- c(all_results, map(1:12, ~process_pop_year(.x, 22)))

# Process 2023 data
all_results <- c(all_results, map(c(3:8, 11:12), ~process_pop_year(.x, 23)))

# Combine all results
dall2 <- bind_rows(all_results)

# Remove any potential NA rows from initialization
dall2 <- dall2 %>% filter(!is.na(dtuse))

#end of Claude's code


dall2net <- dall2 %>% filter(!is.na(spp) & !is.na(dtuse) & !is.na(vals) & !is.na(meas) & !is.na(tbs) & !is.na(tbtype) &
                            meas=="net_gains")
dall2net$net_gains <- dall2net$vals
dall2net <- dall2net %>% select(-vals, -meas)

dall3 <- dall2 #just so I don't lose work when I re-run
dall2net <- dall2net %>% distinct() #there was 161153; now 142721; note: even with Claude's code reduces

dall2net_wide <- dall2net %>%
  pivot_wider(
    id_cols = c(dtuse, spp, site, sex),
    names_from = tbtype,
    values_from = c(tbs, net_gains),
    names_glue = "{.value}_{tbtype}"
  )


dall2net_wide <- dall2net_wide %>% 
  rowwise() %>%
  mutate(
    highest_ng = max(net_gains_Tbcagesun, 
                     net_gains_Tbcageshade, 
                     net_gains_Tbsoilsun,
                     net_gains_Tbsoilshade, 
                     na.rm = TRUE),
    lowest_ng = min(net_gains_Tbcagesun, 
                    net_gains_Tbcageshade, 
                    net_gains_Tbsoilsun,
                    net_gains_Tbsoilshade, 
                    na.rm = TRUE)
  ) %>%
  ungroup()

dall2net_wideold <- dall2net_wide

# #all this doesn't currently have to be here
# 
# dall2net %>%
#   dplyr::group_by(dtuse, spp, site, sex, tbtype) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# 
# 
# dall2net_wide <- dall2net %>% group_by(tbtype) %>%
#   mutate(row = row_number()) %>%
#   tidyr::pivot_wider(names_from = tbtype, values_from = net_gains) %>%
#   select(-row)
# 
# dall2_wide <- dall2 %>%
#   group_by(tbtype) %>%
#   mutate(row = row_number()) %>%
#   # Step 1: Pivot the temperature data
#   tidyr::pivot_wider(
#     #id_cols = c(dtuse, spp, site, sex, meas, vals, row),
#     names_from = tbtype,
#     values_from = tbs#,  # Note: changed from 'tb' to 'tbs'
#     #names_prefix = "tb_"
#   ) %>%
#   # Step 2: Pivot the energy values data
#   pivot_wider(
#     id_cols = c(dtuse, spp, site, sex, tb_Tbsoilshade, tb_Tbcageshade),
#     names_from = meas,
#     values_from = vals
#   )
# 
# 
# dall2_wide <- dall2 %>%
#   # Step 1: Pivot the temperature data
#   pivot_wider(
#     id_cols = c(dtuse, spp, site, sex, meas, vals),
#     names_from = tbtype,
#     values_from = tbs,
#     names_prefix = "tb_"
#   ) %>%
#   # Step 2: Pivot the energy values data
#   pivot_wider(
#     id_cols = c(dtuse, spp, site, sex, starts_with("tb_")),
#     names_from = c(meas, tbtype),
#     values_from = vals,
#     names_sep = "_"
#   )
# 
# dall2_wide <- dall2 %>%
#   pivot_wider(
#     id_cols = c(dtuse, spp, site, sex),
#     names_from = c(meas, tbtype),
#     values_from = c(vals, tb),
#     names_sep = "_"
#   )
# 
# 
# dall2_wide <- dall2 %>% group_by(meas) %>%
#   mutate(row = row_number()) %>%
#   tidyr::pivot_wider(names_from = c(meas, tbtype), values_from = c(vals, tbs)) %>%
#   select(-row)
# 
# dall2_wide <- dall2_wide %>% mutate(yr = year(dtuse)) %>% mutate(site=fct_relevel(site, "Eldo", "A1", "B1", "C1"))

#energy gain
dall2net_wide <- dall2net_wide %>% mutate(yr = year(dtuse)) %>% mutate(site=fct_relevel(site, "Eldo", "A1", "B1", "C1"))

s <- dall2net_wide %>% group_by(spp, site, sex, yr) %>% 
  summarize(sum_netgains_soil=sum(net_gains_Tbcageshade),sum_netgains_cage=sum(net_gains_Tbsoilshade))

#a plot based on air temps
ggplot(s, aes(x=site, y=sum_netgains_cage, color=sex)) + geom_col() + facet_wrap(spp~yr)

pop_energy_complex <- function(sppi, sitei, sexi,start_date, end_date, pops, clim, which_Tb){
  tbs <- get_temps_complex(sitei, start_date, end_date, clim, which_Tb)
  tbtype <- which_Tb
  #print(tbs)
  eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
  dts <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
  df <- cbind(eb, tbs, tbtype, dts)
  return(df)
}  

get_temps_complex <- function(sitei, start_date, end_date, clim, which_Tb) { #more things potentially (i.e. behavior rule or range of sunny-ness), spp
  clim_temps <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date)
  # we would usually get an estimate of Tbs here -- I'm just doing things super simple for a sec
  return(as.vector(clim_temps[[which_Tb]]))
}
##test
sppi <- "MB"
sexi <- "M"
sitei <- "B1"
clim <- clim2
start_date <- "2022-06-23"
end_date  <- "2022-06-25"
which_Tb <- "Tbsoilsun"
# 
# clim_temps <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% 
#   select(which_Tb) #check exact column names
tbs <- get_temps_complex(sitei, start_date, end_date, clim, which_Tb)
tbtype <- which_Tb
#print(tbs)
eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
dts <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
df <- cbind(eb, tbs, tbtype, dts)