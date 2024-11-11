library(tidyverse)
library(TrenchR)
library(rTPC)

# Read and clean climate data ####

clim <- read.csv("biophys/climateuse2.csv")

fix_datetime <- function(x) {
  # Convert to character if it's not already
  x <- as.character(x)
  
  # For entries that only have a date, add " 00:00:00" to the end
  x[!grepl("\\d{2}:\\d{2}:\\d{2}$", x)] <- paste(x[!grepl("\\d{2}:\\d{2}:\\d{2}$", x)], "00:00:00")
  
  # Convert back to POSIXct
  as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
}

clim$dtuse <- fix_datetime(clim$dtuse)

summary(clim) #note the columns with "use" are the most important

# Hopper attributes ####

p <- readxl::read_xlsx("C:/Users/smith/Downloads/SpecDat2.xlsx")

pops <- data.frame(spp=c(rep("MB", 6), 
                         rep("MS", 6)), 
                   site=c(rep("A1",2), 
                          rep("B1",2),
                          rep("C1",2),
                          rep("Eldo",2),
                          rep("A1",2), 
                          rep("B1",2)),
                   elev=c(rep(2195,2), rep(2591,2), rep(3014,2), 
                          rep(1740,2), rep(2195,2), rep(2591,2)), # elevation corresponding to site
                   sex=rep(c("M", "F"), 6),
                   #mass=c(.4,.7,.35,.65,.3,.6,#in g
                   #                          .5,.5,.5,.5,.5,.5),
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
                   #pbt=c(rep(35,6), rep(37,6)), #made-up
                   #behav=c(), #?
                   start_date_22=as.POSIXct(c("6/25/2022", "6/25/2022", "6/27/2022", "6/27/2022", "7/14/2022", "7/14/2022", 
                                              "8/10/2022", "8/10/2022", "8/13/2022", "8/13/2022","8/25/2022", "8/25/2022"), tz="America/Denver", "%m/%d/%Y"), #estimated
                   end_date_22=as.POSIXct(c("7/25/2022", "7/25/2022", "7/27/2022", "7/27/2022", "8/14/2022", "8/14/2022", 
                                            "9/10/2022", "9/10/2022", "9/13/2022", "9/13/2022","9/25/2022", "9/25/2022"), , tz="America/Denver", "%m/%d/%Y"), #even more estimated... actually we'll just give them all a month for simplicity
                   start_date_23=as.POSIXct(c("7/13/2023", "7/13/2023", "7/15/2023", "7/15/2023", "7/17/2023", "7/17/2023",
                                              "7/24/2023", "7/24/2023", "7/29/2023", "7/29/2023","8/04/2023", "8/04/2023"), , tz="America/Denver", "%m/%d/%Y"), #estimated
                   end_date_23=as.POSIXct(c("8/13/2023", "8/13/2023", "8/15/2023", "8/15/2023", "8/17/2023", "8/17/2023",
                                            "8/24/2023", "8/24/2023", "8/29/2023", "8/29/2023","9/04/2023", "9/04/2023"), , tz="America/Denver", "%m/%d/%Y")) #even more estimated... actually we'll just give them all a month for simplicity

#note: for this example assuming no need for activity/behavior
#note: start and end dates could vary year to year (2022, 2023)

m <- merge(pops, p, by=c("site", "sex", "spp"))
pops <- m

pops

#setting EB start dates (DI=5.5 using self cages) and end dates at 1 month after
#could neaten later to make it part of pops, but this isn't a priority right now
sitesinfo <- data.frame(site=c("Eldo", "A1", "B1", "C1"),
                        MB_start_date_22=as.POSIXct(c(NA, "6/25/2022", "6/27/2022", "7/14/2022"), tz="America/Denver", "%m/%d/%Y"),
                        MB_end_date_22=as.POSIXct(c(NA, "7/25/2022", "7/27/2022", "8/14/2022"), tz="America/Denver", "%m/%d/%Y"),
                        MS_start_date_22=as.POSIXct(c("8/10/2022", "8/13/2022", "8/25/2022", NA), tz="America/Denver", "%m/%d/%Y"),
                        MS_end_date_22=as.POSIXct(c("9/10/2022", "9/13/2022", "9/25/2022", NA), tz="America/Denver", "%m/%d/%Y"),
                        MB_start_date_23=as.POSIXct(c(NA, "7/13/2023", "7/15/2023", "7/17/2023"), tz="America/Denver", "%m/%d/%Y"),
                        MB_end_date_23=as.POSIXct(c(NA, "8/13/2023", "8/15/2023", "8/17/2023"), tz="America/Denver", "%m/%d/%Y"),
                        MS_start_date_23=as.POSIXct(c("7/24/2023", "7/29/2023", "8/04/2023", NA), tz="America/Denver", "%m/%d/%Y"),
                        MS_end_date_23=as.POSIXct(c("8/24/2023", "8/29/2023", "9/04/2023", NA), tz="America/Denver", "%m/%d/%Y"))

sitesinfo

# add Tbs to climate data ####

## lay some groundwork ####

#new function: corrected Tb_grasshopper from TrenchR
Tb_grasshopper2.5 <- function (T_a, T_g, u, S, K_t, psi, l, Acondfact = 0.25, z = 0.001, 
                               abs = 0.7, r_g = 0.3) 
{
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
  Thick <- .025
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
}


#function: calculate psi (zenith angle of sun) given dt and site (lat and lon)
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


#get the surface roughness (which we use for all sites)
C1wsprofile <- read_csv("C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))

#get just the parts that I need for the EB... makes it run faster, but could leave as full range for flexibility
ex_clim <- clim %>% filter((dtuse>=as.POSIXct("2022-06-25 00:00", tz="America/Denver") & dtuse<=as.POSIXct("2022-09-26 00:00", tz="America/Denver")) |
                             (dtuse>=as.POSIXct("2023-06-23 00:00", tz="America/Denver") & dtuse<=as.POSIXct("2023-09-10 00:00", tz="America/Denver")))


## Add columns to climate dataframe (ultimately clim2) ####

#make a psi column
ex_clim <- ex_clim %>% rowwise() %>% mutate(psi=get_psi(dtuse, site))



#make Tb columns under different assumptions
#for the moment, let's do a range of 10%-90% sunlight and have cage and soil
clim2 <- ex_clim %>% rowwise() %>%
  mutate(Tbsoilsun=Tb_grasshopper2.5(
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
  mutate(Tbsoilshade=Tb_grasshopper2.5(
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
  mutate(Tbcagesun=Tb_grasshopper2.5(
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
  mutate(Tbcageshade=Tb_grasshopper2.5(
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

summary(clim2)

# Energy budget functions ####

## Big-ish picture ####

#returns a df with energy budget components (i.e. gains and losses at each dt)
pop_energy_complex <- function(sppi, sitei, sexi,start_date, end_date, pops, clim, which_Tb){
  tbs <- get_temps_complex(sitei, start_date, end_date, clim, which_Tb)
  tbtype <- which_Tb
  #print(tbs)
  eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
  dts <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
  df <- cbind(eb, tbs, tbtype, dts)
  return(df)
}  

#get series of Tbs given site, time range, climate df, and selected Tb assumptions
get_temps_complex <- function(sitei, start_date, end_date, clim, which_Tb) { #more things potentially (i.e. behavior rule or range of sunny-ness), spp
  clim_temps <- clim %>% filter(site==sitei & dtuse>=start_date & dtuse<=end_date)
  # we would usually get an estimate of Tbs here -- I'm just doing things super simple for a sec
  return(as.vector(clim_temps[[which_Tb]]))
}

#get energy gains, losses, and net gains by calling MR and TPC functions
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

## Metabolism ####

#get MRs for a series of Tbs
get_mrs <- function(tb, mass, elev, b0, b1, b2, b3, k=8.62*10^-5) {
  rmr <- exp(b0+b1*log(mass) + b2*(1/(k*(tb+273.15))) + b3*elev)  #leaving b0 out for now since the article doesn't share it
  #could have option of active mr, but leaving out for now
  return(rmr)
}

#convert MRs to energy lost (kJ)
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

# ^ I THINK THERE'S A SMALL ERROR IN THIS WHICH I ADDRESSED IN MY FUNCTION
# Respiratory quotient = vCO2/ vO2 so it should be 1/0.7*CO2


## Feeding ####

#should modify for LRF TPCs
get_tpc_gains <- function(tbs, q10, a, b, c, assim.rate=.40){ #assim assumption
  dry.fec.mg.hr <- rezende_2019(tbs,q10,a,b,c)
  #print(dry.fec.mg.hr)
  dry.fec.mg.hr[dry.fec.mg.hr<0]=0
  dry.wga.mg.hr <- dry.fec.mg.hr*12.8/14.8*assim.rate/(1-assim.rate) #see harrison & fewell
  kcal.hr <- dry.wga.mg.hr*14.8/1000 #from Fewell & Harrison
  kJ.hr <- kcal.hr*4.184
  return(kJ.hr)
}


## Functions to run many trials at once ####

#Process a year for one population with a variety of assumptions about Tb
process_pop_year_flexible <- function(p, year, climate_site) {
  original_site <- pops[p,]$site
  
  # Check if simulation should be run (i.e., if dates are not NA for the climate site)
  if (year == 22) {
    start_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_start_date_22")]
    end_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_end_date_22")]
  } else {
    start_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_start_date_23")]
    end_date <- sitesinfo[sitesinfo$site == climate_site, paste0(pops[p,]$spp, "_end_date_23")]
  }
  
  if (is.na(start_date) || is.na(end_date)) {
    return(NULL)  # Skip simulation if dates are NA
  }
  
  results <- map_dfr(tbs, function(tb) {
    dthing <- pop_energy_complex(pops[p,]$spp, climate_site, pops[p,]$sex, 
                                 start_date, end_date, 
                                 pops, clim2, tb)
    dthing$tbtype <- tb
    return(dthing)
  })
  
  results %>%
    pivot_longer(cols = c(gains, losses, net_gains), names_to = "meas", values_to = "vals") %>%
    mutate(spp = pops[p,]$spp,
           original_site = original_site,
           climate_site = climate_site,
           sex = pops[p,]$sex,
           year = paste0("20", year))
}


#Process all combinations of populations and climate sites... so reciprocal transplant
process_all_combinations <- function() {
  all_results <- list()
  
  for (p in 1:nrow(pops)) {
    for (climate_site in sitesinfo$site) {
      # Process 2022 data
      result_22 <- process_pop_year_flexible(p, 22, climate_site)
      if (!is.null(result_22)) all_results <- c(all_results, list(result_22))
      
      # Process 2023 data
      result_23 <- process_pop_year_flexible(p, 23, climate_site)
      if (!is.null(result_23)) all_results <- c(all_results, list(result_23))
    }
  }
  
  return(all_results)
}



# Run trials ####

#Run the simulation for all combinations

#specify the different types of Tbs (there's prob a more elegant way to do this)
tbs <- c("Tbcagesun", "Tbcageshade", "Tbsoilsun", "Tbsoilshade")

#run
all_results2 <- process_all_combinations()

final_results2 <- bind_rows(all_results2)

#save
#saveRDS(final_results2, "EBsimRTbetter.rds")