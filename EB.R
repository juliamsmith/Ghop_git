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
                   start_date=as.Date(c("7/1/2023", "7/3/2023", "7/3/2023", "7/5/2023", "7/7/2023", "7/9/2023",
                                "7/13/2023", "7/15/2023", "7/15/2023", "7/17/2023","7/17/2023", "7/19/2023"), "%m/%d/%Y"), #estimated
                   end_date=as.Date(c("8/1/2023", "8/3/2023", "8/3/2023", "8/5/2023", "8/7/2023", "8/9/2023",
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
  lipid.g <- .7*mrs/2
  loss.kJ <- lipid.g*39 #assume that 39 kJ*g^-1
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

get_tpc_gains <- function(tbs, q10, a, b, c, assim.rate=.40){ #assim assumption
  dry.fec.mg.hr <- rezende_2019(tbs,q10,a,b,c)
  #print(dry.fec.mg.hr)
  dry.wga.mg.hr <- dry.fec.mg.hr*assim.rate/(1-assim.rate) #dry wg assimilated per hr, see eq 2 Youngblood https://login.offcampus.lib.washington.edu/login?qurl=https://esajournals.onlinelibrary.wiley.com%2fdoi%2ffull%2f10.1002%2fecm.1550
  wet.wga.mg.hr <- dry.wga.mg.hr*(1/.1489) #linear relationship between wet and dry
  kcal.hr <- 329/100000 * wet.wga.mg.hr #329 kcal/100g source: https://www.microgreensilo.com/microgreen-types/wheatgrass/#nutrition-facts
  kJ.hr <- kcal.hr*4.184
  return(kJ.hr)
}


get_energy_gains <- function(sppi, sitei, sexi, tbs, pops){
  pop_dat <- pops %>% filter(spp==sppi & site==sitei & sex==sexi)
  print(pop_dat)
  gains <- get_tpc_gains(tbs, 
                        pop_dat$tpc_q10, 
                        pop_dat$tpc_a, 
                        pop_dat$tpc_b, 
                        pop_dat$tpc_c)
  mrs <- get_mrs(tbs, pop_dat$mass[1], pop_dat$elev[1], pop_dat$rmr_b0[1], pop_dat$rmr_b1[1], pop_dat$rmr_b2[1], pop_dat$rmr_b3[1])
  losses <- get_mr_losses(mrs)
  print(gains)
  print(losses)
  net_gains <- gains - losses
  df <- data.frame(gains, losses, net_gains)
  return(df) 
}
                   
                   
                   
pop_energy <- function(sppi, sitei, sexi,start_date, end_date, pops, clim){
  tbs <- get_temps(sitei, start_date, end_date, clim)
  print(tbs)
  eb <- get_energy_gains(sppi, sitei, sexi, tbs, pops)
  dts <- clim %>% filter(spp==sppi & site==sitei & dtuse>=start_date & dtuse<=end_date) %>% select(dtuse)
  df <- cbind(eb, tbs, dts)
  return(df)
}         
                   
## RUN IT AND VISUALIZE ####                   

#pick a time range and try it                   
thing <- pop_energy("MB", "A1", "M", "2022-06-23", "2022-07-05", pops, clim)


#now do it for all populations and look over a range of temps
for(p in 1:12){ #can show 1:12... they are all the same
  temps = 1:45
  d<- get_energy_gains(pops[p,]$spp,pops[p,]$site,pops[p,]$sex,temps, pops)
  d$temps <- temps
  d2 <- d %>% pivot_longer(cols=c(gains, losses, net_gains), names_to="meas", values_to="vals")
  pl <- ggplot(d2, aes(x=temps, vals, color=meas)) + 
    labs(x="temperature (C)", y="energy (kJ/hr)") +
    geom_line() +
    ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  print(pl)
  
  #See TPC with TPC units
  tpcpl <- ggplot() + geom_line(aes(1:45, 
                                    rezende_2019(1:45, 
                                                 pops$tpc_q10[p], 
                                                 pops$tpc_a[p], 
                                                 pops$tpc_b[p], 
                                                 pops$tpc_c[p]))) + 
    labs(x="temperature (C)", y="dried feces/time (mg/hr)")  +
    ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  print(tpcpl)
  mrs <- 
  rmrpl <- ggplot() + geom_line(aes(1:45,
                                get_mrs(1:45, 
                                        pop_dat$mass[p], 
                                        pop_dat$elev[p], 
                                        pop_dat$rmr_b0[p], 
                                        pop_dat$rmr_b1[p], 
                                        pop_dat$rmr_b2[p], 
                                        pop_dat$rmr_b3[p])/(pop_dat$mass[p]^.9))) +
    labs(x="temperature (C)", y="CO2/time (ml/(g*hr)") + #putting it in /g but doesn't have to be
    ggtitle(paste(pops$spp[p],pops$site[p], pops$sex[p]))
  print(rmrpl)
}






