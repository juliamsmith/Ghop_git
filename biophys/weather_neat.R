library(tidyverse)
library(gsubfn)
library(lubridate)
library(TrenchR)
library(ggnewscale)
library(tibbletime)

setwd("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022")

# ASSEMBLE OUR WS DATA (all) ####

fs <- list.files("WeatherStationData", pattern="^[0-9].*.txt")

for(i in 1:12){
  line <- fs[i]
  site <- strsplit(line, "\\_|\\.")[[1]][3] #note the site (will add as a column)
  datafile <- readLines(paste0("WeatherStationData/", line)) #read file in
  colstr <- read.pattern(text=datafile, pattern="^Date Time.*")[2,] 
  cols <- trimws(strsplit(colstr, split=",")[[1]])
  df <- read.pattern(text=datafile, pattern="^2022.*")
  assign(paste0("set", i), df %>% separate(col=V1, into=cols, sep=",") %>% mutate(Site = site))
}


#split up because of a weird difference between my computer and Monica's
for(i in 13:length(fs)){
  line <- fs[i]
  site <- strsplit(line, "\\_|\\.")[[1]][3] #note the site (will add as a column)
  datafile <- readLines(paste0("WeatherStationData/", line)) #read file in
  colstrs <- read.pattern(text=datafile, pattern="^Ch. Descr:.*")
  cols <- trimws(do.call(rbind, strsplit(colstrs[["V1"]], split=": "))[,2])
  cols <- c("Date Time", cols[cols != ""], "ambient temp")
  df <- read.pattern(text=datafile, pattern="^2022.*")
  assign(paste0("set", i), df %>% separate(col=V1, into=cols, sep=",") %>% mutate(Site = site))
}
#warnings probably fine

#1, 6, 8, 11, 14 correct the name
set1 <- set1 %>% dplyr::rename(`Avg wind speed`=`Avg windspeed`)
set6 <- set6 %>% dplyr::rename(`Avg wind speed`=`Avg windspeed`, 
                               `Solar radiation sensor` = `solar radiation sensor`)
set8 <- set8 %>% dplyr::rename(`Avg wind speed`=`Avg windspeed`, 
                               `Solar radiation sensor` = `solar radiation sensor`)
set11 <- set11 %>% dplyr::rename(`Avg wind speed`=`Avg windspeed`, 
                                 `Solar radiation sensor` = `solar radiation sensor`)
set14 <- set14 %>% dplyr::rename(`Avg wind speed`=`Avg windspeed`, 
                                 `Solar radiation sensor` = `solar radiation sensor`)

WS_dat <- bind_rows(set1, set2, set3, set4, set5, set6, set7, set8, set9, set10,
                    set11, set12, set13, set14, set15, set16)

WS_dat <- WS_dat %>% select(-`ambient temp`, -`MB VEGH`, -`MS VEGH`, -`MB SOIL`, -`MS SOIL`) #remove the ambient temp that isn't my air temp

WS <- WS_dat %>% dplyr::rename(dt=`Date Time`,
                               site=Site,
                               ws=`Avg wind speed`,
                               sr=`Solar radiation sensor`,
                               T_1.00=`Ambient temp`,
                               T_soil=`Soil temp`)

WS$dt <- as.POSIXct(WS$dt, tz = "MST" ) #doesn't change the times, just sets the timezone
WS$ws <- as.numeric(WS$ws)
WS$sr <- as.numeric(WS$sr)
WS$T_1.00 <- as.numeric(WS$T_1.00)
WS$T_soil <- as.numeric(WS$T_soil)


WS <- WS %>% mutate(dt=dt+hours(1), t=format(dt, "%I:%M:%S %p")) #also need to adjust for MST
WS <- WS %>% mutate(t=format(dt, "%I:%M:%S %p")) 


# Isolate usable WS data

#a first pass at getting rid of unusable data, there's prob still issues, see my notes
uWS <- WS %>% filter(ws>=0, T_1.00>-15, T_soil>-15, sr>=0, T_1.00<60) 
uWS <- unique(uWS) #something happened causing there to be lots of duplicates!

uWS <- uWS %>% mutate(date=format(dt,"%Y-%m-%d"))
uWS$date <- as.POSIXct(uWS$date)

setwd("C:/Users/smith/Desktop/Ghop_git/biophys") #MODIFY TO YOUR PATH
#library(rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #or use this!

#read in new WS data (from 2023)
WS23 <- readRDS("all_WS_dat_excA1.RDS")



WS23 <- WS23 %>%
  mutate(solarWm2ReferenceValue =  sr * (1.6 / 1.67)) %>%
  mutate(sr2 = (((T_1.25 - 25)  *  0.0012)  * solarWm2ReferenceValue) + solarWm2ReferenceValue) #%>% mutate(sr = ((sr-39) * 1.8) + ((25 -T_1.25) * .0012))

uWS <- uWS %>% mutate(sr2=sr)

all <- bind_rows(uWS, WS23) %>% select(-date, -t)

all$dt_noyr <- all$dt

year(all$dt_noyr) <- 2024

coeff=30
ggplot(all, aes(x=dt_noyr)) + 
  geom_line(aes(y=T_1.00, color="Air temp")) + 
  geom_line(aes(y=T_soil, color = "Soil temp")) +
  geom_line(aes(y=ws, color="Wind speed")) +
  geom_line(aes(y=sr2/coeff, color = "Solar rad")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C) / Wind speed (m/s)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Radiation (W/m2)")
  ) +
  theme(axis.title.y.right = element_text(color = "red")) +
  scale_color_manual(name='Climate vars',
                     breaks=c('Air temp', 'Soil temp', 'Wind speed', 'Solar rad'),
                     values=c('Air temp' = 'blue', 
                       'Soil temp'='brown', 
                       'Wind speed'='gray', 
                       'Solar rad'='red')) +
  facet_grid(site~year(dt)) 

# #sanity check under construction -- checking for difference between air temperatures
# #not using for now
# sites <- c("Eldo", "A1", "B1", "C1")
# things <- (all %>% filter(site==s) %>% mutate(Tdiff=NA, dtdiff=NA))[1,]
# for(s in sites){
#   temp <- all %>% filter(site==s)
#   dtdiff <- diff(temp$dt)
#   Tdiff <- diff(temp$T_1.00)
#   tempdiff <- data.frame(Tdiff, dtdiff)
#   fulltemp <- cbind(temp, rbind(tempdiff, c(NA,NA))) 
#   #calculate a rate of change and if that exceeds ~1degree/min then something is weird
#   exc <- fulltemp %>% filter(dtdiff<20 & abs(Tdiff)/as.numeric(dtdiff)>1)
#   things <- rbind(things, exc)
#   }


#Getting surface roughness estimate from C1 2022
C1wsprofile <- read_csv("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022/C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf_r <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))


# READ DATA FROM OTHER SOURCES ####

## nrel data for Eldo (nrel) ####

### 2022 and 2023 #### 
nrel1 <- read.csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/nrel/nrel_eldo.csv")
nrel2 <- read.csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/nrel/nrel_eldo2.csv")
nrel0 <- rbind(nrel1, nrel2)
nrel0 <- distinct(nrel0) #this is every single minute

#grouping into 10 mins
thing <- c()
for(i in 1:72719) { #nrows/10 rounded down
  thing <- c(thing, rep(i, 10))
}
thing <- c(thing, 72720) #there's one extra observation so it gets its own group
nrel0$group <- thing
nrel_less <- nrel0 %>% 
  group_by(group) %>% 
  summarize(dt=mean(as.POSIXct(paste(DATE..MM.DD.YYYY., MST), format="%m/%d/%Y %H:%M", tz="MST")), 
            sr=mean(Global.Horizontal..W.m.2.), 
            T_2.00 = mean(Temperature...2m..deg.C.),
            ws_2.00=mean(Avg.Wind.Speed...2m..m.s.), surf=mean(Est.Surface.Roughness..m.))

#trim to relevant range for our field seasons
nrel <- nrel_less %>% filter(dt>as.Date("5/15/2022 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2022 12:21", format="%m/%d/%Y %H:%M") | 
                               (dt>as.Date("5/15/2023 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2023 12:21", format="%m/%d/%Y %H:%M")))

### Adjustments ####
nrel$dt <- nrel$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary

# #ws profile down to 1m (note -- might like to use surf, but not cooperating)
# nrel <- nrel %>% mutate(ws=wind_speed_profile_neutral(u_r=WS_2.00, zr=2, z0=surf_r, z=1)) 
# 
# #need to estimate a surface temperature
# nr22 <- nrel %>% filter(year(dt)==2022)
# nr22$T_soilest_src <- soil_temperature(
#   z_r.intervals = 12,
#   z_r=2,
#   z=2, #what do the intervals mean?
#   T_a=nr22$T_2.00,
#   u=nr22$WS_2.00,
#   Tsoil0=nr22$T_2.00[1], # have to create this as a var... or just make up a #
#   z0=surf_r,
#   SSA=.7, #guess
#   TimeIn=hour(nr22$dt),
#   S=nr22$sr,
#   water_content = 0.2, #.2 here
#   air_pressure=80.8,
#   rho_so = 1620,
#   shade = TRUE
# )
# 
# nr23 <- nrel %>% filter(year(dt)==2023)
# nr23$T_soilest_src <- soil_temperature(
#   z_r.intervals = 12,
#   z_r=2,
#   z=2, #what do the intervals mean?
#   T_a=nr23$T_2.00,
#   u=nr23$WS_2.00,
#   Tsoil0=nr23$T_2.00[1], # have to create this as a var... or just make up a #
#   z0=surf_r,
#   SSA=.7, #guess
#   TimeIn=hour(nr23$dt),
#   S=nr23$sr,
#   water_content = 0.2, #.2 here
#   air_pressure=80.8,
#   rho_so = 1620,
#   shade = TRUE
# )
# 
# nrel <- rbind(nr22, nr23)
# 
# #air temp profile down to 1m (note -- might like to use surf, but not cooperating)
# nrel <- nrel %>% mutate(T_1.00=air_temp_profile(T_r=T_2.00, u_r=WS_2.00, zr=2, z0=surf_r, z=1, T_s=T_soilest_src)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m


## hydroshare data for B1 (hydroS) ####

### 2022 and 2023 ####

#reading in B1 substitute data (this is north and south facing air temp)  
#south facing is a better fit

#hydroN22 <- read_csv("hydro/hydro_Met_NF_2022.csv")
#hydroN23 <- read_csv("hydro/hydro_Met_NF_2023.csv")

#hydroN <- rbind(hydroN22, hydroN23)

#hydroN$TIMESTAMP <- as.POSIXct(hydroN$TIMESTAMP, format="%m/%d/%Y %H:%M")

hydroS22 <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/hydro/hydro_Met_SF_2022.csv")
hydroS23 <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/hydro/hydro_Met_SF_2023.csv")

hydroS <- rbind(hydroS22, hydroS23)

hydroS <- hydroS %>% mutate(dt=as.POSIXct(hydroS$TIMESTAMP, format="%m/%d/%Y %H:%M"),
                            T_2.50=`AIRTEMP(C)-2.5M(AVG)`,
                            ws_2.50=`WINDSPEED(m/s)-2.5M(AVG)`,
                            sr=`IN SW RAD(W/m^2)-2.5M(AVG)`)

hydroS <- hydroS[complete.cases(hydroS[ ,17:20]), ]
### Adjustments ####

hydroS$dt <- hydroS$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary



## Niwot LTER data for C1 (dlter) ####


### 2022 ####

C1_niwot <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/c-1cr23x-cr1000.10minute.ml.data.csv")
#note -- changed from date.time_start to date.time_end

C1_niwot$date.time_end <- as.POSIXct(C1_niwot$date.time_end)

C1_niwot$date.time_end <- force_tz(C1_niwot$date.time_end, "MST")

C1_niwot_win <- C1_niwot %>% filter((date.time_end> "2022-5-15 16:30 MST" & date.time_end<"2022-10-01 12:21 MST") | 
                                      (date.time_end>"2023-5-15 16:30 MST" & date.time_end<"2023-10-01 12:21 MST"))

C1_niwot_win_df <- as.data.frame(C1_niwot_win)

C1_niwot_win_df$dt <- as.POSIXct(C1_niwot_win_df$date.time_end, format="%m/%d/%Y %H:%M", tz = "MST") 

### 2023 ####



# library(RCurl)
# 
# url <- "ftp://niwotlter.colorado.edu/raw_climate_data/"
# filenames <- getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
# filenames <- strsplit(filenames, "\r\n")
# filenames <- unlist(filenames)
# filenames


# download.file(paste(url, "c1_cr1000_tenminute_20221031_20230903.backup", sep=""), paste("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "c1_cr1000_tenminute_20221031_20230903.backup", sep=""))
# 
# download.file(paste(url, "c1_cr1000x_tenminute.dat", sep=""), paste(""G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "c1_cr1000x_tenminute.dat", sep=""))
# 
# download.file(paste(url, "ReadMe.txt", sep=""), paste("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "ReadMe.txt", sep=""))
# 
# 
# dat <- as.data.frame(read.delim("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/c1_cr1000_tenminute_20221031_20230903.backup", sep=" "))
# 
# dat2 <- dat[4:dim(dat)[1],]
# 
# ns <- read.table(text = dat[1,], sep = ",", fill = TRUE, as.is = TRUE)
# 
# 
# d <- read.table(text = dat2, sep = ",", fill = TRUE, as.is = TRUE)
# 
# d2 <- as.data.frame(d)
# 
# colnames(d2) <- ns
# 
# #now d2 is what we want!
# 
# d2$TIMESTAMP <- as.POSIXct(d2$TIMESTAMP, tz = "MST" )

### Bind 2022 and 2023 lter together ####
# d2023 <- d2 %>% 
#   select(TIMESTAMP, AirTC_1_Avg, WS_ms_Avg, SlrW_Avg, T107_C_Avg) %>% 
#   rename(dt="TIMESTAMP", T_1.7="AirTC_1_Avg", wshigh="WS_ms_Avg", sr="SlrW_Avg", T_soil="T107_C_Avg")
dlter <- C1_niwot_win_df %>% 
  select(dt,airtemp_as_1.7_avg, ws_avg, solrad_avg, soiltemp_5cm_avg) %>%
  rename(T_1.7="airtemp_as_1.7_avg", ws_9="ws_avg", sr="solrad_avg", T_soil="soiltemp_5cm_avg")

dlter$T_1.7[dlter$T_1.7 < - 20]=NA

#dlter = rbind(d2022, d2023)  

### Adjustments ####
dlter$dt <- dlter$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary


#dlter <- dlter[-34674,] #WHY?

#uncommenting for now... removing some patchy/unreliable entries
dlter <- dlter %>% filter(dt<"2023-09-03 05:20:00")

#ws profile down to 1.7m and 1m (zr=9m... ~30ft)
#dlter2 <- dlter %>% filter(dt<"2023-09-03 05:20:00")

# dlter <- dlter %>% mutate(ws_1.7=NA,
#                           ws=NA)
# 
# dlter$ws_1.7 <- wind_speed_profile_neutral(u_r=dlter$wshigh, zr=9, z0=surf_r, z=1.7)
# dlter$ws <- wind_speed_profile_neutral(u_r=dlter$wshigh, zr=9, z0=surf_r, z=1)
# 
# 
# #air temp profile down to 1m
# dlter <- dlter %>% mutate(T_1.00=NA)
# 
# #need a surface temperature
# lt22 <- dlter %>% filter(year(dt)==2022) %>% filter(!is.na(T_1.7) & !is.na(ws_1.7) & !is.na(sr))
# 
# lt22$T_soilest <- soil_temperature(
#   z_r.intervals = 12,
#   z_r=1.7,
#   z=2, #what do the intervals mean?
#   T_a=lt22$T_1.7,
#   u=lt22$ws_1.7,
#   Tsoil0=lt22$T_1.7[1], # have to create this as a var... or just make up a #
#   z0=surf_r,
#   SSA=.7, #guess
#   TimeIn=hour(lt22$dt),
#   S=lt22$sr,
#   water_content = 0.2, #.2 here
#   air_pressure=71,
#   rho_so = 1620,
#   shade = TRUE
# )
# 
# #ISSUE ABOVE! For now I am setting T_soilest to the air temperature at 1.7m
# #lt22$T_soilest <- lt22$T_1.7
# 
# 
# lt23 <- dlter %>% filter(year(dt)==2023)
# lt23$T_soilest <- soil_temperature(
#   z_r.intervals = 12,
#   z_r=1.7,
#   z=2, #what do the intervals mean?
#   T_a=lt23$T_1.7,
#   u=lt23$ws_1.7,
#   Tsoil0=lt23$T_1.7[1], # have to create this as a var... or just make up a #
#   z0=surf_r,
#   SSA=.7, #guess
#   TimeIn=hour(lt23$dt),
#   S=lt23$sr,
#   water_content = 0.2, #.2 here
#   air_pressure=71,
#   rho_so = 1620,
#   shade = TRUE
# )
# 
# dlter <- rbind(lt22, lt23)
# 
# 
# #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m
# dlter$T_1.00 <- air_temp_profile(T_r=dlter$T_1.7, u_r=dlter$ws_1.7, zr=1.7, z0=surf_r, z=1, T_s=dlter$T_soilest)


# MERGE DATA AND MODEL ####

## Eldo/nrel ####

El <- all %>% filter(site=="Eldo")

Elints <- as.integer(El$dt)
nrints <- as.integer(nrel$dt)

nrmatchinds <-lapply(Elints, FUN=function(x){
  return(which.min(abs(nrints-x)))
})



nrel_src <- nrel %>% rename(dt_src="dt", ws_2.00_src="ws_2.00", T_2.00_src="T_2.00", sr_src="sr") 

indsvec <- rep(NA,length(nrmatchinds))
for(i in 1:length(nrmatchinds)){ indsvec[i] = nrmatchinds[[i]]}

##sanity check to make sure that the matches are never off by more than 5mins
#sum(abs(Elints-nrints[indsvec])/60>5)

Eldobind <- cbind(El, nrel_src[indsvec,]) %>% 
  select(-sr2, -Group.1, -solarWm2ReferenceValue)



#now all we are missing is some of those in-between indices
#so we look for gaps and if they are a certain size we fill them
#arrange in ascending order by dt beforehand just in case
Eldobind <- Eldobind[order(Eldobind$dt),]
#here are the indices right before gaps in coverage that are larger than 20mins
gap_inds <- which(diff(Eldobind$dt)>20)
#the first one is the gap between the 2022 and 2023 field season

#handling the three predictable gaps (beginnings and ends of seasons)
edgesnrel_src<- nrel_src %>% filter((dt_src>"2022-05-15 00:00:01 MST" & dt_src < Eldobind$dt[1]-5) |
                      (dt_src>Eldobind$dt[gap_inds[1]+1]+5 & dt_src<"2022-10-01 00:00:01 MST") |
                      (dt_src> Eldobind$dt[length(Eldobind$dt)] +5 & dt_src<"2023-10-01 00:00:01 MST"))

#handling the rest in a for loop... oops not quite how it works since I've already pared it down too much
#make a list of conditions
txt <- "gapsnrel_src <- nrel_src %>% filter(0==1" 
for(i in 2:length(gap_inds)){
  txt <- paste0(txt," | (dt_src > Eldobind$dt[gap_inds[", i, "]]+5 & dt_src < Eldobind$dt[gap_inds[", i, "]+1]-5)")
}
txt <- paste0(txt, ")")

eval(parse(text=txt)) #run that string as code

#add in modeled values to fill gaps in time
Eldobinds <- plyr::rbind.fill(Eldobind, edgesnrel_src, gapsnrel_src)


### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(Eldobinds, aes(x=T_2.00_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_2.00_src*year(dt), Eldobinds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod) #well seems strange that so much significance is attributed to year

#plot and mod we used
ggplot(Eldobinds, aes(x=T_2.00_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
modTEldo <- lm(T_1.00~T_2.00_src, Eldobinds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(modTEldo)

Eldobinds <- Eldobinds  %>% mutate(T_1.00pred = T_2.00_src*modTEldo$coefficients[2] + modTEldo$coefficients[1],
                                         T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                                         T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                                         dtuse = as.POSIXct(ifelse(is.na(T_1.00), dt_src, dt), tz="MST")) #could also create a column that's a measurement of error


Eldobinds <- unique(Eldobinds)
Eldobinds <- Eldobinds[order(Eldobinds$dtuse),]
# is2remove <- c()
# for(i in 2:(length(Eldobinds$dtuse)-1)){
#   if(Eldobinds$T_1.00ori[i]=="modelled"){
#     if(Eldobinds$T_1.00ori[i-1]=="direct" & Eldobinds$T_1.00ori[i+1]=="direct"){
#       is2remove <- c(is2remove, i)
#     }
#   }
# }
# 
# Eldobinds <- Eldobinds[-is2remove,] 
## ^ nothing to remove

ggplot(Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use
ggplot(data= Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 Eldo Air Temps")



#### Solar radiation ####
#there is an issue with the solar radiation readings after 8/28/22
ggplot(Eldobinds %>% filter(dtuse<="2022-08-28 19:31:08 MST"), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
modsrEldo <- lm(sr~sr_src, Eldobinds %>% filter(dtuse<="2022-08-28 19:31:08 MST"))
summary(modsrEldo)

Eldobinds <- Eldobinds  %>% mutate(srpred = sr_src*modsrEldo$coefficients[2] + modsrEldo$coefficients[1],
                                   sruse = ifelse(dtuse>="2022-08-28 19:31:08 MST" | is.na(sr), srpred, sr),
                                   srori = ifelse(dtuse>="2022-08-28 19:31:08 MST" | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 Eldo Solar Radiation")

#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(Eldobinds %>% filter(year(dt)==2023)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_2.00_src), color="red")

#We just set to .5 for all sites and years due to systematic differences between years and annemometers
Eldobinds <- Eldobinds  %>% mutate(wspred = .5,
                                   wsuse = .5,
                                   wsori = "modelled") #could also create a column that's a measurement of error

#### Soil temperature ####

#soil_temperature() works with a time series, so I'm handling '22 and '23 separately

Eb22 <- Eldobinds %>% filter(year(dtuse)==2022)
Eb22$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Eb22$T_1.00use,
  u=Eb22$wsuse,
  Tsoil0=Eb22$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Eb22$dtuse),
  S=Eb22$sruse,
  water_content = 0.2, #.2 here
  air_pressure=80.8,
  rho_so = 1620,
  shade = TRUE
)

Eb23 <- Eldobinds %>% filter(year(dtuse)==2023)
Eb23$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Eb23$T_1.00use,
  u=Eb23$wsuse,
  Tsoil0=Eb23$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Eb23$dtuse),
  S=Eb23$sruse,
  water_content = 0.2, #.2 here
  air_pressure=80.8,
  rho_so = 1620,
  shade = TRUE
)

Eldobinds <- rbind(Eb22, Eb23)

### Combine and refine ####
Eldobinds_old <- Eldobinds
Eldobinds <- Eldobinds %>% select(-ws_2.00_src, -group, -T_2.00_src, -surf) 

#add site column before merging 
Eldobinds$site <- "Eldo"


## B1/hydroshare ####

B1 <- all %>% filter(site=="B1")

B1ints <- as.integer(B1$dt)
hyints <- as.integer(hydroS$dt)

hymatchinds <-lapply(B1ints, FUN=function(x){
  return(which.min(abs(hyints-x)))
})


hy_src <- hydroS %>% rename(dt_src="dt", ws_2.50_src="ws_2.50", T_2.50_src="T_2.50", sr_src="sr") %>%
  select(dt_src, sr_src, ws_2.50_src, T_2.50_src)

indsvec <- rep(NA,length(hymatchinds))
for(i in 1:length(hymatchinds)){ indsvec[i] = hymatchinds[[i]]}

##sanity check to make sure that the matches are never off by more than 5mins
sum(abs(B1ints-hyints[indsvec])/60>5) 
#the biggest differnce between matches is 19mins and for now I can live with that

B1bind <- cbind(B1, hy_src[indsvec,]) #%>% 
  #select(-sr2, -Group.1, -solarWm2ReferenceValue)

#now all we are missing is some of those in-between indices
#so we look for gaps and if they are a certain size we fill them
#arrange in ascending order by dt beforehand just in case
B1bind <- B1bind[order(B1bind$dt),]
#here are the indices right before gaps in coverage that are larger than 20mins
gap_inds <- which(diff(B1bind$dt)>20)
#the first one is the gap between the 2022 and 2023 field season

#handling the three predictable gaps (beginnings and ends of seasons)
edgeshy_src<- hy_src %>% filter((dt_src>"2022-05-15 00:00:01 MST" & dt_src < B1bind$dt[1]-5) |
                                      (dt_src>B1bind$dt[gap_inds[2]+1]+5 & dt_src<"2022-10-01 00:00:01 MST") |
                                      (dt_src> B1bind$dt[length(B1bind$dt)] +5 & dt_src<"2023-10-01 00:00:01 MST"))

#handling the rest in a for loop
#make a list of conditions
txt <- "gapshy_src <- hy_src %>% filter(0==1" #putting a never met condition so that a | can come next
for(i in c(1, 3:length(gap_inds))){
  txt <- paste0(txt," | (dt_src > B1bind$dt[gap_inds[", i, "]]+5 & dt_src < B1bind$dt[gap_inds[", i, "]+1]-5)")
}
txt <- paste0(txt, ")")

eval(parse(text=txt)) #run that string as code

#add in modeled values to fill gaps in time
B1binds <- plyr::rbind.fill(B1bind, edgeshy_src, gapshy_src)


### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(B1binds, aes(x=T_2.50_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_2.50_src*year(dt), B1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

#plot and mod we used
ggplot(B1binds, aes(x=T_2.50_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
modTB1 <- lm(T_1.00~T_2.50_src, B1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(modTB1)

B1binds <- B1binds  %>% mutate(T_1.00pred = T_2.50_src*modTB1$coefficients[2] + modTB1$coefficients[1],
                                   T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                                   T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                                   dtuse = as.POSIXct(ifelse(is.na(dt), dt_src, dt), tz="MST")) 

#remove duplicates and one-offs
B1binds <- unique(B1binds)
B1binds <- B1binds[order(B1binds$dtuse),]
# is2remove <- c()
# for(i in 2:(length(B1binds$dtuse)-1)){
#   if(B1binds$T_1.00ori[i]=="modelled"){
#     if(B1binds$T_1.00ori[i-1]=="direct" & B1binds$T_1.00ori[i+1]=="direct"){
#       is2remove <- c(is2remove, i)
#     }
#   }
# }
# 
# B1binds <- B1binds[-is2remove,] 
##^ don't need to remove any

#THIS IS A TEST
ggplot(B1binds %>% filter(dtuse>"2022-06-22 00:00:01 MST" & dtuse<"2022-06-24 00:00:01 MST"), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_2.50_src),color="red", alpha=.5) + theme_bw() 


ggplot(B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use -- but note it's basically all direct and not modeled
ggplot(data= B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 B1 Air Temps")



#### Solar radiation ####
ggplot(B1binds %>% filter(year(dtuse)==2022), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
modsrB1 <- lm(sr~sr_src, B1binds %>% filter(year(dtuse)==2022))
summary(modsrB1)

B1binds <- B1binds  %>% mutate(srpred = sr_src*modsrB1$coefficients[2] + modsrB1$coefficients[1],
                               sruse = ifelse(year(dtuse)==2023 | is.na(sr), srpred, sr),
                               srori = ifelse(year(dtuse)==2023 | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 B1 Solar Radiation")



#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(B1binds %>% filter(year(dt)==2022)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_2.50_src), color="red")

#for now I'll fill in NAs with the average for that year actually
B1binds <- B1binds  %>% mutate(wspred = .5,
                               wsuse = .5,
                               wsori= "modelled") #could also create a column that's a measurement of error


#### Soil temperature ####
#soil_temperature() works with a time series, so I'm handling '22 and '23 separately
Bb22 <- B1binds %>% filter(year(dtuse)==2022)
Bb22$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Bb22$T_1.00use,
  u=Bb22$wsuse,
  Tsoil0=Bb22$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Bb22$dtuse),
  S=Bb22$sruse,
  water_content = 0.2, #.2 here
  air_pressure=mean(hydroS$`BAROMETRIC PRESS(MBAR)`)/10,
  rho_so = 1620,
  shade = TRUE
)

Bb23 <- B1binds %>% filter(year(dtuse)==2023)
Bb23$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Bb23$T_1.00use,
  u=Bb23$wsuse,
  Tsoil0=Bb23$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Bb23$dtuse),
  S=Bb23$sruse,
  water_content = 0.2, #.2 here
  air_pressure=mean(hydroS$`BAROMETRIC PRESS(MBAR)`)/10,
  rho_so = 1620,
  shade = TRUE
)

B1binds <- rbind(Bb22, Bb23)

##a demo
#ggplot(B1binds %>% filter(dtuse>"2023-06-18 19:31:08 MST" & dt<"2023-06-23 19:31:08 MST"), aes(x=dtuse)) + geom_line(aes(y=T_soil), color="darkgreen") + geom_line(aes(y=T_soilest), color="lightgreen") + geom_line(aes(y=T_1.00use), color="blue")
B1binds_old <- B1binds
B1binds <- B1binds %>% select(-sr2, -solarWm2ReferenceValue, -Group.1, -sr_src, -T_2.50_src, -ws_2.50_src)

B1binds$site <- "B1"

## C1/dlter ####

C1 <- all %>% filter(site=="C1")

#we lose functional lter data after this point on 9/3 2023
#NOTE: as-is this causes an issue with solar radiation after that time
#perhaps could find the relationship between 2023 sr and 2022 sr indirectly 
#(through lter sr relationship)
C1tomatch <- C1 %>% filter(dt<"2023-09-03 05:11:00 MST")
C1extras <- C1 %>% filter(dt>"2023-09-03 05:11:00 MST")

C1ints <- as.integer(C1tomatch$dt)
ltints <- as.integer(dlter$dt)

ltmatchinds <-lapply(C1ints, FUN=function(x){
  return(which.min(abs(ltints-x)))
})


#added T_soil thing... but could maybe also just remove that column
lter_src <- dlter %>% rename(dt_src="dt", ws_9_src="ws_9", T_1.7_src="T_1.7", sr_src="sr", T_soil_src="T_soil") 


indsvec <- rep(NA,length(ltmatchinds))
for(i in 1:length(ltmatchinds)){ indsvec[i] = ltmatchinds[[i]]}

##sanity check to make sure that the matches are never off by more than 5mins 
sum(abs(C1ints-ltints[indsvec])/60>5)

C1bind <- cbind(C1tomatch, lter_src[indsvec,]) %>%  #strange decimal rows i.e. x.1, maybe because lter_src is shorter
  select(-sr2, -Group.1, -solarWm2ReferenceValue)

#now all we are missing is some of those in-between indices
#so we look for gaps and if they are a certain size we fill them
#arrange in ascending order by dt beforehand just in case
C1bind <- C1bind[order(C1bind$dt),]
#here are the indices right before gaps in coverage that are larger than 20mins
gap_inds <- which(diff(C1bind$dt)>20)
#the first one is the gap between the 2022 and 2023 field season,
#the last several are on 8/03 where there's patchy cover

#handling the three predictable gaps (beginnings and ends of seasons)
edgeslter_src<- lter_src %>% filter((dt_src>"2022-05-15 00:00:01 MST" & dt_src < C1bind$dt[1]-5) |
                                      (dt_src>C1bind$dt[gap_inds[1]+1]+5 & dt_src<"2022-10-01 00:00:01 MST") |
                                      (dt_src> C1bind$dt[length(C1bind$dt)] +5 & dt_src<"2023-10-01 00:00:01 MST"))

#handling the rest in a for loop
#make a list of conditions
txt <- "gapslter_src <- lter_src %>% filter(0==1" #this condition is never met
for(i in 2:length(gap_inds)){
  txt <- paste0(txt," | (dt_src > C1bind$dt[gap_inds[", i, "]]+5 & dt_src < C1bind$dt[gap_inds[", i, "]+1]-5)")
}
txt <- paste0(txt, ")")

eval(parse(text=txt)) #run that string as code

#add in modeled values to fill gaps in time
C1binds <- plyr::rbind.fill(C1bind, C1extras, edgeslter_src, gapslter_src)


### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(C1binds, aes(x=T_1.7_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.7_src*year(dt), C1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

#plot and mod we used -- a tiny bit different / worse... due to using "date.time_end" rather than "date.time_start" as dt_src? 
ggplot(C1binds, aes(x=T_1.7_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
modTC1 <- lm(T_1.00~T_1.7_src, C1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(modTC1) 

C1binds <- C1binds  %>% mutate(T_1.00pred = T_1.7_src*modTC1$coefficients[2] + modTC1$coefficients[1],
                               T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                               T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                               dtuse = as.POSIXct(ifelse(is.na(dt), dt_src, dt), tz="MST")) #could also create a column that's a measurement of error


C1binds <- unique(C1binds)
# C1binds <- C1binds[order(C1binds$dtuse),]
#  is2remove <- c()
#  for(i in 2:(length(C1binds$dtuse)-1)){
#    if(C1binds$T_1.00ori[i]=="modelled"){
#      if(C1binds$T_1.00ori[i-1]=="direct" & C1binds$T_1.00ori[i+1]=="direct"){
#        is2remove <- c(is2remove, i)
#      }
#    }
#  }
# 
# C1binds <- C1binds[-is2remove,]
#^unnecessary

ggplot(C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use
ggplot(data= C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 C1 Air Temps")


#### Solar radiation ####
ggplot(C1binds %>% filter(year(dtuse)==2022), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
modsrC1 <- lm(sr~sr_src, C1binds %>% filter(year(dtuse)==2022))
summary(modsrC1)

C1binds <- C1binds  %>% mutate(srpred = sr_src*modsrC1$coefficients[2] + modsrC1$coefficients[1],
                               sruse = ifelse(year(dtuse)==2023 | is.na(sr), srpred, sr),
                               srori = ifelse(year(dtuse)==2023 | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 C1 Solar Radiation")

#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(C1binds %>% filter(year(dt)==2023)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_9_src), color="red")

#for now I'll fill in NAs with the average for that year actually
C1binds <- C1binds  %>% mutate(wspred = .5,
                               wsuse = .5,
                               wsori = "modelled") #could also create a column that's a measurement of error



Cb22 <- C1binds %>% filter(year(dtuse)==2022)
Cb22$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Cb22$T_1.00use,
  u=Cb22$wsuse,
  Tsoil0=Cb22$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Cb22$dtuse),
  S=Cb22$sruse,
  water_content = 0.2, #.2 here
  air_pressure=71,
  rho_so = 1620,
  shade = TRUE
)

Cb23 <- C1binds %>% filter(year(dtuse)==2023 & dtuse < "2023-09-03 05:11:00 MST")
Cb23$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Cb23$T_1.00use,
  u=Cb23$wsuse,
  Tsoil0=Cb23$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Cb23$dtuse),
  S=Cb23$sruse,
  water_content = 0.2, #.2 here
  air_pressure=71,
  rho_so = 1620,
  shade = TRUE
)

C1binds <- rbind(Cb22, Cb23)

C1binds_old <- C1binds

C1binds <- C1binds %>% select(-sr2, -solarWm2ReferenceValue, -Group.1, -ws_9_src, -T_1.7_src, -sr_src)

C1binds$site <- "C1"

## A1 2022 soil temperature ####

A1 <- all %>% filter(site=="A1") 

A1 <- A1 %>% mutate(dtuse=dt, sruse=sr, wsuse=ws, T_1.00use=T_1.00, 
                    srori = "direct", wsori = "direct", T_1.00ori = "direct")

A1$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=A1$T_1.00use,
  u=A1$wsuse,
  Tsoil0=A1$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(A1$dtuse),
  S=A1$sruse,
  water_content = 0.2, #.2 here
  air_pressure=mean(hydroS$`BAROMETRIC PRESS(MBAR)`)/10, #same as B1 for now
  rho_so = 1620,
  shade = TRUE
)

A1_old <- A1

A1 <- A1 %>% select(-sr2, -solarWm2ReferenceValue, -Group.1)



# BIND SITES ####
climateuse <- plyr::rbind.fill(Eldobinds, A1, B1binds, C1binds)

climateuse$T_soilori = "modelled"

## and save it as a csv and/or Rda ####
write.csv(climateuse, "climateuse.csv")

saveRDS(climateuse, "climateuse.rds")

## celebratory plot of all the data now without gaps ####
#(not showing what's infilled here but we'll zoom in later)

climateuse$dt_noyr <- climateuse$dtuse
year(climateuse$dt_noyr) <- 2024

coeff=30
ggplot(climateuse, aes(x=dt_noyr)) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=T_1.00use, color="Air temp")) + 
  geom_line(aes(y=T_soilest, color = "Soil temp")) +
  geom_line(aes(y=wsuse, color="Wind speed")) +
  geom_line(aes(y=sruse/coeff, color = "Solar rad")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C) / Wind speed (m/s)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Radiation (W/m2)")
  ) +
  theme(axis.title.y.right = element_text(color = "red")) +
  scale_color_manual(name='Climate vars',
                     breaks=c('Air temp', 'Soil temp', 'Wind speed', 'Solar rad'),
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'Air temp' = 'blue', 
                       'Soil temp'='brown', 
                       'Wind speed'='gray', 
                       'Solar rad'='red')) +
  facet_grid(site~year(dtuse)) 


# STRAY TESTS ####

#ggplot(data= C1bind %>% filter(dt>"2022-07-11 00:00:01 MST" & dt<"2022-07-12 00:00:01 MST")) + geom_line(aes(x=dt, y=sr), color="red") + geom_line(aes(x=dt_src, y=sr_src))+ theme_bw() + ggtitle("2023 C1 sr")
#solar_noon(lon=-105.55, doy=day_of_year("2022-07-11 00:00:01 MST"), offset=-6)