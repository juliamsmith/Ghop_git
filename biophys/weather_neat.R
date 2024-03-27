#Takes ~ 2hrs total to run :/

#library(plyr)
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
  #tempsite <- df %>% separate(V1, into=cols, sep=",")
  #morecols <- setdiff(allcols, cols)
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

WS$dt <- as.POSIXct(WS$dt, tz = "MST" )
WS$ws <- as.numeric(WS$ws)
WS$sr <- as.numeric(WS$sr)
WS$T_1.00 <- as.numeric(WS$T_1.00)
WS$T_soil <- as.numeric(WS$T_soil)


WS <- WS %>% mutate(dt=dt+hours(1), t=format(dt, "%I:%M:%S %p")) #also need to adjust for MST
WS <- WS %>% mutate(t=format(dt, "%I:%M:%S %p")) 


# Isolate usable WS data

#a first pass at getting rid of unusable data, there's prob still issues, see my notes
uWS <- WS %>% filter(ws>=0, T_1.00>-49.99, T_soil>-49.99, sr>=0, T_1.00<60) 
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
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
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
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'Air temp' = 'blue', 
                       'Soil temp'='brown', 
                       'Wind speed'='gray', 
                       'Solar rad'='red')) +
  facet_grid(site~year(dt)) 

#Getting surface roughness estimate from C1 2022
C1wsprofile <- read_csv("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022/C1_2022_surfroughness_mod.csv")
wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)
surf_r <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))


# READ DATA FROM OTHER SOURCES ####

## nrel data for Eldo (nrel) ISSUES? ####

### 2022 and 2023 #### 
nrel <- read.csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/nrel/nrel_eldo.csv")
nrel2 <- read.csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/nrel/nrel_eldo2.csv")
nrel <- rbind(nrel, nrel2)
nrel <- distinct(nrel)
#usEldo <- all %>% filter(site=="Eldo", year(dt)==2023)
thing <- c()
for(i in 1:72719) { #Have to re-think these #s 72864
  thing <- c(thing, rep(i, 10))
}
thing <- c(thing, 72720) #72865 rep(, 1)
nrel$group <- thing
nrel_less <- nrel %>% 
  group_by(group) %>% 
  summarize(dt=mean(as.POSIXct(paste(DATE..MM.DD.YYYY., MST), format="%m/%d/%Y %H:%M", tz="MST")), 
            sr=mean(Global.Horizontal..W.m.2.), 
            T_2.00 = mean(Temperature...2m..deg.C.),
            WS_2.00=mean(Avg.Wind.Speed...2m..m.s.), surf=mean(Est.Surface.Roughness..m.))


nrel <- nrel_less %>% filter(dt>as.Date("5/15/2022 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2022 12:21", format="%m/%d/%Y %H:%M") | 
                               (dt>as.Date("5/15/2023 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2023 12:21", format="%m/%d/%Y %H:%M")))

### Adjustments ####
nrel$dt <- nrel$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary

#ws profile down to 1m (note -- might like to use surf, but not cooperating)
nrel <- nrel %>% mutate(ws=wind_speed_profile_neutral(u_r=WS_2.00, zr=2, z0=surf_r, z=1)) 

#air temp profile down to 1m (note -- might like to use surf, but not cooperating)
nrel <- nrel %>% mutate(T_1.00=air_temp_profile(T_r=T_2.00, u_r=WS_2.00, zr=2, z0=surf_r, z=1, T_s=T_2.00)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m


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
                            WS_2.50=`WINDSPEED(m/s)-2.5M(AVG)`,
                            sr=`IN SW RAD(W/m^2)-2.5M(AVG)`)

hydroS <- hydroS[complete.cases(hydroS[ ,17:20]), ]
### Adjustments ####

hydroS$dt <- hydroS$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary

#ws profile down to 1m
hydroS <- hydroS %>% mutate(ws=wind_speed_profile_neutral(u_r=WS_2.50, zr=2.5, z0=surf_r, z=1)) 

#air temp profile down to 1m
hydroS <- hydroS %>% mutate(T_1.00=air_temp_profile(T_r=T_2.50, u_r=WS_2.50, zr=2.5, z0=surf_r, z=1, T_s=T_2.50)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m


## Niwot LTER data for C1 (dlter) ####


### 2022 ####

C1_niwot <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/c-1cr23x-cr1000.10minute.ml.data.csv")
#note -- changed from date.time_start to date.time_end
C1_niwot_win <- C1_niwot %>% filter((as.Date(date.time_end, format="%m/%d/%Y %H:%M")>as.Date("5/15/2022 16:30", format="%m/%d/%Y %H:%M") & as.Date(date.time_end, format="%m/%d/%Y %H:%M")<as.Date("10/01/2022 12:21", format="%m/%d/%Y %H:%M")) | 
                                      (as.Date(date.time_end, format="%m/%d/%Y %H:%M")>as.Date("5/15/2023 16:30", format="%m/%d/%Y %H:%M") & as.Date(date.time_end, format="%m/%d/%Y %H:%M")<as.Date("10/01/2023 12:21", format="%m/%d/%Y %H:%M")))

C1_niwot_win_df <- as.data.frame(C1_niwot_win)

C1_niwot_win_df$dt <- as.POSIXct(C1_niwot_win_df$date.time_end, format="%m/%d/%Y %H:%M", tz = "MST") 

### 2023 ####

library(RCurl)

url <- "ftp://niwotlter.colorado.edu/raw_climate_data/"
filenames <- getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
filenames <- strsplit(filenames, "\r\n")
filenames <- unlist(filenames)
filenames


# download.file(paste(url, "c1_cr1000_tenminute_20221031_20230903.backup", sep=""), paste("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "c1_cr1000_tenminute_20221031_20230903.backup", sep=""))
# 
# download.file(paste(url, "c1_cr1000x_tenminute.dat", sep=""), paste(""G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "c1_cr1000x_tenminute.dat", sep=""))
# 
# download.file(paste(url, "ReadMe.txt", sep=""), paste("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/", "ReadMe.txt", sep=""))


dat <- as.data.frame(read.delim("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/lter/c1_cr1000_tenminute_20221031_20230903.backup", sep=" "))

dat2 <- dat[4:dim(dat)[1],]

ns <- read.table(text = dat[1,], sep = ",", fill = TRUE, as.is = TRUE)


d <- read.table(text = dat2, sep = ",", fill = TRUE, as.is = TRUE)

d2 <- as.data.frame(d)

colnames(d2) <- ns

#now d2 is what we want!

d2$TIMESTAMP <- as.POSIXct(d2$TIMESTAMP, tz = "MST" )

### Bind 2022 and 2023 lter together ####
d2023 <- d2 %>% 
  select(TIMESTAMP, AirTC_1_Avg, WS_ms_Avg, SlrW_Avg, T107_C_Avg) %>% 
  rename(dt="TIMESTAMP", T_1.7="AirTC_1_Avg", wshigh="WS_ms_Avg", sr="SlrW_Avg", T_soil="T107_C_Avg")
d2022 <- C1_niwot_win_df %>% 
  select(dt,airtemp_as_1.7_avg, ws_avg, solrad_avg, soiltemp_5cm_avg) %>%
  rename(T_1.7="airtemp_as_1.7_avg", wshigh="ws_avg", sr="solrad_avg", T_soil="soiltemp_5cm_avg")

dlter = rbind(d2022, d2023)  

### Adjustments ####

dlter$dt <- dlter$dt + 60*60 #TIMEZONE not sure why, but it seems this correction may be necessary


dlter <- dlter[-34674,] #WHY?

#ws profile down to 1.7m and 1m (note: zr=7m is a guess)
dlter <- dlter %>% mutate(ws_1.7=wind_speed_profile_neutral(u_r=wshigh, zr=7, z0=surf_r, z=1.7),
                          ws=wind_speed_profile_neutral(u_r=wshigh, zr=7, z0=surf_r, z=1))

#air temp profile down to 1m
dlter <- dlter %>% mutate(T_1.00=air_temp_profile(T_r=T_1.7, u_r=ws_1.7, zr=1.7, z0=surf_r, z=1, T_s=T_1.7)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m



#idea -- is it desirable to merge all the data from other sources together first? I don't really think it's necessary... could do for the sake of viz

# MERGE DATA AND MODEL ####

## Eldo/nrel ####

### 2022 ####
nrel2022 <- nrel %>% filter(year(dt)==2022)
usEldo <- all %>% filter(site=="Eldo", year(dt)==2022)

tempus <- usEldo
tempnr <- nrel2022[1353:length(nrel2022$dt),]

nrextras22 <- tempnr[1:1352,]

i_offset=0
for(i in 2:33806) { #8000
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-tempnr$dt[i]
  diff_lag <- tempus$dt[i]-tempnr$dt[i+1]
  diff_lead <- tempus$dt[i]-tempnr$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    nrextras22 <- rbind(nrextras22, tempnr[i,])
    tempnr <- tempnr[-i,]
    i_offset=i_offset+1
    print("lag")
    nrextras22 <- rbind(nrextras22, tempnr[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}
#nr extras should be different
nrextras22 <- rbind(nrextras22, tempnr[16905:length(tempnr$dt),])

t <- tempnr %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") 

nrextras22 <- nrextras22 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr")


Eldobind22 <- cbind(tempus, t[1:16904,]) %>% 
  select(-sr2, -Group.1, -solarWm2ReferenceValue, -group, -surf, -T_2.00, -WS_2.00)

Eldobind22 <- plyr::rbind.fill(Eldobind22, nrextras22)

### 2023 ####
nrel2023 <- nrel %>% filter(year(dt)==2023)
usEldo <- all %>% filter(site=="Eldo", year(dt)==2023)

tempus <- usEldo
tempnr <- nrel2023[4208:length(nrel2023$dt),] #strange that this didn't change when I added an hour

nrextras23 <- nrel2023[1:4207,]

i_offset=0
for(i in 2:9705) { #8000 #3273
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-tempnr$dt[i]
  diff_lag <- tempus$dt[i]-tempnr$dt[i+1]
  diff_lead <- tempus$dt[i]-tempnr$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    nrextras23 <- rbind(nrextras23, tempnr[i,])
    tempnr <- tempnr[-i,]
    i_offset=i_offset+1
    print("lag")
    nrextras23 <- rbind(nrextras23, tempnr[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}



#I think they might be matched up to each other 3273 rows


nrextras23 <- rbind(nrextras23, tempnr[3274:length(tempnr$dt),])

t <- tempnr %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr")

nrextras23 <- nrextras23 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr")


Eldobind23 <- cbind(tempus, t[1:3273,]) %>% 
  select(-sr2, -Group.1, -solarWm2ReferenceValue, -group, -surf, -T_2.00, -WS_2.00)

Eldobind23 <- plyr::rbind.fill(Eldobind23, nrextras23)

### Bind 2022 and 2023 nrel together ####

Eldobinds <- rbind(Eldobind22, Eldobind23)

### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(Eldobinds, aes(x=T_1.00_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src*year(dt), Eldobinds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

#plot and mod we used
ggplot(Eldobinds, aes(x=T_1.00_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src, Eldobinds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

Eldobinds <- Eldobinds  %>% mutate(T_1.00pred = T_1.00_src*mod$coefficients[2] + mod$coefficients[1],
                                         T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                                         T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                                         dtuse = as.POSIXct(ifelse(is.na(T_1.00), dt_src, dt), tz="MST")) #could also create a column that's a measurement of error


Eldobinds <- unique(Eldobinds)
Eldobinds <- Eldobinds[order(Eldobinds$dtuse),]
is2remove <- c()
for(i in 2:(length(Eldobinds$dtuse)-1)){
  if(Eldobinds$T_1.00ori[i]=="modelled"){
    if(Eldobinds$T_1.00ori[i-1]=="direct" & Eldobinds$T_1.00ori[i+1]=="direct"){
      is2remove <- c(is2remove, i)
    }
  }
}

Eldobinds <- Eldobinds[-is2remove,] 

ggplot(Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use
ggplot(data= Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 Eldo Air Temps")



#### Solar radiation ####
ggplot(Eldobinds %>% filter(dtuse<="2022-08-28 19:31:08 MST"), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
mod <- lm(sr~sr_src, Eldobinds %>% filter(dtuse<="2022-08-28 19:31:08 MST"))
summary(mod)

Eldobinds <- Eldobinds  %>% mutate(srpred = sr_src*mod$coefficients[2] + mod$coefficients[1],
                                   sruse = ifelse(dtuse>="2022-08-28 19:31:08 MST" | is.na(sr), srpred, sr),
                                   srori = ifelse(dtuse>="2022-08-28 19:31:08 MST" | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= Eldobinds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 Eldo Solar Radiation")

#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(Eldobinds %>% filter(year(dt)==2023)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_src), color="red")

#for now I'll fill in NAs with the average for that year actually
Eldobinds <- Eldobinds  %>% mutate(wspred = ifelse(year(dtuse)==2022, mean((Eldobinds %>% filter(year(dtuse)==2022))$ws, na.rm=TRUE), mean((Eldobinds %>% filter(year(dtuse)==2023))$ws, na.rm=TRUE)),
                                   wsuse = ifelse(is.na(ws), wspred, ws),
                                   wsori = ifelse(is.na(ws),"modelled", "direct")) #could also create a column that's a measurement of error

#### Soil temperature ####
# Eldobinds$T_soilest <- soil_temperature(
#   z_r.intervals = 12,
#   z_r=1,
#   z=2, #what do the intervals mean?
#   T_a=Eldobinds$T_1.00use,
#   u=Eldobinds$wsuse,
#   Tsoil0=25, # have to create this as a var... or just make up a #
#   z0=surf_r,
#   SSA=.7, #guess
#   TimeIn=hour(Eldobinds$dtuse),
#   S=Eldobinds$sruse,
#   water_content = 0.2, #.2 here
#   air_pressure=80.8,
#   rho_so = 1620,
#   shade = TRUE
# )

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

Eldobinds <- Eldobinds %>% select(-WS_2.00, -group, -T_2.00, -surf)

#add site column before merging 
Eldobinds$site <- "Eldo"


## B1/hydroshare ####

### 2022 ####

hydroS22 <- hydroS %>% filter(year(dt)==2022)
usB1 <- all %>% filter(site=="B1", year(dt)==2022)

tempus <- usB1
temphydro <- hydroS22[21953:length(hydroS22$dt),]

hyextras22 <- hydroS22[19506:21952,]

i_offset=0
for(i in 2:32048) { 
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-temphydro$dt[i]
  diff_lag <- tempus$dt[i]-temphydro$dt[i+1]
  diff_lead <- tempus$dt[i]-temphydro$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    hyextras22 <- rbind(hyextras22, temphydro[i,])
    temphydro <- temphydro[-i,]
    i_offset=i_offset+1
    print("lag")
    hyextras22 <- rbind(hyextras22, temphydro[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}


#hyextras22 <- rbind(hyextras22, temphydro[16010:length(temphydro$dt),]) #prob unnecessary

t <- temphydro %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)

hyextras22 <- hyextras22 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)


#issue with amount... trace bqck by looking at templt and tempus
B1bind22 <- cbind(tempus, t[1:16010,]) 

B1bind22 <- plyr::rbind.fill(B1bind22, hyextras22)

### 2023 ####

hydroS23 <- hydroS %>% filter(year(dt)==2023)
usB1 <- all %>% filter(site=="B1", year(dt)==2023)

tempus <- usB1[20:length(usB1$dt),] #NOTE -- look out for beginnings and ends
temphydro <- hydroS23[23386:length(hydroS23$dt),]

hyextras23 <- hydroS23[19506:23386,]

i_offset=0
for(i in 2:14050) { 
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-temphydro$dt[i]
  diff_lag <- tempus$dt[i]-temphydro$dt[i+1]
  diff_lead <- tempus$dt[i]-temphydro$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    hyextras23 <- rbind(hyextras23, temphydro[i,])
    temphydro <- temphydro[-i,]
    i_offset=i_offset+1
    print("lag")
    hyextras23 <- rbind(hyextras23, temphydro[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}


#hyextras23 <- rbind(hyextras23, temphydro[16010:length(temphydro$dt),]) #prob unnecessary

t <- temphydro %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)

hyextras23 <- hyextras23 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)


#issue with amount... trace bqck by looking at templt and tempus
B1bind23 <- cbind(tempus, t[1:11476,]) #check this #

B1bind23 <- plyr::rbind.fill(B1bind23, hyextras23)

### Bind 2022 and 2023 together ####

B1binds <- rbind(B1bind22, B1bind23)

### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(B1binds, aes(x=T_1.00_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src*year(dt), B1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

#plot and mod we used
ggplot(B1binds, aes(x=T_1.00_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src, B1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

B1binds <- B1binds  %>% mutate(T_1.00pred = T_1.00_src*mod$coefficients[2] + mod$coefficients[1],
                                   T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                                   T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                                   dtuse = as.POSIXct(ifelse(is.na(dt), dt_src, dt), tz="MST")) #could also create a column that's a measurement of error

#remove duplicates and one-offs
B1binds <- unique(B1binds)
B1binds <- B1binds[order(B1binds$dtuse),]
is2remove <- c()
for(i in 2:(length(B1binds$dtuse)-1)){
  if(B1binds$T_1.00ori[i]=="modelled"){
    if(B1binds$T_1.00ori[i-1]=="direct" & B1binds$T_1.00ori[i+1]=="direct"){
      is2remove <- c(is2remove, i)
    }
  }
}

B1binds <- B1binds[-is2remove,] 

ggplot(B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use -- but note it's basically all direct and not modeled
ggplot(data= B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 B1 Air Temps")



#### Solar radiation ####
ggplot(B1binds %>% filter(year(dtuse)==2022), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
mod <- lm(sr~sr_src, B1binds %>% filter(year(dtuse)==2022))
summary(mod)

B1binds <- B1binds  %>% mutate(srpred = sr_src*mod$coefficients[2] + mod$coefficients[1],
                               sruse = ifelse(year(dtuse)==2023 | is.na(sr), srpred, sr),
                               srori = ifelse(year(dtuse)==2023 | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= B1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 B1 Solar Radiation")



#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(B1binds %>% filter(year(dt)==2022)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_src), color="red")

#for now I'll fill in NAs with the average for that year actually
B1binds <- B1binds  %>% mutate(wspred = ifelse(year(dtuse)==2022, mean((B1binds %>% filter(year(dtuse)==2022))$ws, na.rm=TRUE), mean((B1binds %>% filter(year(dtuse)==2023))$ws, na.rm=TRUE)),
                               wsuse = ifelse(is.na(ws), wspred, ws),
                               wsori= ifelse(is.na(ws), "modelled", "direct")) #could also create a column that's a measurement of error


#### Soil temperature ####

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

B1binds <- B1binds %>% select(-sr2, -solarWm2ReferenceValue, -Group.1)

B1binds$site <- "B1"

## C1/dlter ####

### 2022 ####

dlter22 <- dlter %>% filter(year(dt)==2022)
usC1 <- all %>% filter(site=="C1", year(dt)==2022)

tempus <- usC1
templt <- dlter22 #[21953:length(dlter22$dt),]

ltextras22 <- dlter22[1,]

i_offset=0
for(i in 2:33579) { 
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-templt$dt[i]
  diff_lag <- tempus$dt[i]-templt$dt[i+1]
  diff_lead <- tempus$dt[i]-templt$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    ltextras22 <- rbind(ltextras22, templt[i,])
    templt <- templt[-i,]
    i_offset=i_offset+1
    print("lag")
    ltextras22 <- rbind(ltextras22, templt[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}


#ltextras22 <- rbind(ltextras22, templt[16010:length(templt$dt),]) #prob unnecessary

t <- templt %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)

ltextras22 <- ltextras22 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)


#issue with amount... trace bqck by looking at templt and tempus
C1bind22 <- cbind(tempus, t[1:15096,]) 

C1bind22 <- plyr::rbind.fill(C1bind22, ltextras22)

### 2023 ####

dlter23 <- dlter %>% filter(year(dt)==2023)
usC1 <- all %>% filter(site=="C1", year(dt)==2023)

tempus <- usC1
templt <- dlter23[23535:35339,]

ltextras23 <- dlter23[19352:23534,] #FOUND THE MISTAKE maybe templt or maybe start earlier

i_offset=0
for(i in 2:11806) { 
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-templt$dt[i]
  diff_lag <- tempus$dt[i]-templt$dt[i+1]
  diff_lead <- tempus$dt[i]-templt$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    ltextras23 <- rbind(ltextras23, templt[i,])
    templt <- templt[-i,]
    i_offset=i_offset+1
    print("lag")
    ltextras23 <- rbind(ltextras23, templt[i,])
  }
  if(abs(diff_lead)<abs(diff) & abs(diff_lead)<abs(diff_lag)) {
    #print("lead") # I think we never used... probably doesn't work
    #break
    tempus <- tempus[-i,]
    i_offset=i_offset+1
    print("lead")
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}


#ltextras23 <- rbind(ltextras23, templt[16010:length(templt$dt),]) #prob unnecessary

t <- templt %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)

ltextras23 <- ltextras23 %>% rename(dt_src="dt", ws_src="ws", T_1.00_src="T_1.00", sr_src="sr") %>%
  select(dt_src, sr_src, ws_src, T_1.00_src)

C1bind23 <- cbind(tempus[1:7443,], t)

C1bind23 <- plyr::rbind.fill(C1bind23, ltextras23)

### Bind 2022 and 2023 together ####

C1binds <- rbind(C1bind22, C1bind23)

### Adding in the predictions/gap-filling ####

#### Air Temperature ####

#seeing if/how year matters
ggplot(C1binds, aes(x=T_1.00_src, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src*year(dt), C1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)

#plot and mod we used -- a tiny bit different / worse... due to using "date.time_end" rather than "date.time_start" as dt_src? 
ggplot(C1binds, aes(x=T_1.00_src, y=T_1.00)) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_src, C1binds) #could have *year(dt) but excluding it bc doesn't make a big diff
summary(mod)


C1binds <- C1binds  %>% mutate(T_1.00pred = T_1.00_src*mod$coefficients[2] + mod$coefficients[1],
                               T_1.00use = ifelse(is.na(T_1.00), T_1.00pred, T_1.00),
                               T_1.00ori = ifelse(is.na(T_1.00), "modelled", "direct"),
                               dtuse = as.POSIXct(ifelse(is.na(dt), dt_src, dt), tz="MST")) #could also create a column that's a measurement of error


C1binds <- unique(C1binds)
C1binds <- C1binds[order(C1binds$dtuse),]
 is2remove <- c()
 for(i in 2:(length(C1binds$dtuse)-1)){
   if(C1binds$T_1.00ori[i]=="modelled"){
     if(C1binds$T_1.00ori[i-1]=="direct" & C1binds$T_1.00ori[i+1]=="direct"){
       is2remove <- c(is2remove, i)
     }
   }
 }

C1binds <- C1binds[-is2remove,]

ggplot(C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse)) + geom_line(aes(y=T_1.00), alpha=.5) + geom_line(aes(y=T_1.00pred),color="red", alpha=.5) + theme_bw() 

#what we are going to use
ggplot(data= C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, color=T_1.00ori, y=T_1.00use)) + geom_line(aes(group=1)) + theme_bw() + ggtitle("2023 C1 Air Temps")


#### Solar radiation ####
ggplot(C1binds %>% filter(year(dtuse)==2022), aes(x=sr_src, y=sr)) + geom_point() + geom_smooth(method="lm")
mod <- lm(sr~sr_src, C1binds %>% filter(year(dtuse)==2022))
summary(mod)

C1binds <- C1binds  %>% mutate(srpred = sr_src*mod$coefficients[2] + mod$coefficients[1],
                               sruse = ifelse(year(dtuse)==2023 | is.na(sr), srpred, sr),
                               srori = ifelse(year(dtuse)==2023 | is.na(sr), "modelled", "direct")) #could also create a column that's a measurement of error

#what we are going to use
ggplot(data= C1binds %>% filter(year(dtuse)==2023), aes(x=dtuse, y=srpred)) + geom_line(color="blue") + theme_bw() + ggtitle("2023 C1 Solar Radiation")

#### Wind speed ####

#See how similar/different the wind speeds are
ggplot(C1binds %>% filter(year(dt)==2023)) + geom_density(aes(x=ws)) + geom_density(aes(x=ws_src), color="red")

#for now I'll fill in NAs with the average for that year actually
C1binds <- C1binds  %>% mutate(wspred = ifelse(year(dtuse)==2022, mean((C1binds %>% filter(year(dtuse)==2022))$ws, na.rm=TRUE), mean((C1binds %>% filter(year(dtuse)==2023))$ws, na.rm=TRUE)),
                               wsuse = ifelse(is.na(ws), wspred, ws),
                               wsori = ifelse(is.na(ws), "modelled", "direct")) #could also create a column that's a measurement of error



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

Cb23 <- C1binds %>% filter(year(dtuse)==2023)
Cb23$T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=Cb23$T_1.00use,
  u=Cb23$wsuse,
  Tsoil0=Cb23$T_1.00use[1], # have to create this as a var... or just make up a #
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(Bb23$dtuse),
  S=Cb23$sruse,
  water_content = 0.2, #.2 here
  air_pressure=71,
  rho_so = 1620,
  shade = TRUE
)

C1binds <- rbind(Cb22, Cb23)


C1binds <- C1binds %>% select(-sr2, -solarWm2ReferenceValue, -Group.1)

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