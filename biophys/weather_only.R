#read in all old data

#library(plyr)
library(tidyverse)
library(gsubfn)
library(lubridate)
library(TrenchR)
library(ggnewscale)
library(tibbletime)

setwd("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022")

### Read in and process weather station data (dataframe WS) ####

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


### Isolate usable WS data

#a first pass at getting rid of unusable data, there's prob still issues, see my notes
uWS <- WS %>% filter(ws>=0, T_1.00>-49.99, T_soil>-49.99, sr>=0, T_1.00<60) 
uWS <- unique(uWS) #something happened causing there to be lots of duplicates!

uWS <- uWS %>% mutate(date=format(dt,"%Y-%m-%d"))
uWS$date <- as.POSIXct(uWS$date)

setwd("C:/Users/smith/Desktop/Ghop_git")

#read in new WS data (from 2023)
WS23 <- readRDS("biophys/all_WS_dat_excA1.RDS")



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

C1_niwot <- read_csv("biophys/c-1cr23x-cr1000.10minute.ml.data.csv")

C1_niwot_win <- C1_niwot %>% filter((as.Date(date.time_start, format="%m/%d/%Y %H:%M")>as.Date("5/15/2022 16:30", format="%m/%d/%Y %H:%M") & as.Date(date.time_start, format="%m/%d/%Y %H:%M")<as.Date("10/01/2022 12:21", format="%m/%d/%Y %H:%M")) | 
                                      (as.Date(date.time_start, format="%m/%d/%Y %H:%M")>as.Date("5/15/2023 16:30", format="%m/%d/%Y %H:%M") & as.Date(date.time_start, format="%m/%d/%Y %H:%M")<as.Date("10/01/2023 12:21", format="%m/%d/%Y %H:%M")))

C1_niwot_win_df <- as.data.frame(C1_niwot_win)

C1_niwot_win_df$dt <- as.POSIXct(C1_niwot_win_df$date.time_start, format="%m/%d/%Y %H:%M", tz = "MST") 

ggplot(C1_niwot_win_df, aes(x=dt, format="%m/%d/%Y %H:%M")) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=airtemp_hmp1_avg, color="Air temp")) + #other options: other hmps, airtemp_as_1.7_avg
  geom_line(aes(y=soiltemp_5cm_avg, color = "Soil temp")) +
  geom_line(aes(y=ws_avg, color="Wind speed")) +
  geom_line(aes(y=solrad_avg/coeff, color = "Solar rad")) +
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
                       'Solar rad'='red')) #+
#  facet_grid(year(dt)) 

#aha only have data until 9/30 2022 -- not great! FTP-ing other data
library(RCurl)

url <- "ftp://niwotlter.colorado.edu/raw_climate_data/"
filenames <- getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
filenames <- strsplit(filenames, "\r\n")
filenames <- unlist(filenames)
filenames


#download.file(paste(url, "c1_cr1000_tenminute_20221031_20230903.backup", sep=""), paste(getwd(), "/", "c1_cr1000_tenminute_20221031_20230903.backup", sep=""))

#download.file(paste(url, "c1_cr1000x_tenminute.dat", sep=""), paste(getwd(), "/", "c1_cr1000x_tenminute.dat", sep=""))

#download.file(paste(url, "ReadMe.txt", sep=""), paste(getwd(), "/", "ReadMe.txt", sep=""))


dat <- as.data.frame(read.delim("c1_cr1000_tenminute_20221031_20230903.backup", sep=" "))

dat2 <- dat[4:dim(dat)[1],]

ns <- read.table(text = dat[1,], sep = ",", fill = TRUE, as.is = TRUE)


d <- read.table(text = dat2, sep = ",", fill = TRUE, as.is = TRUE)

d2 <- as.data.frame(d)

colnames(d2) <- ns

#now d2 is what we want! Not sure if there is a soil temp tho!
#what variables mean is a little confusing

d2$TIMESTAMP <- as.POSIXct(d2$TIMESTAMP, tz = "MST" )

ggplot(d2, aes(x=TIMESTAMP)) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=AirTC_1_Avg, color="TC 1")) + #other options: other hmps, airtemp_as_1.7_avg
  geom_line(aes(y=T107_C_Avg, color="107")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C) / Wind speed (m/s)") +
  scale_color_manual(name='Climate vars',
                     breaks=c('TC 1', '107'),
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'TC 1' = 'blue', 
                       '107'="pink")) #+
#  facet_grid(year(dt)) 


#let's bind the old to the new niwot climate data
d2023 <- d2 %>% 
  select(TIMESTAMP, AirTC_1_Avg, WS_ms_Avg, SlrW_Avg, T107_C_Avg) %>% 
  rename(dt="TIMESTAMP", T_1.7="AirTC_1_Avg", ws="WS_ms_Avg", sr="SlrW_Avg", T_soil="T107_C_Avg")
d2022 <- C1_niwot_win_df %>% 
  select(dt,airtemp_as_1.7_avg, ws_avg, solrad_avg, soiltemp_5cm_avg) %>%
  rename(T_1.7="airtemp_as_1.7_avg", ws="ws_avg", sr="solrad_avg", T_soil="soiltemp_5cm_avg")

dlter = rbind(d2022, d2023)  


#small intermission
C12022ourtemps <- all %>% filter(site=="C1", year(dt)==2022) %>% select(dt, T_1.00)
C12022ltertemps <- C1_niwot_win_df %>% select(date.time_start, date.time_end, airtemp_as_1.7_avg, airtemp_hmp1_avg, airtemp_hmp2_avg, airtemp_hmp3_avg)
C12022ltertemps$dt <- as.POSIXct(C12022ltertemps$date.time_start, format="%m/%d/%Y %H:%M", tz="MST")

C1_airtemp_compare <- full_join(C12022ourtemps, C12022ltertemps, by = "dt")

ggplot(C1_airtemp_compare, aes(x=dt, format="%m/%d/%Y %H:%M")) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=T_1.00, color="ours")) + #other options: other hmps, airtemp_as_1.7_avg
  geom_line(aes(y=airtemp_as_1.7_avg, color = "as1.7")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Air temperature (C)") +
  scale_color_manual(name='Climate vars',
                     breaks=c('ours', 'as1.7'),
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'ours' = 'blue', 
                       'as1.7' = 'brown')) #+
#  facet_grid(year(dt)) 


C1_airtemp_compare_t <- C1_airtemp_compare %>% gather("clim_var", "value", -dt, -date.time_start, -date.time_end) 
#project it down to 1.0 using this function https://trenchproject.github.io/TrenchR/reference/air_temp_profile.html


#more intermission
oursr22 <- all %>% filter(site=="C1", year(dt)==2022) %>% select(dt, sr) 
ltsr22 <- dlter %>% filter(dt>oursr22$dt[1] & dt<oursr22$dt[length(oursr22$dt)]) %>% select(dt, sr)

ggplot() +
  geom_line(data=oursr22 %>% filter(dt > "2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="blue") +
  geom_line(data=ltsr22%>% filter(dt > "2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="lightgreen") 


#resume
dourC1 <- all %>% filter(site=="C1") %>% select(-site, -T_1.25, -T_0.75, -T_0.25, -T_0.50, -dt_noyr)
dourC1$src <- "Buckley"

dlter$src <- "Morse"


library(TrenchR)
#read

C1wsprofile <- read_csv("G:/Shared drives/RoL_FitnessConstraints/data/Transplant2022/C1_2022_surfroughness_mod.csv")

wslow <- mean(C1wsprofile$windspeed1)
wsmed <- mean(C1wsprofile$windspeed2)
wshi <- mean(C1wsprofile$windspeed3)

surf_r <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))


dlter <- dlter[-34674,]

dlter <- dlter %>% mutate(ws_1.7=wind_speed_profile_neutral(u_r=ws, zr=7, z0=surf_r, z=1.7))

#now for air temp
dlter <- dlter %>% mutate(T_1.00=air_temp_profile(T_r=T_1.7, u_r=ws_1.7, zr=1.7, z0=surf_r, z=1, T_s=T_1.7)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m

lt2023 <- dlter %>% filter(year(dt)==2023)
lt2023 <- lt2023[23535:35339,]

us2023 <- all %>% filter(site=="C1", year(dt)==2023)

# #probably too slow -- takes 30mins to run as is (did not finish in that time)
# mrg2023 <- nearestTime(us2023, lt2023, "dt", "dt")
# 
# 
# 
# for(i in 1:dim(lt2023)[1]){
#   obs <- final_obs_spp[i,]
#   #allweather <- as.data.frame(allweather)
#   s <- obs$Site
#   aw_new <- WS %>% filter(site==s)
#   closestind <- which.min(abs(difftime(aw_new$dt, obs$dt)))
#   nearest_WS_time <- aw_new[closestind, "dt"]
#   final_obs_spp$wdt[i] <- nearest_WS_time
# }

#matching up from 2022
lt2022 <- dlter %>% filter(year(dt)==2022)
us2022 <- all %>% filter(site=="C1", year(dt)==2022)
templt <- lt2022#[1:(27965-(35339-11805)+100),]
tempus <- us2022#[1:4300,]
i_offset=0
for(i in 2:100000) {
  i=i-i_offset
  diff <- tempus$dt[i]-templt$dt[i]
  diff_lag <- tempus$dt[i]-templt$dt[i+1]
  diff_lead <- tempus$dt[i]-templt$dt[i-1]
  if(abs(diff_lag)<abs(diff)) {
    templt <- templt[-i,]
    i_offset=i_offset+1 #added this in because I think it should be there
  }
  if(abs(diff_lead)<abs(diff)) {
    tempus <- tempus[-i,]
    i_offset=i_offset+1
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}

t <- templt %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")
C1bind2 <- cbind(tempus, t[1:15096,])
ggplot(C1bind2, aes(x=sr_lt, y=sr)) + geom_point() + geom_smooth(method="lm")
mod <- lm(sr~sr_lt, C1bind2)
summary(mod) #.6073 R2

ggplot(C1bind2, aes(x=T_1.00_lt, y=T_1.00)) + geom_point() + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_lt, C1bind2)
summary(mod) #.8763 R2

ggplot(C1bind2 %>%  filter(dt > "2022-07-09 19:31:08 MST" & dt < "2022-07-10 19:31:08 MST")) + 
  geom_line(aes(x=dt, y=sr_lt), color="red") + 
  geom_line(aes(x= dt-60*60, y=sr), color="darkred")

templt <- lt2023#[1:(27965-(35339-11805)+100),]
tempus <- us2023#[1:4300,]
i_offset=0
for(i in 2:100000) {
  i=i-i_offset
  diff <- tempus$dt[i]-templt$dt[i]
  diff_lag <- tempus$dt[i]-templt$dt[i+1]
  diff_lead <- tempus$dt[i]-templt$dt[i-1]
  if(abs(diff_lag)<abs(diff)) {
    templt <- templt[-i,]
    i_offset=i_offset+1 #added this in because I think it should be there
  }
  if(abs(diff_lead)<abs(diff)) {
    tempus <- tempus[-i,]
    i_offset=i_offset+1
  }
  # print(length(templt$dt))
  # print(length(tempus$dt))
  print("i")
  print(i)
  print("diff")
  print(abs(diff))
}

t <- templt %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")
C1bind <- cbind(tempus[1:7443,], t)


#showing sr
#get zenith of the sun
C1bind <- C1bind %>% mutate(zenith=zenith_angle(doy=day_of_year(dt), lat=40.03, lon=-105.55, hour=hour(dt)))
C1bind$time <- format(C1bind$dt, format = "%H:%M")
C1bind <- C1bind %>% mutate(sr_eq=direct_solar_radiation(lat=40.03, doy=day_of_year(dt), elev=3014, t=hour(dt), t0=solar_noon(lon=-105.55, doy=day_of_year(dt))))
ggplot(C1bind, aes(x=sr_lt, y=sr)) + geom_point()
mod <- lm(sr~sr_lt, C1bind)
summary(mod) #.6892 R2

ggplot(C1bind, aes(zenith,sr)) + geom_point()
ggplot(C1bind, aes(hour(dt),sr)) + geom_point() + geom_smooth() #probably not a good sign? 
#why are there times in the middle of the day when it's like 0?


C1day <- C1bind %>% filter(hour(dt)>7 & hour(dt)<19)

mod <- lm(sr~sr_lt+zenith, C1bind) #or could do times... both work p well
summary(mod) #.73 R2
#note should probably work to linearize zenith / use the proper relationship



ggplot(C1bind, aes(x=dt_lt, format="%m/%d/%Y %H:%M")) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=sr, color="ours")) + #other options: other hmps, airtemp_as_1.7_avg
  geom_line(aes(y=sr_lt, color = "niw")) +
  geom_line(aes(y=sr_eq, color = "eq")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Air temperature (C)") +
  scale_color_manual(name='Climate vars',
                     breaks=c('ours', 'niw', 'eq'),
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'ours' = 'lightgray', 
                       'niw' = 'lightgreen',
                       'eq'="red"))#,
                      # err="lightgray"))

C1bindsr <- C1bind %>% mutate(sr_rat = sr/sr_lt) %>% filter(sr_rat < 1000)
ggplot(C1bindsr, aes(x=dt_lt, y=sr/sr_lt)) + geom_line()

ggplot(C1binds %>% filter(year(dt)==2022), aes(x=dt_lt, y=sr/sr_lt)) + geom_line()

ggplot(C1binds %>% filter(year(dt)==2022 & dt > "2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt_lt)) + geom_line(aes(y=sr), color="blue") + geom_line(aes(y=sr_lt), color="green") 

# 
# #new idea -- line them up directly and then prepare to "roll" 
# #but going to treat this separately for a few points
# #cut off at 7/14 16:10 us[4200], lt2023[27965-(35339-11805)]
# templt <- lt2023[1:(27965-(35339-11805)+100),]
# tempus <- us2023[1:4300,]
# i_offset=0
# for(i in 2:4200) {
#   i=i-i_offset
#   diff <- tempus$dt[i]-templt$dt[i]
#   diff_lag <- tempus$dt[i]-templt$dt[i+1]
#   diff_lead <- tempus$dt[i]-templt$dt[i-1]
#   if(abs(diff_lag)<abs(diff)) {
#     templt <- templt[-i,]
#     i_offset=i_offset+1 #added this in because I think it should be there
#   }
# if(abs(diff_lead)<abs(diff)) {
#   tempus <- tempus[-i,]
#   i_offset=i_offset+1
# }
#  # print(length(templt$dt))
#  # print(length(tempus$dt))
#   print("i")
#   print(i)
#   print("diff")
#   print(abs(diff))
# }
# 
# t <- templt[1:(3858-(28065-28025)),] %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")
# 
# firstbind <- cbind(tempus[1:(4200-(4300-3918)),], t)
# 
# 
# templt <- lt2023[(30838-(35339-11805)):length(lt2023$dt),]
# tempus <- us2023[4211:length(us2023$dt),]
# i_offset=0
# for(i in 2:5666) {
#   i=i-i_offset
#   diff <- tempus$dt[i]-templt$dt[i]
#   diff_lag <- tempus$dt[i]-templt$dt[i+1]
#   diff_lead <- tempus$dt[i]-templt$dt[i-1]
#   if(abs(diff_lag)<abs(diff)) {
#     templt <- templt[-i,]
#     
#   }
#   if(abs(diff_lead)<abs(diff)) {
#     tempus <- tempus[-i,]
#     i_offset=i_offset+1
#   }
#   # print(length(templt$dt))
#   # print(length(tempus$dt))
#   print("i")
#   print(i)
#   print("diff")
#   print(abs(diff))
# }
# 
# 
# t <- templt %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")
# 
# #issue with amount... trace bqck by looking at templt and tempus
# secondbind <- cbind(tempus[1:3716,], t)

#SKIP?
#' C1bind <- rbind(firstbind, secondbind)
#' 
#' ggplot(C1bind, aes(x=T_1.00_lt, y=T_1.00)) + geom_point() + geom_smooth(method="lm")
#' mod <- lm(T_1.00~T_1.00_lt, C1bind)
#' summary(mod)
#' 
#' us_add <- us2023[4201:4210,] #for now there's a little mismatch here bc we could line lt up... ignoring it
#' lt_add <-lt2023[(27965-(35339-11805)+61):(30838-(35339-11805)-1),] %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")
#' C1all <- plyr::rbind.fill(C1bind, us_add, lt_add)
#' 
#' C1all <- C1all %>% mutate(niwotpred = T_1.00_lt*mod$coefficients[2] + mod$coefficients[1])
#' 
#' 
#' 
#' ggplot(C1all, aes(x=dt_lt, format="%m/%d/%Y %H:%M")) + 
#'   #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
#'   #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
#'   #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
#'   geom_line(aes(y=T_1.00, color="ours")) + #other options: other hmps, airtemp_as_1.7_avg
#'   geom_line(aes(y=niwotpred, color = "niw")) +
#'   geom_line(aes(y=niwotpred-T_1.00, color = "err")) +
#'   scale_y_continuous(
#'     # Features of the first axis
#'     name = "Air temperature (C)") +
#'   scale_color_manual(name='Climate vars',
#'                      breaks=c('ours', 'as1.7'),
#'                      values=c(#'Ghop temp'='green',
#'                        #'Soil model temp' = 'darkseagreen',
#'                        #'Veg model temp' = 'darkgreen',
#'                        'ours' = 'blue', 
#'                        'niw' = 'lightgreen',
#'                        err="lightgray"))
#' 
#' ggplot(C1all, aes(x=dt_lt, y=niwotpred-T_1.00)) + geom_line()
#' 

C1binds <- rbind(C1bind %>% select(-zenith, -time, -sr_eq), C1bind2)

C1binds <- C1binds %>% mutate(yr=as.factor(year(dt)))

ggplot(C1binds, aes(x=T_1.00_lt, y=T_1.00, color=yr)) + geom_point(alpha=.3) + geom_smooth(method="lm")
#temperatures luckily seem to agree from year to year pretty well (not too much diff in relationship with weather station)

mod <- lm(T_1.00~T_1.00_lt * yr, C1binds)
summary(mod) #and yet the year does make  significant difference (though it barely affects R2)


mod <- lm(T_1.00~T_1.00_lt, C1binds)
summary(mod) 

ggplot(C1bind, aes(x=T_soil_lt, y=T_soil)) + geom_point(alpha=.3) + geom_smooth(method="lm")
#temperatures luckily seem to agree from year to year pretty well (not too much diff in relationship with weather station)
mod <- lm(T_soil~T_soil_lt, C1bind)
summary(mod) #.488 R2 for 2022, much less for 2023 (5 inches in vs surface temp)


#now going for Eldo
nrel <- read.csv("nrel_eldo.csv")
nrel2 <- read.csv("nrel_eldo2.csv")
nrel <- rbind(nrel, nrel2)
nrel <- distinct(nrel)
usEldo <- all %>% filter(site=="Eldo", year(dt)==2023)
thing <- c()
for(i in 1:72719) { #Have to re-think these #s 72864
  thing <- c(thing, rep(i, 10))
}
thing <- c(thing, 72720) #72865 rep(, 1)
nrel$group <- thing
nrel_less <- nrel %>% 
  group_by(group) %>% 
  summarize(dt=mean(as.POSIXct(paste(DATE..MM.DD.YYYY., MST), format="%m/%d/%Y %H:%M", tz="MST")), GH_sr=mean(Global.Horizontal..W.m.2.), 
            DN_sr=mean(Direct.Normal..W.m.2.), DH_sr=mean(Direct.Normal..W.m.2.), 
            GA=mean(Global..Accumulated...kWhr.m.2.), T_2.00 = mean(Temperature...2m..deg.C.),
            WS_2.00=mean(Avg.Wind.Speed...2m..m.s.), surf=mean(Est.Surface.Roughness..m.))

nrel_win <- nrel_less %>% filter(dt>as.Date("5/15/2022 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2022 12:21", format="%m/%d/%Y %H:%M") | 
                                      (dt>as.Date("5/15/2023 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("10/01/2023 12:21", format="%m/%d/%Y %H:%M")))

nrel2023 <- nrel_win %>% filter(year(dt)==2023)

#now for air temp
nrel2023 <- nrel2023 %>% mutate(T_1.00=air_temp_profile(T_r=T_2.00, u_r=WS_2.00, zr=2, z0=surf_r, z=1, T_s=T_2.00)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m

tempus <- usEldo
tempnr <- nrel2023[4214:length(nrel2023$dt),]

nrextras <- tempnr[1:4213,]

i_offset=0
for(i in 2:10000) { #8000
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-tempnr$dt[i]
  diff_lag <- tempus$dt[i]-tempnr$dt[i+1]
  diff_lead <- tempus$dt[i]-tempnr$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    nrextras <- rbind(nrextras, tempnr[i,])
    tempnr <- tempnr[-i,]
    i_offset=i_offset+1
    print("lag")
    nrextras <- rbind(nrextras, tempnr[i,])
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



#I think they might be matched up to each other 3273 rows


nrextras <- rbind(nrextras, tempnr[3274:length(tempnr$dt),])

t <- tempnr %>% rename(dt_nr="dt", ws_nr="WS_2.00", T_1.00_nr="T_1.00")

nrextras <- nrextras %>% rename(dt_nr="dt", ws_nr="WS_2.00", T_1.00_nr="T_1.00")


#issue with amount... trace bqck by looking at templt and tempus
Eldobind <- cbind(tempus, t[1:3273,])

ggplot(Eldobind, aes(x=T_1.00_nr, y=T_1.00)) + geom_point() + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_nr, Eldobind)
summary(mod)
lm_coef <- coef(mod)

# # mod <- lm(T_1.00~T_1.00_nr, Eldobind)
# # summary(mod)
# 
# lm_coef <- coef(mod)
# 
# #should perhaps use nls https://stackoverflow.com/questions/31851936/exponential-curve-fitting-in-r
# 
# ggplot(Eldobind, aes(x=T_1.00_nr, y=T_1.00)) + geom_point() + geom_function(fun=function(x) exp(lm_coef[1])*exp(lm_coef[2]*x), color="red")
# 
# #exp(lm_coef[1])*exp(lm_coef[2]*t)


Eldoall <- plyr::rbind.fill(Eldobind, nrextras)

Eldoall <- Eldoall %>% mutate(nrpred = lm_coef[1]+lm_coef[2]*T_1.00_nr)



ggplot(Eldoall, aes(x=dt_nr, format="%m/%d/%Y %H:%M")) + 
  #  geom_line(aes(y=Tb_pred, color="Ghop temp")) +
  #  geom_line(aes(y=mbsoil, color="Soil model temp")) +
  #  geom_line(aes(y=mbvegh, color="Veg model temp")) +
  geom_line(aes(y=T_1.00, color="ours")) + #other options: other hmps, airtemp_as_1.7_avg
  geom_line(aes(y=nrpred, color = "nrel prediction")) +
  geom_line(aes(y=nrpred-T_1.00, color = "error")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Air temperature (C)") +
  scale_color_manual(name='Climate vars',
                     breaks=c('ours', 'nrel prediction', 'error'),
                     values=c(#'Ghop temp'='green',
                       #'Soil model temp' = 'darkseagreen',
                       #'Veg model temp' = 'darkgreen',
                       'ours' = 'blue', 
                       'nrel prediction' = 'lightgreen',
                       'error'="lightgray"))

ggplot(Eldoall, aes(x=dt_nr, y=nrpred-T_1.00)) + geom_line()




#doing it for 2022

nrel2022 <- nrel_win %>% filter(year(dt)==2022)

#now for air temp
nrel2022 <- nrel2022 %>% mutate(T_1.00=air_temp_profile(T_r=T_2.00, u_r=WS_2.00, zr=2, z0=surf_r, z=1, T_s=T_2.00)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m

usEldo <- all %>% filter(site=="Eldo", year(dt)==2022)

tempus <- usEldo
tempnr <- nrel2022[1359:length(nrel2022$dt),]

nrextras <- tempnr[1:1359,]

i_offset=0
for(i in 2:40000) { #8000
  print(i)
  i=i-i_offset
  diff <- tempus$dt[i]-tempnr$dt[i]
  diff_lag <- tempus$dt[i]-tempnr$dt[i+1]
  diff_lead <- tempus$dt[i]-tempnr$dt[i-1]
  if(abs(diff_lag)<abs(diff) & abs(diff_lag)<abs(diff_lead)) {
    nrextras <- rbind(nrextras, tempnr[i,])
    tempnr <- tempnr[-i,]
    i_offset=i_offset+1
    print("lag")
    nrextras <- rbind(nrextras, tempnr[i,])
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

nrextras <- rbind(nrextras, tempnr[16905:length(tempnr$dt),])

t <- tempnr %>% rename(dt_nr="dt", ws_nr="WS_2.00", T_1.00_nr="T_1.00")

nrextras <- nrextras %>% rename(dt_nr="dt", ws_nr="WS_2.00", T_1.00_nr="T_1.00")


#issue with amount... trace bqck by looking at templt and tempus
Eldobind2 <- cbind(tempus, t[1:16904,])

Eldobinds <- rbind(Eldobind, Eldobind2)

ggplot(Eldobinds, aes(x=T_1.00_nr, y=T_1.00, color=as.factor(year(dt)))) + geom_point(alpha=.3) + geom_smooth(method="lm")
mod <- lm(T_1.00~T_1.00_nr*year(dt), Eldobinds)
summary(mod)


ggplot(Eldobind, aes(x=GH_sr, y=sr, color=hour(dt))) + geom_point() #+ geom_smooth(method="lm")
mod <- lm(sr~GH_sr, Eldobind)
summary(mod)


#cut off at ~8/28 end of day for solar radiation (27797 index) (2023) -- sr issues
Eldobind2 <- Eldobind2 %>% filter(dt<="2022-08-28 19:31:08 MST")

ggplot(Eldobind2, aes(x=GH_sr, y=sr)) + geom_point() #+ geom_smooth(method="lm")
mod <- lm(sr~GH_sr, Eldobind2)
summary(mod)

#note to self: how do I make relationship with time linear? would zenith do it?



#reading in B1 substitute data (soil is somewhere else, this is north and south facing)  
hydroN22 <- read_csv("hydro_Met_NF_2022.csv")
hydroN23 <- read_csv("hydro_Met_NF_2023.csv")

hydroN <- rbind(hydroN22, hydroN23)

hydroN$TIMESTAMP <- as.POSIXct(hydroN$TIMESTAMP, format="%m/%d/%Y %H:%M")

hydroS22 <- read_csv("hydro_Met_SF_2022.csv")
hydroS23 <- read_csv("hydro_Met_SF_2023.csv")

hydroS <- rbind(hydroS22, hydroS23)

hydroS$TIMESTAMP <- as.POSIXct(hydroS$TIMESTAMP, format="%m/%d/%Y %H:%M")

#random thing -- let's plot all the solar radiations (i.e. at least C1 equivalent and Eldo equivalent)
#as.POSIXct(paste(DATE..MM.DD.YYYY., MST), format="%m/%d/%Y %H:%M", tz="MST")
ggplot() +
  geom_line(data=nrel_less %>% filter(as.POSIXct(dt)>"2023-06-28 19:31:08 MST" & as.POSIXct(dt) < "2023-07-04 19:31:08 MST"), aes(x=as.POSIXct(dt), y=GH_sr), color="red") +
  geom_line(data=dlter  %>% filter(dt>"2023-06-28 19:31:08 MST" & dt < "2023-07-04 19:31:08 MST"), aes(x=as.POSIXct(dt), y=sr), color="blue") +
  geom_line(data=hydroN %>% filter(TIMESTAMP>"2023-06-28 19:31:08 MST" & TIMESTAMP < "2023-07-04 19:31:08 MST"), aes(x=TIMESTAMP, y=`IN SW RAD(W/m^2)-2.5M(AVG)`), color="purple") +
  geom_line(data=hydroS  %>% filter(TIMESTAMP>"2023-06-28 19:31:08 MST" & TIMESTAMP < "2023-07-04 19:31:08 MST"), aes(x=TIMESTAMP, y=`IN SW RAD(W/m^2)-2.5M(AVG)`), color="green")


ggplot() + 
  geom_line(data=all %>% filter(site=="B1" & dt>"2023-06-28 19:31:08 MST" & dt < "2023-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="darkgreen") +
  geom_line(data=hydroS %>% filter(TIMESTAMP>"2023-06-28 19:31:08 MST" & TIMESTAMP < "2023-07-04 19:31:08 MST"), aes(x=TIMESTAMP, y=`IN SW RAD(W/m^2)-2.5M(AVG)`), color="lightgreen")


ggplot() + 
  geom_line(data=all %>% filter(site=="Eldo" & dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="darkred") +
  geom_line(data=nrel_less %>% filter(dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=GH_sr), color="red")


ggplot() + 
  geom_line(data=all %>% filter(site=="C1" & dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="darkblue") +
  geom_line(data=dlter %>% filter(dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=sr), color="lightblue")

#trying with T_1.00
# ggplot() + 
#   geom_line(data=all %>% filter(site=="C1" & dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt-60*60, y=T_1.00), color="darkblue") +
#   geom_line(data=dlter %>% filter(dt>"2022-06-28 19:31:08 MST" & dt < "2022-07-04 19:31:08 MST"), aes(x=dt, y=T_1.00), color="lightblue")


ggplot() + 
  geom_line(data=all %>% filter(site=="C1" & dt>"2023-07-07 19:31:08 MST" & dt < "2023-07-08 19:31:08 MST"), aes(x=dt-60*60, y=sr), color="darkblue") +
  geom_line(data=dlter %>% filter(dt>"2023-07-07 19:31:08 MST" & dt < "2023-07-08 19:31:08 MST"), aes(x=dt, y=sr), color="lightblue")