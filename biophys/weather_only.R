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

WS23 <- WS23 %>% mutate(sr = ((sr-39) * 1.8) + ((25 -T_1.25) * .0012)) 

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
  geom_line(aes(y=sr/coeff, color = "Solar rad")) +
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

surf <- surface_roughness(u_r=c(wslow,	wsmed,	wshi), zr=c(.57, .82, 1.05))


dlter <- dlter[-34674,]

dlter <- dlter %>% mutate(ws_1.7=wind_speed_profile_neutral(u_r=ws, zr=7, z0=surf, z=1.7))

#now for air temp
dlter <- dlter %>% mutate(T_1.00=air_temp_profile(T_r=T_1.7, u_r=ws_1.7, zr=1.7, z0=surf, z=1, T_s=T_1.7)) #FOR NOW putting air temp for soil temp because it's unlikely to affect it much at 1m

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

#new idea -- line them up directly and then prepare to "roll" 
#but going to treat this separately for a few points
#cut off at 7/14 16:10 us[4200], lt2023[27965-(35339-11805)]
templt <- lt2023[1:(27965-(35339-11805)+100),]
tempus <- us2023[1:4300,]
i_offset=0
for(i in 2:4200) {
  i=i-i_offset
  diff <- tempus$dt[i]-templt$dt[i]
  diff_lag <- tempus$dt[i]-templt$dt[i+1]
  diff_lead <- tempus$dt[i]-templt$dt[i-1]
  if(abs(diff_lag)<abs(diff)) {
    templt <- templt[-i,]
    
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

t <- templt[1:(3858-(28065-28025)),] %>% rename(dt_lt="dt", ws_lt="ws", sr_lt="sr", T_soil_lt="T_soil", T_1.00_lt="T_1.00")

firstbind <- cbind(tempus[1:(4200-(4300-3918)),], t)



ggplot(firstbind, aes(x=T_1.00, y=T_1.00_lt)) + geom_point() + geom_smooth(method="lm")
summary(lm(T_1.00~T_1.00_lt, firstbind)) 