#combine2022

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




# read in behavior and merge in

obs <- read.csv("JuliaObs/rough_obs_09_28.csv")
cages <- readxl::read_xlsx("cage_metadata.xlsx")

site_obs <- obs %>% filter(Tag=="Ghop") %>% mutate(Site=case_when(
  Elevation>1000 & Elevation < 2100 ~ "Eldo",
  Elevation>2100 & Elevation < 2400 ~"A1",
  Elevation>2400 & Elevation < 2700 ~ "B1",
  Elevation>2700 ~ "C1"
  
))
#4389 obs

new_obs <- site_obs %>% mutate(t=as.POSIXct(strptime(sub(".*at ", "", Date.Created), "%I:%M:%S %p")), cage=as.numeric(str_remove(str_remove(substr(Content, 1, 3), "[a-zA-Z]"), " "))) %>% filter(!is.na(Site) & !is.na(cage))
#4158 obs

#these are the 231 obs that were discarded by the filter... fix them at some point with some data sleuthing!
disc_obs <- site_obs %>% mutate(t=as.POSIXct(strptime(sub(".*at ", "", Date.Created), "%I:%M:%S %p")), cage=as.numeric(str_remove(str_remove(substr(Content, 1, 3), "[a-zA-Z]"), " "))) %>% filter(is.na(Site) | is.na(cage))

cages$cage_nr <- as.numeric(cages$cage_nr)

new_obs_spp <- left_join(new_obs, cages, by=c("Site"="transplant_site", "cage"="cage_nr"))


# Remove any duplicates
new_obs_spp <- new_obs_spp %>% arrange(Date.Created, desc(species)) %>% filter(!duplicated(Date.Created))

#sloppy way to account for some missing entries in cage metadata
new_obs_spp$species[is.na(new_obs_spp$species)] <- "MS"

kywrd <- function(content, keylist){
  categ <- NA
  for(i in 1:length(keylist)){
    k <- keylist[i]
    categ <- ifelse(grepl(k, content), k, categ)
  }
  return(categ)
}

activities <- c("WALK", "FEED", "MATE", "STAT", "GROO", "CLMB", "EGGL")
locations <- c("CAGL", "CAGM", "CAGH", "SOIL", "VEGL", "VEGH", "ROCK")
exposures <- c("SHAD", "MSHA", "SHSU", "MSUN", "ASUN")

#now trying to extract from "content"
final_obs_spp <- new_obs_spp %>% mutate(Activity=kywrd(Content, activities), Location=kywrd(Content, locations), Exposure=kywrd(Content, exposures))

#probably something that didn't work out / end up being necessary\

# library(data.table)
# obsDT <- as.data.table(final_obs_spp)
# #siteDT <- as.data.table(C1_0921)
# 
# obsDT[siteDT, reference_date := i.reference_date, on = .(Date = reference_date), roll = TRUE]

o_spp <- final_obs_spp
o_spp$dt <- as.POSIXct(sub("at", "", final_obs_spp$Date.Created),format="%b %d- %Y %I:%M:%S %p", tz="MST")
o_spp <- o_spp %>% mutate(dt=dt + hours(1), t=t+hours(1)) #note Date.Created is untrustworthy / still off now

### Merge behavior and WS data (dataframe new_dat) ####

#finding nearest WS time to behavioral obs
allobs <- o_spp
#allweather <- uWS
o_spp$wdt <- o_spp$dt #duplicate just to get the format right
for(i in 1:dim(o_spp)[1]){
  obs <- o_spp[i,]
  #allweather <- as.data.frame(allweather)
  s <- obs$Site
  aw_new <- uWS %>% filter(site==s)
  closestind <- which.min(abs(difftime(aw_new$dt, obs$dt)))
  nearest_WS_time <- aw_new[closestind, "dt"]
  o_spp$wdt[i] <- nearest_WS_time
}

dat <- distinct(merge(o_spp, uWS, by.x=c("wdt"), by.y=c("dt")))
new_dat <- dat %>% mutate(WALK=as.numeric(Activity=="WALK"), 
                          STAT=as.numeric(Activity=="STAT"), 
                          MATE=as.numeric(Activity=="MATE"), 
                          GROO=as.numeric(Activity=="GROO"),
                          FEED=as.numeric(Activity=="FEED"),
                          EGGL=as.numeric(Activity=="EGGL"),
                          CLMB=as.numeric(Activity=="CLMB"),
                          VEGL=as.numeric(Location=="VEGL"), 
                          VEGH=as.numeric(Location=="VEGH"), 
                          SOIL=as.numeric(Location=="SOIL"), 
                          ROCK=as.numeric(Location=="ROCK"),
                          CAGL=as.numeric(Location=="CAGL"),
                          CAGM=as.numeric(Location=="CAGM"),
                          CAGH=as.numeric(Location=="CAGH"),
                          t=format(wdt, "%I:%M:%S %p")
                          

)


new_dat_self <- new_dat %>% filter(Site==coll_site)
# 
# setwd("C:/Users/smith/Desktop/Ghop_git")
# saveRDS(new_dat_self, file = "biophys/pairedcagews2022.RDS")
# 
#new_dat_self$dt <- as.POSIXct(new_dat_self$dt,format="%b %d- %Y %I:%M:%S %p", tz="MST")
#new_dat_

new_dat_self$Species <- new_dat_self$species

new_dat_self <- new_dat_self %>% select(-coll_site, 
                        -coll_date, 
                        -coll_elevation, 
                        -starting_instar, 
                        -starting_n_females, 
                        -starting_n_males,
                        -days_between_coll_transplant,
                        -site,
                        -notes,
                        -transplant_date,
                        -transplant_elevation,
                        -t.x,
                        -t.y,
                        -end_date,
                        -species)

new_dat_self$year <- "2022"

setwd("C:/Users/smith/Desktop/Ghop_git")

pairedcagews <- readRDS("biophys/pairedcagews.RDS")

pairedcagews$year <- "2023"

all <- bind_rows(pairedcagews, new_dat_self)


setwd("C:/Users/smith/Desktop/Ghop_git")
saveRDS(all, file = "biophys/pairedcagewsbothyrs.RDS")