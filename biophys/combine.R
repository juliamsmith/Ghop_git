library(tidyverse)

#read in WS data
WS <- readRDS("biophys/all_WS_dat_excA1.RDS")

#read in behavioral observations
setwd("G:/Shared drives/RoL_FitnessConstraints/projects/TbandObs2023")

##get cage observations
obs <- read.csv("logs3.csv")

obs <- obs %>% mutate(t=as.POSIXct(strptime(sub(".*at ", "", Date.Created), "%I:%M:%S %p"))+60*60, 
                      dt=as.POSIXct(sub("at", "", Date.Created),format="%b %d- %Y %I:%M:%S %p", tz="MST") + 60*60)

obs <- obs %>% mutate(Site=case_when(
  Elevation>1000 & Elevation < 2100 ~ "Eldo",
  Elevation>2100 & Elevation < 2400 ~"A1",
  Elevation>2400 & Elevation < 2700 ~ "B1",
  Elevation>2700 ~ "C1"
))

obs2023 <- obs %>% filter(as.numeric(format(obs$dt, "%Y"))==2023) %>% mutate(date=as.Date(dt))

obs2 <- read_csv("extralogs.csv")

obs2 <- obs2 %>% mutate(date=as.Date(date, "%m/%d/%Y"))

obs2023plus <- rbind(obs2023 %>% select(Tag, Content, t, date, Site), obs2)

cages <- obs2023 %>% filter(Tag=="Ghop") 


new_cages <- cages %>% mutate(cage=as.numeric(str_remove(str_remove(substr(Content, 1, 3), "[a-zA-Z]"), " "))) %>% filter(!is.na(Site) & !is.na(cage))

#add a species column
new_cages2 <- new_cages %>% mutate(Species=ifelse(Site=="Eldo", 
                                                 "MS", 
                                                 ifelse(Site=="C1",
                                                        "MB",
                                                        ifelse(cage<14 | cage>19, 
                                                               "MS", 
                                                               "MB"))))




##finding keywords in order to separate out Activity, Location, and Sun exposure from Content
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

#extract from "content"
final_obs_spp <- new_cages2 %>% mutate(Activity=kywrd(Content, activities), Location=kywrd(Content, locations), Exposure=kywrd(Content, exposures))


##combine ...



#finding nearest WS time to behavioral obs
#allobs <- final_obs_spp
#allweather <- WS
final_obs_spp$wdt <- final_obs_spp$dt #duplicate just to get the format right

for(i in 1:dim(final_obs_spp)[1]){
  obs <- final_obs_spp[i,]
  #allweather <- as.data.frame(allweather)
  s <- obs$Site
  aw_new <- WS %>% filter(site==s)
  closestind <- which.min(abs(difftime(aw_new$dt, obs$dt)))
  nearest_WS_time <- aw_new[closestind, "dt"]
  final_obs_spp$wdt[i] <- nearest_WS_time
}

dat <- distinct(merge(final_obs_spp, WS, by.x=c("wdt", "Site"), by.y=c("dt", "site")))
dat <- dat %>% mutate(t=format(wdt, "%I:%M:%S %p"))
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
                          CAGH=as.numeric(Location=="CAGH")#,
                          #t=format(wdt, "%I:%M:%S %p")
)


#successfuly merged the two! let's save that
setwd("C:/Users/smith/Desktop/Ghop_git")
saveRDS(new_dat, file = "biophys/pairedcagews.RDS")
