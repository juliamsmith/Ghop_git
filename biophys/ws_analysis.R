library(readr)
library(tidyverse)

# wsd <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_06202023_mod.csv", col_names=FALSE)
# 
# wsd$sr <- wsd$X23
# wsd$ws <- wsd$X15
# wsd$dt <- as.POSIXct( wsd$X1, format="%m/%d/%Y %H:%M", tz = "MST" ) + 60*60 #plus 1hr
# 
# #mean of duplicate times -- a little quick and dirty... really want bins of +-1 min or so
# wsd_less <- wsd %>% group_by(dt) %>% summarize(sr=mean(sr), ws=mean(ws))

#for A1 x6 is t_soil, x7 is t_0.25, x8 is t_0.50, x3 is t_0.75, x4 t_1.00, x5 t_1.25

###SKIP to here for now

wsdb1a <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/B1/B1_06192023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdb1b <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/B1/B1_07132023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdb1c <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/B1/B1_08212023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdb1d <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/B1/B1_09182023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1a <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_06202023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1b <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_07042023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1c <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_07142023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1d <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_07282023_only.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1e <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_08192023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdc1f <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/C1/C1_09192023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdela <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/Eldo/Eldo_06172023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdelb <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/Eldo/Eldo_07252023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdelc <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/Eldo/Eldo_08192023_mod2.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)

wsdb1 <- rbind(wsdb1a, wsdb1b, wsdb1c, wsdb1d)

list_c1_df <- list(wsdc1a,wsdc1b,wsdc1c,wsdc1d,wsdc1e,wsdc1f)

wsdc1 <- list_c1_df %>% reduce(full_join)

list_el_df <- list(wsdela,wsdelb,wsdelc)

wsdel <- list_el_df %>% reduce(full_join)

wsdb1$site <- "B1"
wsdc1$site <- "C1"
wsdel$site <- "Eldo"

wsd <- rbind(wsdb1, wsdc1, wsdel)
#as.POSIXct(wsd_less$dt, format="%m/%d/%Y %H:%M", tz = "MST" )
dff <- as.numeric(difftime(as.POSIXct(wsd$dt, format="%m/%d/%Y %H:%M", tz = "MST" ),
                as.POSIXct(wsd$dt[1], format="%m/%d/%Y %H:%M", tz = "MST" ),
                units="mins"))
# 
# #from https://stackoverflow.com/questions/66220486/group-a-vector-of-numbers-by-range
# dffu <- unique(dff)
# first_list <- unique(apply(outer(dffu, dffu, "-"), 1, function(x){vec[(x < 3 & x >= 0)] }))
# final_list <- first_list[!sapply(seq_along(first_list), function(i) max(sapply(first_list[-i],function(L) all(first_list[[i]] %in% L))))]
# #error can't allocate that much memory

#new idea is a double diff
dffl <- dff-lag(dff)
break_inds <- dffl>1
bins <- dff[break_inds]
bins[1] <- 0

#note break should be after index 18 before 19 of dff

wsd$group <- cut(dff, breaks=unique(bins), include.lowest=TRUE, right=FALSE)

#wsd %>% group_by(group) %>% (mean(dt))
wsd$dt <- as.POSIXct(wsd$dt, format="%m/%d/%Y %H:%M", tz = "MST" ) 
wsd_less <- aggregate(wsd, list(wsd$group, site=wsd$site), mean) 
wsd_less <- wsd_less[,2:11]
#res$dt <- as.POSIXct(res$dt, origin="1970-01-01")

# wsd_less <- wsd %>% group_by(dt, site) %>% summarize(sr=mean(sr), 
#                                               ws=mean(ws), 
#                                               T_soil=mean(T_soil), 
#                                               T_0.25=mean(T_0.25),
#                                               T_0.50=mean(T_0.50),
#                                               T_0.75=mean(T_0.75),
#                                               T_1.00=mean(T_1.00),
#                                               T_1.25=mean(T_1.25))
# 
# wsd_less <- wsd_less[3:length(wsd_less$sr),] #remove a few weird entries
# #wsdb1_less <- wsdb2less

#had forgotten to do this before
wsd_less$dt <- as.POSIXct(wsd_less$dt, format="%m/%d/%Y %H:%M", tz = "MST" ) + 60*60 #plus 1hr

saveRDS(wsd_less, file = "biophys/all_WS_dat_excA1.RDS")

wsd_less_t <- wsd_less %>% gather("clim_var", "value", -dt, -site) #prob want to expand to all ws vars (exc dt)

#make a plot -- note some gaps in different sites' WS -- gap in Eldo (blue) early/mid and late in season, gap in C1 (green) mid-season
ggplot(wsd_less_t %>% filter(clim_var=="T_0.25"), aes(x=dt, y=value, col=site)) + geom_point(alpha=.25)

#let's make a clearer plot that's just about coverage (not data):
ggplot(wsd_less %>% mutate(v=ifelse(site=="C1", .75, ifelse(site=="B1", .5, .25))), aes(x=dt, y=v, color=site)) + geom_point()

#let's zero in on a location of overlap of coverage at all sites: mid-august 
wsdzoom <- wsd_less_t %>% filter(as.Date(dt, format="%m/%d/%Y %H:%M")>as.Date("8/11/2023 16:30", format="%m/%d/%Y %H:%M") & as.Date(dt, format="%m/%d/%Y %H:%M")<as.Date("8/18/2023 12:21", format="%m/%d/%Y %H:%M"))

#solar radiation
ggplot(wsdzoom %>% filter(clim_var=="sr"), aes(x=dt, y=value, col=site)) + geom_line() +ylab("solar radiation")

#air temp (.25m)
ggplot(wsdzoom %>% filter(clim_var=="T_0.25"), aes(x=dt, y=value, col=site)) + geom_line() + tlab("air temp (.25m)")

#wind speed (1m) -- a little confusing... I guess it varies on a faster time scale?
ggplot(wsdzoom %>% filter(clim_var=="ws"), aes(x=dt, y=value, col=site)) + geom_line() + ylab("wind speed")

#soil temp
ggplot(wsdzoom %>% filter(clim_var=="T_soil"), aes(x=dt, y=value, col=site)) + geom_line() + ylab("soil temp")
wz <- wsdzoom %>% filter(clim_var=="sr")
#write_csv(wsdzoom, "zoomwsmidAug.csv") #this seems to lead to issues

saveRDS(wsdzoom, file = "biophys/zoomwsmidAug.RDS") 


save()
load("stuff.RData")

#let's compare climate data coverage to behavioral observations
setwd("G:/Shared drives/RoL_FitnessConstraints/projects/TbandObs2023")
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

covws <- wsd_less %>% mutate(v=ifelse(site=="C1", .75, ifelse(site=="B1", .5, .25))) %>% select(dt, site, v)
covnc <- new_cages %>% select(Site, dt) %>% mutate(site=Site) %>% mutate(v=ifelse(site=="C1", .7, ifelse(site=="B1", .45, .2))) %>% select(-Site)

cov <- rbind(covws, covnc)

ggplot(cov, aes(x=dt, y=v, color=site)) + geom_point() #note lower values of same color are obs



##these were from before
# ggplot(wsd_less_t %>% filter(dt>as.Date("7/12/2023 16:42", format="%m/%d/%Y %H:%M") & dt<as.Date("7/13/2023 16:42", format="%m/%d/%Y %H:%M")), aes(x=dt, y=temp, col=height_m)) + geom_line()
# 
# ggplot(wsd_less_t , aes(x=dt, y=temp, col=height_m)) + geom_line()
# 
# ggplot(wsd_less_t , aes(x=dt, y=ws)) + geom_line()
# 
# ggplot(wsd_less_t , aes(x=dt, y=sr)) + geom_line()
# 
# ggplot(wsd_less_t %>% filter(dt>"7/12/2023 5:42" & dt<"7/12/2023 22:42"), aes(x=dt, y=temp, col=height_m)) + geom_line(group=1)
