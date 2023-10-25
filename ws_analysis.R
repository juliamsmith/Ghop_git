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

wsd_less <- wsd %>% group_by(dt, site) %>% summarize(sr=mean(sr), 
                                              ws=mean(ws), 
                                              T_soil=mean(T_soil), 
                                              T_0.25=mean(T_0.25),
                                              T_0.50=mean(T_0.50),
                                              T_0.75=mean(T_0.75),
                                              T_1.00=mean(T_1.00),
                                              T_1.25=mean(T_1.25))

wsd_less <- wsd_less[3:length(wsd_less$sr),] #remove a few weird entries
#wsdb1_less <- wsdb2less

#had forgotten to do this before
wsd_less$dt <- as.POSIXct(wsd_less$dt, format="%m/%d/%Y %H:%M", tz = "MST" ) + 60*60 #plus 1hr

wsd_less_t <- wsd_less %>% gather("clim_var", "value", -dt, -site) #prob want to expand to all ws vars (exc dt)

#make a plot -- note some gaps in different sites' WS -- gap in Eldo (blue) early/mid and late in season, gap in C1 (green) mid-season
ggplot(wsd_less_t %>% filter(clim_var=="T_0.25"), aes(x=dt, y=value, col=site)) + geom_point(alpha=.25)

#let's make a clearer plot that's just about coverage (not data):
ggplot(wsd_less %>% mutate(v=ifelse(site=="C1", .75, ifelse(site=="B1", .5, .25))), aes(x=dt, y=v, color=site)) + geom_point()

#let's zero in on a location of overlap of coverage at all sites: mid-august 
wsdzoom <- wsd_less_t %>% filter(dt>as.Date("8/11/2023 16:30", format="%m/%d/%Y %H:%M") & dt<as.Date("8/18/2023 12:21", format="%m/%d/%Y %H:%M"))

#solar radiation
ggplot(wsdzoom %>% filter(clim_var=="sr"), aes(x=dt, y=value, col=site)) + geom_line() +ylab("solar radiation")

#air temp (.25m)
ggplot(wsdzoom %>% filter(clim_var=="T_0.25"), aes(x=dt, y=value, col=site)) + geom_line() + tlab("air temp (.25m)")

#wind speed (1m) -- a little confusing... I guess it varies on a faster time scale?
ggplot(wsdzoom %>% filter(clim_var=="ws"), aes(x=dt, y=value, col=site)) + geom_line() + ylab("wind speed")

#soil temp
ggplot(wsdzoom %>% filter(clim_var=="T_soil"), aes(x=dt, y=value, col=site)) + geom_line() + ylab("soil temp")

#write_csv(wsdzoom, "zoomwsmidAug.csv")

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
