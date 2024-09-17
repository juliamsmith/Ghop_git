library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(boot)
library(car)
library(patchwork)
library(minpack.lm)

#vignette("fit_many_curves") #first just following this

setwd("C:/Users/smith/Desktop/Ghop_git") #set wd to project directory

dry <- read_csv("TPC/DigestionDryMasses.csv")
wetMB <- read_csv("TPC/MB_WetMasses_rough.csv")
wetMS <- read_csv("TPC/MS_WetMasses_rough.csv")
trialsMB <- read_csv("TPC/MB_AllSites.csv")
trialsMS <- read_csv("TPC/MS_AllSites.csv")
hoppersMB <- read_csv("TPC/MB_Individual_data_rough.csv")
hoppersMS <- read_csv("TPC/MS_Individual_data_rough.csv")

wetMB$spp <- "MB"
wetMS$spp <- "MS"

wetMB <- wetMB %>% select(-...8, -...9)
wetMS <- wetMS %>% select(-...8, -...9, -...10, -...11, -old_plot_ID)

trialsMB$spp <- "MB"
trialsMS$spp <- "MS"

hoppersMB$spp <- "MB"
hoppersMS$spp <- "MS"

trials <- rbind(trialsMS, trialsMB)

hoppers <- rbind(hoppersMS, hoppersMB)

#investigating missing hind legs
#let's look into some odd masses (incl correcting for mhl)
#for a given sex and species (and site if possible), find the avg diff between
#mass of those with and w/o both hl
hoppers <- hoppers %>% mutate(mhl_init=ifelse((notes!="mlhl" & notes!="mrhl") | is.na(notes), "alllegs", "missing1"))
mhlmass <- hoppers %>% group_by(spp,sex, mhl_init) %>% summarize(avg_mass=mean(mass, na.rm=TRUE), med_mass=median(mass, na.rm=TRUE), n())
mhlmass <- mhlmass %>% mutate(center_mass=ifelse(`n()`>10, med_mass,avg_mass))
mhlmass1 <- mhlmass %>% select(-`n()`, -avg_mass, -med_mass) %>% pivot_wider(names_from=mhl_init, values_from = center_mass)
mhlmass1 <- mhlmass1 %>% mutate(legmass=alllegs-missing1)
mhlmass1 <- mhlmass1[-5,]
#I look at the difference in masses with missing hind legs and without missing hind legs
#chose .04g as the leg mass for all MS... it's in between that for males and females and seems fairly realistic
hoppers <- hoppers %>% mutate(mass_adj=ifelse(mhl_init=="alllegs", 
                                              mass, 
                                              ifelse(spp=="MS", 
                                                     mass+.04,
                                                     ifelse(sex=="F",
                                                            mass+.0690,
                                                            mass+.0514))))

wet <- rbind(wetMS, wetMB)

#merge hopper data with their trials
trials_more <- merge(trials, hoppers, by=c("full_ID","spp"))


# account for plot and wet mass
wg <- merge(wet, dry, by.x="wg_ID", by.y="sample_num")

morew2d <- read_csv("TPC/Morew2d.csv")

morew2d <- morew2d %>% select(-ID_prime, -rng, -notes)

mod<- lm(`wg_drymass (mg)`~ 0+ wet_mass, morew2d)
summary(mod)

mg2mg <- morew2d %>% mutate(wet_mass=wet_mass*1000)
mod2<- lm(`wg_drymass (mg)`~ 0+ wet_mass, mg2mg)
summary(mod2)

mod2<- lm(wet_mass~ 0+ `wg_drymass (mg)`, mg2mg)
summary(mod2)

newdf <- data.frame(wg %>% select(wet_mass, plot_ID))
newdf$wet_mass <- as.numeric(newdf$wet_mass)
predictions <- predict(mod, newdf, 
                       interval = "prediction", level = .95)

#add predicted dry masses
wgnpred <- cbind(wg, predictions)

#calculated range of dry mass eaten (using full CI range of predicted dry mass)
tpcmb <- wgnpred %>% mutate(eaten=fit-as.numeric(`wg_drymass (mg)`), eaten_l=lwr-as.numeric(`wg_drymass (mg)`), eaten_h=upr-as.numeric(`wg_drymass (mg)`)) #need to connect to temps

#merge with the trials (now we have temperatures)
mrg <- merge(trials_more, tpcmb, by.x=c("wg", "spp"), by.y=c("wg_ID", "spp"))

mrg <- mrg %>% mutate(site=factor(mrg$site, levels=c("Eldo", "A1", "B1", "C1")),fecesdry=ifelse(is.na(as.numeric(`feces_drymass (mg)`)), 0, as.numeric(`feces_drymass (mg)`)), discolored=replace_na(`discolored (Y/N)`, "N"), temp=as.numeric(temp))

#more examination of mrg
ggplot(mrg, aes(x=temp, y=fecesdry, color=interaction(is.na(notes.x), is_valid), shape=sex)) + geom_point() + facet_grid(spp~site)

mrg_refinedforwg <- mrg %>% filter(discolored=="N" & (is.na(`wg_mold (Y/N/1-4)`) | `wg_mold (Y/N/1-4)`=="N") & is.na(is_valid))

mrg_refinedforfeces <- mrg %>% filter((is.na(`feces_mold (Y/N/1-4)`) | `feces_mold (Y/N/1-4)`=="N") & is.na(is_valid))

d_fe_og <- mrg_refinedforfeces %>% rename(rate=fecesdry)
d_fe_og$rate[is.na(d_fe_og$rate)]=0

d_wg_og <- mrg_refinedforwg %>% rename(rate=eaten) %>% filter(!is.na(temp) & !is.na(rate))

d_wg_og <- d_wg_og %>% mutate(batch = paste(start_date, temp, sep="_"))

d_wg_og$start_datetime <- as.POSIXct(d_wg_og$start_datetime, format="%m/%d/%Y %H:%M")
d_wg_og$end_datetime <- as.POSIXct(d_wg_og$end_datetime, format="%m/%d/%Y %H:%M")

d_wg_og <- d_wg_og %>% mutate(eat_time = end_datetime-start_datetime,
                              wg_adj = rate/mass_adj,
                              wg_double_adj=wg_adj/as.numeric(eat_time),
                              wg_tadj = rate/as.numeric(eat_time))


#creating a new variable -- batch reflects combination of date and temperature
d_fe_og <- d_fe_og %>% mutate(batch = paste(start_date, temp, sep="_"))

d_fe_og$start_datetime <- as.POSIXct(d_fe_og$start_datetime, format="%m/%d/%Y %H:%M")
d_fe_og$end_datetime <- as.POSIXct(d_fe_og$end_datetime, format="%m/%d/%Y %H:%M")

d_fe_og <- d_fe_og %>% mutate(fec = ifelse(is.na(as.numeric(`feces_drymass (mg)`)), 0, as.numeric(`feces_drymass (mg)`)), 
                              eat_time = end_datetime-start_datetime,
                              fec_adj = fec/mass_adj,
                              fec_double_adj=fec_adj/as.numeric(eat_time),
                              fec_tadj = fec/as.numeric(eat_time))
#then let's t test to see if males and females differ once mass-corrected
d_fe_og_F <- d_fe_og %>% filter(sex=="F")
d_fe_og_M <- d_fe_og %>% filter(sex=="M")


#some viz

ggplot(d_fe_og %>% filter(spp=="MB"), aes(x=mass_adj, y=as.numeric(`feces_drymass (mg)`), color=sex)) + geom_point() + facet_grid(site~temp)

ggplot(d_fe_og %>% filter(spp=="MB"), aes(x=mass_adj, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=sex)) + geom_point() + facet_grid(site~temp)

ggplot(d_fe_og %>% filter(spp=="MS"), aes(x=mass_adj, y=as.numeric(`feces_drymass (mg)`), color=sex)) + geom_point() + facet_grid(site~temp)

ggplot(d_fe_og %>% filter(spp=="MS"), aes(x=mass_adj, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=sex)) + geom_point() + facet_grid(site~temp)




spps <- c("MB", "MS")
sites <- c("Eldo", "A1", "B1", "C1")
temps <- c(16,23,30,37,40,43)
sppsvec <- c()
sitesvec <- c()
tempsvec <- c()
tsexvec <- c()
for(sppi in spps){
  for(sitei in sites){
    for(tempi in temps){
      dfF <- d_fe_og_F %>% filter(spp==sppi, site==sitei, temp==tempi)
      dfM <- d_fe_og_M %>% filter(spp==sppi, site==sitei, temp==tempi)
      if(nrow(dfF) !=0 & nrow(dfM) !=0){
        sppsvec <- c(sppsvec, sppi)
        sitesvec <- c(sitesvec, sitei)
        tempsvec <- c(tempsvec, tempi)
        tsexvec <- c(tsexvec, t.test(dfF$fec_adj, dfM$fec_adj)$p.value)
      }
    }
  }
}

df <- data.frame(spp=sppsvec, site=sitesvec, temp=tempsvec, tsex=tsexvec)
#mostly not... may be able to ignore sex depending on how we adjust for multiple comparisons


ggplot(d_fe_og %>% filter(spp=="MB", site=="A1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MB", site=="B1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MB", site=="C1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 

ggplot(d_fe_og %>% filter(spp=="MS", site=="Eldo"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MS", site=="A1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MS", site=="B1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=full_ID, shape=sex)) + geom_point() + geom_line() 


ggplot(d_fe_og %>% filter(spp=="MB", site=="A1"), aes(x=temp, y=fec_tadj, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MB", site=="B1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eat_time, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MB", site=="C1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eat_time, color=full_ID, shape=sex)) + geom_point() + geom_line() 

ggplot(d_fe_og %>% filter(spp=="MS", site=="Eldo"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eat_time, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MS", site=="A1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eat_time, color=full_ID, shape=sex)) + geom_point() + geom_line() 
ggplot(d_fe_og %>% filter(spp=="MS", site=="B1"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eat_time, color=full_ID, shape=sex)) + geom_point() + geom_line() 



#sanity check on batches... I think the batches don't look too distinct from one another (overall since site shouldn't matter) 
ggplot(d_fe_og %>% filter(spp=="MB"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=batch, shape=sex)) + geom_jitter(width=1, height=0) #+ facet_wrap(~site) 

ggplot(d_fe_og %>% filter(spp=="MS"), aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/mass_adj, color=batch, shape=sex)) + geom_jitter(width=1, height=0) #+ facet_wrap(~site) 


#and now just doing a regular viz like before

ggplot(d_wg_og, aes(x=temp, y=fecesdry/rate, color=sex)) + geom_point() + geom_smooth() + facet_grid(spp~site)
ggplot(d_wg_og, aes(x=temp, y=rate, color=sex)) + geom_point() + geom_smooth() + facet_grid(spp~site)
ggplot(d_wg_og, aes(x=rate, y=fecesdry, color=as.factor(temp), lty=sex)) + geom_point() + geom_smooth(method="lm", se=FALSE) + facet_grid(spp~site)
d_wg_og %>% group_by(spp, site, sex, temp) %>% summarise(assim=mean(fecesdry/rate), sd=sd(fecesdry/rate))
#try the same thing but with mean fit because small differences in mass prob don't matter


ggplot(d_fe_og, aes(x=temp, y=fec_double_adj, color=sex)) + geom_point() + geom_smooth() + facet_grid(spp~site)

ggplot(d_fe_og %>% filter(site=="A1", spp=="MS", temp==43), aes(x=full_ID, y=fec_double_adj, shape=sex, color=start_date)) + geom_point()

ggplot(d_fe_og %>% filter(spp=="MS", temp==43), aes(x=full_ID, y=fec_double_adj, shape=sex, color=start_date)) + geom_point() #+ facet_wrap(~site)

#d_fe_og %>% filter(spp=="MS") %>% group_by()


#data quality notes to self 
#- duplicates 415x, 568x, 574x, 591x, 610x, 626x, 639x, 659x, 694x, 711x
#- look into time ranges and trials
#-2 20sx, missing 36x, 42x, 68x, 69 (missing everywhere?), 70x, 87 (missing everywhere?), 152x, 197x, 215x, 216x, 227x, 270-280 (ex. 277)x, 284-300 is intentional I think -- gap 269-300 should be intentional
#311x, 319x, 325x, 342x, 381-383 (381 -- 30 8/25; 382 instead of 282?, 283?), 399 (real gap), 400x, 409x, 410 (real gap), 452x, something weird happening with 473(couldn't find issue)
# 582 (real gap)... 482... just missing, 507 (real gap), 517x, 565 (real gap, 43 8/28),642 (real gap, 23 8/30),644 (nowhere), 655 (real gap 43 8/30),678 (real gap 43 8/30)

#20/2: MB_C1_M_05 (0.251g hopper, 40C) and MB_A1_M_11 (0.3417, 37C) both seem like 20, MB_B1_F_04 (is 2)


#### let's do some analyses: lme


#library(nlme)
library(lme4)
library(kableExtra)
library(lmerTest)
library(AICcmodavg)

#temperature and species are significant
d_fe_og$start_date <- as.factor(d_fe_og$start_date) #made start date a factor
#should temp be a factor or not?
# feedmod <- lmer(fec_adj ~ as.factor(temp) + as.factor(temp)*as.factor(site) + as.factor(temp)*as.factor(sex) + as.factor(temp)*as.factor(spp) + 
#                   (1|full_ID) + 
#                   (1|start_date/batch) #this one leads to singularity issues.. not sure if that's okay
#                 ,  #gotta work on this... maybe nesting in start_date is wrong?
#                 na.action = 'na.omit', #uncommented
#                 #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
#                 data = d_fe_og) 
# 
# 
# feedanova <- Anova(feedmod, type=3)



feedmodMB1 <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                  (1|full_ID),  
                na.action = 'na.omit', #uncommented
                REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB1 <- Anova(feedmodMB1, type=3)


feedmodMB2 <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB2 <- Anova(feedmodMB2, type=3)

feedmodMB3 <- lmer(fec_double_adj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB3 <- Anova(feedmodMB3, type=3)


feedmodMB4 <- lmer(fec_double_adj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB4 <- Anova(feedmodMB4, type=3)

feedmodMB5 <- lmer(fec_double_adj ~ poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB5 <- Anova(feedmodMB5, type=3)

aictab(list(feedmodMB1, feedmodMB2, feedmodMB3, feedmodMB4, feedmodMB5), modnames = c("feedmodMB1", "feedmodMB2", "feedmodMB3", "feedmodMB4", "feedmodMB5"))



feedmodMS1 <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS1 <- Anova(feedmodMS1, type=3)


feedmodMS2 <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS2 <- Anova(feedmodMS2, type=3)

feedmodMS3 <- lmer(fec_double_adj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS3 <- Anova(feedmodMS3, type=3)


feedmodMS4 <- lmer(fec_double_adj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS4 <- Anova(feedmodMS4, type=3)

feedmodMS5 <- lmer(fec_double_adj ~ poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS5 <- Anova(feedmodMS5, type=3)

aictab(list(feedmodMS1, feedmodMS2, feedmodMS3, feedmodMS4, feedmodMS5), modnames = c("feedmodMS1", "feedmodMS2", "feedmodMS3", "feedmodMS4", "feedmodMS5"))


#going with 1 here
feedmodMS <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + #currently not giving me the layers I wanted with temp as well
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                  data = d_fe_og %>% filter(spp=="MS")) 


feedanovaMS <- Anova(feedmodMS, type=3)


feedmodMB <- lmer(fec_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + 
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                  data = d_fe_og %>% filter(spp=="MB")) 


feedanovaMB <- Anova(feedmodMB, type=3)



ggplot(d_fe_og %>% filter(spp=="MB", !(start_date=="7/17/2023")), aes(x=temp, y=fec_double_adj, shape=sex, color=start_date, group=full_ID)) + geom_line(alpha=.5, color="gray") + geom_point(alpha=.5) + facet_grid(sex~site)

library(ggeffects)

dat <- ggpredict(feedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(feedmodMB, terms=c("temp", "site", "sex"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("temp", "sex"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("temp", "site"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("site"))
plot(dat)


library(sjPlot)
plot_model(feedmodMB)





MBd <- d_fe_og %>% filter(spp=="MB")


MBd <- MBd %>% mutate(pred=predict(feedmodMB, interval="prediction"))

ggplot(MBd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=fec_double_adj), alpha=.2) + facet_grid(site~sex)

ggplot(MBd) + geom_point(aes(x=fec_double_adj, y=pred)) 

dat <- ggpredict(feedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(feedmodMS, terms=c("temp", "site", "sex"))
plot(dat)

plot_model(feedmodMB)

MSd <- d_fe_og %>% filter(spp=="MS")


MSd <- MSd %>% mutate(pred=predict(feedmodMS))

ggplot(MSd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=fec_double_adj), alpha=.2) + facet_grid(site~sex)

dat <- ggpredict(feedmodMS)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- ggpredict(feedmodMS)

dat <- predict_response(feedmodMS, terms=c("temp", "site"))
plot(dat)

dat <- predict_response(feedmodMS, terms=c("temp", "sex"))
plot(dat)

plot_model(feedmodMB)

#also want to generate estimated coefficients in a nice table
sMB <- summary(feedmodMB)
sMB <- data.frame(sMB[["coefficients"]])


sMB$Significance <- "   "
sMB$Significance[sMB$`Pr...t..`<.05] <- "*  "
sMB$Significance[sMB$`Pr...t..`<.01] <- "** "
sMB$Significance[sMB$`Pr...t..`<.001] <- "***"
sMB$`Pr...t..` <- format(sMB$`Pr...t..`, scientific=TRUE, digits=3)
sMB$`Pr...t..` <- as.character(sMB$`Pr...t..`)
sMB <- sMB %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMB, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")



feedanovaMB$Significance <- "   "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.05] <- "*  "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.01] <- "** "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.001] <- "***"
feedanovaMB$`Pr(>Chisq)` <- format(feedanovaMB$`Pr(>Chisq)`, scientific=TRUE, digits=3)
feedanovaMB$`Pr(>Chisq)` <- as.character(feedanovaMB$`Pr(>Chisq)`)
feedanovaMB <- feedanovaMB %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(feedanovaMB, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("feedanovaMB.png")





sMS <- summary(feedmodMS)
sMS <- data.frame(sMS[["coefficients"]])


sMS$Significance <- "   "
sMS$Significance[sMS$`Pr...t..`<.05] <- "*  "
sMS$Significance[sMS$`Pr...t..`<.01] <- "** "
sMS$Significance[sMS$`Pr...t..`<.001] <- "***"
sMS$`Pr...t..` <- format(sMS$`Pr...t..`, scientific=TRUE, digits=3)
sMS$`Pr...t..` <- as.character(sMS$`Pr...t..`)
sMS <- sMS %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMS, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")


feedanovaMS$Significance <- "   "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.05] <- "*  "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.01] <- "** "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.001] <- "***"
feedanovaMS$`Pr(>Chisq)` <- format(feedanovaMS$`Pr(>Chisq)`, scientific=TRUE, digits=3)
feedanovaMS$`Pr(>Chisq)` <- as.character(feedanovaMS$`Pr(>Chisq)`)
feedanovaMS <- feedanovaMS %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(feedanovaMS, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("feedanovaMS.png")

perfanova <- cbind(feedanovaMB, feedanovaMS)

options(knitr.kable.NA = '')
kable(perfanova, digits=1, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p","<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 3, "MS" = 3)) #%>% save_kable("Tables/Tab4_perfanova.png")



perfs <- cbind(sMB, sMS)

options(knitr.kable.NA = '')
kable(perfs, digits=1, col.names=c("Coeff", "SE", "df", "t", "p", "Coeff", "SE", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 5, "MS" = 5)) #%>% save_kable("Tables/Tab4_perfanova.png")




#same thing but no mass on the y axis
feedmodMB1 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB1 <- Anova(feedmodMB1, type=3)

feedmodMB1.5 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)+mass_adj)^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB1.5 <- Anova(feedmodMB1.5, type=3)

feedmodMB1.55 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 +mass_adj  + #currently not giving me the layers I wanted with temp as well
                        (1|full_ID),  
                      na.action = 'na.omit', #uncommented
                      REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                      data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB1.55 <- Anova(feedmodMB1.55, type=3)


feedmodMB2 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB2 <- Anova(feedmodMB2, type=3)

feedmodMB3 <- lmer(fec_tadj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB3 <- Anova(feedmodMB3, type=3)


feedmodMB4 <- lmer(fec_tadj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB4 <- Anova(feedmodMB4, type=3)

feedmodMB5 <- lmer(fec_tadj ~ poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MB")) 

feedanovaMB5 <- Anova(feedmodMB5, type=3)

aictab(list(feedmodMB1, feedmodMB1.5, feedmodMB1.55, feedmodMB2, feedmodMB3, feedmodMB4, feedmodMB5), modnames = c("feedmodMB1", "feedmodMB1.5", "feedmodMB1.55", "feedmodMB2", "feedmodMB3", "feedmodMB4", "feedmodMB5"))



feedmodMS1 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS1 <- Anova(feedmodMS1, type=3)




feedmodMS1.5 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)+mass_adj)^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS1.5 <- Anova(feedmodMS1.5, type=3)

feedmodMS1.55 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 +mass_adj  + #currently not giving me the layers I wanted with temp as well
                       (1|full_ID),  
                     na.action = 'na.omit', #uncommented
                     REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                     data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS1.55 <- Anova(feedmodMS1.55, type=3)

feedmodMS2 <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS2 <- Anova(feedmodMS2, type=3)

feedmodMS3 <- lmer(fec_tadj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS3 <- Anova(feedmodMS3, type=3)


feedmodMS4 <- lmer(fec_tadj ~ (poly(temp, 2)+as.ordered(as.factor(site))+as.factor(sex))^2 - as.ordered(as.factor(site)):as.factor(sex) + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS4 <- Anova(feedmodMS4, type=3)

feedmodMS5 <- lmer(fec_tadj ~ poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex)  + #currently not giving me the layers I wanted with temp as well
                     (1|full_ID),  
                   na.action = 'na.omit', #uncommented
                   REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                   data = d_fe_og %>% filter(spp=="MS")) 

feedanovaMS5 <- Anova(feedmodMS5, type=3)

aictab(list(feedmodMS1,feedmodMS1.5,feedmodMS1.55, feedmodMS2, feedmodMS3, feedmodMS4, feedmodMS5), modnames = c("feedmodMS1","feedmodMS1.5","feedmodMS1.55", "feedmodMS2", "feedmodMS3", "feedmodMS4", "feedmodMS5"))


#going with 1 here
feedmodMS <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + mass_adj + #currently not giving me the layers I wanted with temp as well
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                  data = d_fe_og %>% filter(spp=="MS")) 


feedanovaMS <- Anova(feedmodMS, type=3)


feedmodMB <- lmer(fec_tadj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + mass_adj +
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with different fixed effects
                  data = d_fe_og %>% filter(spp=="MB")) 


feedanovaMB <- Anova(feedmodMB, type=3)


library(ggeffects)

dat <- ggpredict(feedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(feedmodMB, terms=c("temp", "site", "sex"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("temp", "sex"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("temp", "site"))
plot(dat)

dat <- predict_response(feedmodMB, terms=c("site"))
plot(dat)


library(sjPlot)
plot_model(feedmodMB)





MBd <- d_fe_og %>% filter(spp=="MB")


MBd <- MBd %>% mutate(pred=predict(feedmodMB, interval="prediction"))

ggplot(MBd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=fec_tadj), alpha=.2) + facet_grid(site~sex)

ggplot(MBd) + geom_point(aes(x=fec_tadj, y=pred)) 

dat <- ggpredict(feedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(feedmodMS, terms=c("temp", "site", "sex"))
plot(dat)

plot_model(feedmodMB)

MSd <- d_fe_og %>% filter(spp=="MS")


MSd <- MSd %>% mutate(pred=predict(feedmodMS))

ggplot(MSd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=fec_tadj), alpha=.2) + facet_grid(site~sex)

dat <- ggpredict(feedmodMS)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- ggpredict(feedmodMS)

dat <- predict_response(feedmodMS, terms=c("temp", "site"))
plot(dat)

dat <- predict_response(feedmodMS, terms=c("temp", "sex"))
plot(dat)

plot_model(feedmodMB)

#also want to generate estimated coefficients in a nice table
sMB <- summary(feedmodMB)
sMB <- data.frame(sMB[["coefficients"]])


sMB$Significance <- "   "
sMB$Significance[sMB$`Pr...t..`<.05] <- "*  "
sMB$Significance[sMB$`Pr...t..`<.01] <- "** "
sMB$Significance[sMB$`Pr...t..`<.001] <- "***"
sMB$`Pr...t..` <- format(sMB$`Pr...t..`, scientific=TRUE, digits=3)
sMB$`Pr...t..` <- as.character(sMB$`Pr...t..`)
sMB <- sMB %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMB, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")



feedanovaMB$Significance <- "   "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.05] <- "*  "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.01] <- "** "
feedanovaMB$Significance[feedanovaMB$`Pr(>Chisq)`<.001] <- "***"
feedanovaMB$`Pr(>Chisq)` <- format(feedanovaMB$`Pr(>Chisq)`, scientific=TRUE, digits=3)
feedanovaMB$`Pr(>Chisq)` <- as.character(feedanovaMB$`Pr(>Chisq)`)
feedanovaMB <- feedanovaMB %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(feedanovaMB, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("feedanovaMB.png")





sMS <- summary(feedmodMS)
sMS <- data.frame(sMS[["coefficients"]])


sMS$Significance <- "   "
sMS$Significance[sMS$`Pr...t..`<.05] <- "*  "
sMS$Significance[sMS$`Pr...t..`<.01] <- "** "
sMS$Significance[sMS$`Pr...t..`<.001] <- "***"
sMS$`Pr...t..` <- format(sMS$`Pr...t..`, scientific=TRUE, digits=3)
sMS$`Pr...t..` <- as.character(sMS$`Pr...t..`)
sMS <- sMS %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMS, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")


feedanovaMS$Significance <- "   "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.05] <- "*  "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.01] <- "** "
feedanovaMS$Significance[feedanovaMS$`Pr(>Chisq)`<.001] <- "***"
feedanovaMS$`Pr(>Chisq)` <- format(feedanovaMS$`Pr(>Chisq)`, scientific=TRUE, digits=3)
feedanovaMS$`Pr(>Chisq)` <- as.character(feedanovaMS$`Pr(>Chisq)`)
feedanovaMS <- feedanovaMS %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(feedanovaMS, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("feedanovaMS.png")

perfanova <- cbind(feedanovaMB, feedanovaMS)

options(knitr.kable.NA = '')
kable(perfanova, digits=1, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p","<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 3, "MS" = 3)) #%>% save_kable("Tables/Tab4_perfanova.png")



perfs <- cbind(sMB, sMS)

options(knitr.kable.NA = '')
kable(perfs, digits=1, col.names=c("Coeff", "SE", "df", "t", "p", "Coeff", "SE", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 5, "MS" = 5)) #%>% save_kable("Tables/Tab4_perfanova.png")






#let's do the same thing for wheatgrass consumed (scaled by mass)
#going with 1 here
wgedmodMS <- lmer(wg_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + #currently not giving me the layers I wanted with temp as well
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with difwgrent fixed efwgts
                  data = d_wg_og %>% filter(spp=="MS")) 


wgedanovaMS <- Anova(wgedmodMS, type=3)


wgedmodMB <- lmer(wg_double_adj ~ (poly(temp, 3)+as.ordered(as.factor(site))+as.factor(sex))^2 + 
                    (1|full_ID),  
                  na.action = 'na.omit', #uncommented
                  #REML = FALSE, #note... REML=FALSE should be used when comparing AICs of models with difwgrent fixed efwgts
                  data = d_wg_og %>% filter(spp=="MB")) 


wgedanovaMB <- Anova(wgedmodMB, type=3)


library(ggeffects)

dat <- ggpredict(wgedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

library(sjPlot)
plot_model(wgedmodMB)




MBd <- d_wg_og %>% filter(spp=="MB")


MBd <- MBd %>% mutate(pred=predict(wgedmodMB, interval="prediction"))

ggplot(MBd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=wg_double_adj), alpha=.2) + facet_grid(site~sex)

ggplot(MBd) + geom_point(aes(x=wg_double_adj, y=pred)) 

dat <- ggpredict(wgedmodMB)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(wgedmodMB, terms=c("temp", "site", "sex"))
plot(dat)

plot_model(wgedmodMB)

MSd <- d_wg_og %>% filter(spp=="MS")


MSd <- MSd %>% mutate(pred=predict(wgedmodMS))

ggplot(MSd) + geom_point(aes(x=temp, y=pred), color="red", alpha=.2) + geom_point(aes(x=temp, y=wg_double_adj), alpha=.2) + facet_grid(site~sex)

dat <- ggpredict(wgedmodMS)
pM <- plot(dat)#, facet = TRUE)
pM

dat <- predict_response(wgedmodMS, terms=c("temp", "site", "sex"))
plot(dat)


#also want to generate estimated coefficients in a nice table
sMB <- summary(wgedmodMB)
sMB <- data.frame(sMB[["coefficients"]])


sMB$Significance <- "   "
sMB$Significance[sMB$`Pr...t..`<.05] <- "*  "
sMB$Significance[sMB$`Pr...t..`<.01] <- "** "
sMB$Significance[sMB$`Pr...t..`<.001] <- "***"
sMB$`Pr...t..` <- format(sMB$`Pr...t..`, scientific=TRUE, digits=3)
sMB$`Pr...t..` <- as.character(sMB$`Pr...t..`)
sMB <- sMB %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMB, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")



wgedanovaMB$Significance <- "   "
wgedanovaMB$Significance[wgedanovaMB$`Pr(>Chisq)`<.05] <- "*  "
wgedanovaMB$Significance[wgedanovaMB$`Pr(>Chisq)`<.01] <- "** "
wgedanovaMB$Significance[wgedanovaMB$`Pr(>Chisq)`<.001] <- "***"
wgedanovaMB$`Pr(>Chisq)` <- format(wgedanovaMB$`Pr(>Chisq)`, scientific=TRUE, digits=3)
wgedanovaMB$`Pr(>Chisq)` <- as.character(wgedanovaMB$`Pr(>Chisq)`)
wgedanovaMB <- wgedanovaMB %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(wgedanovaMB, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("wgedanovaMB.png")





sMS <- summary(wgedmodMS)
sMS <- data.frame(sMS[["coefficients"]])


sMS$Significance <- "   "
sMS$Significance[sMS$`Pr...t..`<.05] <- "*  "
sMS$Significance[sMS$`Pr...t..`<.01] <- "** "
sMS$Significance[sMS$`Pr...t..`<.001] <- "***"
sMS$`Pr...t..` <- format(sMS$`Pr...t..`, scientific=TRUE, digits=3)
sMS$`Pr...t..` <- as.character(sMS$`Pr...t..`)
sMS <- sMS %>% unite(p, c(`Pr...t..`, Significance), sep="")
options(knitr.kable.NA = '')
kable(sMS, digits=3, col.names=c("Coefficient", "Std. Error", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("hoppingcoeff.png")


wgedanovaMS$Significance <- "   "
wgedanovaMS$Significance[wgedanovaMS$`Pr(>Chisq)`<.05] <- "*  "
wgedanovaMS$Significance[wgedanovaMS$`Pr(>Chisq)`<.01] <- "** "
wgedanovaMS$Significance[wgedanovaMS$`Pr(>Chisq)`<.001] <- "***"
wgedanovaMS$`Pr(>Chisq)` <- format(wgedanovaMS$`Pr(>Chisq)`, scientific=TRUE, digits=3)
wgedanovaMS$`Pr(>Chisq)` <- as.character(wgedanovaMS$`Pr(>Chisq)`)
wgedanovaMS <- wgedanovaMS %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")
options(knitr.kable.NA = '')
kable(wgedanovaMS, digits=3, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() #%>% save_kable("wgedanovaMS.png")

perfanova <- cbind(wgedanovaMB, wgedanovaMS)

options(knitr.kable.NA = '')
kable(perfanova, digits=1, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p","<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 3, "MS" = 3)) #%>% save_kable("Tables/Tab4_perfanova.png")



perfs <- cbind(sMB, sMS)

options(knitr.kable.NA = '')
kable(perfs, digits=1, col.names=c("Coeff", "SE", "df", "t", "p", "Coeff", "SE", "df", "t", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "MB" = 5, "MS" = 5)) #%>% save_kable("Tables/Tab4_perfanova.png")


#note on rescaling and interactions: https://stackoverflow.com/questions/76308262/why-do-my-p-values-change-after-rescaling-variables-mixed-models

#note on temperature
# x = rnorm(1000)
# raw.poly = poly(x,6,raw=T)
# orthogonal.poly = poly(x,6)
# cor(raw.poly)
# cor(orthogonal.poly)


# #fullmodel of hopping... which include a polynomial to allow testtemp to go d
# hopmod <- lmer(value ~ poly(TestTemp, 2, raw = TRUE) * Sex * Site * Temperature * Photoperiod + 
#                  #(1|variable/ID/Female2), #original
#                  #(1|Female2/ID/variable), #this gave a singularity issue 
#                  (1|Female2/ID), #+ (1|variable), #so this did work -- might wanna double check it!
#                REML = FALSE, #
#                na.action = 'na.omit', data = Hop_Long)



# feedmod <- lmer(fec_adj ~ temp * factor(site) * factor(sex) + 
#                  (1|full_ID), 
#                na.action = 'na.omit', #uncommented
#                #REML = FALSE,
#                data = d_fe_og %>% filter(spp=="MB")) 
# 
# feedanova <- Anova(feedmod, type=3)
# 
# 
# 
# 
# 
# feedmod <- lmer(fec_adj ~ temp + site + sex + 
#                   (1|full_ID), 
#                 na.action = 'na.omit', #uncommented
#                 #REML = FALSE,
#                 data = d_fe_og %>% filter(spp=="MS")) 
# 
# feedanova <- Anova(feedmod, type=3)
# 
# feedmod <- lmer(fec_adj ~ as.factor(temp) * as.factor(site, ordered=TRUE) * as.factor(sex) + 
#                   (1|full_ID), 
#                 na.action = 'na.omit', #uncommented
#                 #REML = FALSE,
#                 data = d_fe_og %>% filter(spp=="MB")) 
# 
# feedanova1 <- Anova(feedmod, type=3)
# 
# 
# 
# feedmod1 <- lmer(fec_adj ~ temp * site * sex + 
#                   (1|full_ID), 
#                 na.action = 'na.omit', #uncommented
#                 #REML = FALSE,
#                 data = d_fe_og %>% filter(spp=="MB")) 
# 
# feedanova1 <- Anova(feedmod1, type=3)

#if we do it with spp, then only temp*spp comes out

#code for making table

# rownames(feedanova) <- c()
# feedanova$Significance <- "   "
# feedanova$Significance[feedanova$`Pr(>Chisq)`<.05] <- "*  "
# feedanova$Significance[feedanova$`Pr(>Chisq)`<.01] <- "** "
# feedanova$Significance[feedanova$`Pr(>Chisq)`<.001] <- "***"
# feedanova$`Pr(>Chisq)` <- format(feedanova$`Pr(>Chisq)`, scientific=TRUE, digits=3)
# feedanova$`Pr(>Chisq)` <- as.character(feedanova$`Pr(>Chisq)`)
# feedanova <- feedanova %>% unite(p, c(`Pr(>Chisq)`, Significance), sep="")


# perfanova <- cbind(hopanova, feedanova)
#
# options(knitr.kable.NA = '')
# kable(perfanova, digits=1, col.names=c("<var>&chi;<sup>2</sup></var>", "df", "p","<var>&chi;<sup>2</sup></var>", "df", "p"), escape=FALSE, format="html") %>% kable_styling() %>% add_header_above(c(" " = 1, "Hopping distance" = 3, "Leaf area consumed" = 3)) #%>% save_kable("Tables/Tab4_perfanova.png")


##make a progress bar

# when scaling up our code to fit hundreds of models, its nice to have a progress bar
# edit nls_multstart to allow for a progress bar
nls_multstart_progress <- function(formula, data = parent.frame(), iter, start_lower, 
                                   start_upper, supp_errors = c("Y", "N"), convergence_count = 100, 
                                   control, modelweights, ...){
  if(!is.null(pb)){
    pb$tick()
  }
  nls_multstart(formula = formula, data = data, iter = iter, start_lower = start_lower, 
                start_upper = start_upper, supp_errors = supp_errors, convergence_count = convergence_count, 
                control = control, modelweights = modelweights, ...)
}

#WORKING WITH FECES SCALED BY HOPPER WEIGHT AND TIME --edit just time

##need to drop a bunch of columns (I think this may be necessary for nesting)
d_fe_sc <- d_fe_og %>% mutate(rate=fec_tadj) %>% select(spp, site, sex, temp, rate) #fec_double_adj


# start progress bar and estimate time it will take
number_of_models <- 2
number_of_curves <- nrow(unique(d_fe_sc %>%select(spp, site, sex)))

# setup progress bar
pb <- progress::progress_bar$new(total = number_of_curves*number_of_models,
                                 clear = FALSE,
                                 format ="[:bar] :percent :elapsedfull")



# fit three chosen model formulation in rTPC
d_fits_fe_sc <- nest(d_fe_sc, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart_progress(rate~gaussian_1987(temp = temp, rmax,topt,a),
                                                      data = .x,
                                                      iter = c(4,4,4),
                                                      start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                                      start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                                      lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      supp_errors = 'Y',
                                                      convergence_count = FALSE)),
         # beta = map(data, ~nls_multstart_progress(rate~beta_2012(temp = temp, a, b, c, d, e),
         #                                          data = .x,
         #                                          iter = c(6,6,6,6,6),
         #                                          start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
         #                                          start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
         #                                          lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
         #                                          upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
         #                                          supp_errors = 'Y',
         #                                          convergence_count = FALSE)),
         # lrf = map(data, ~nls_multstart(rate~lrf_1991(temp = temp, rmax, topt, tmin, tmax),
         #                                data = .x,
         #                                iter = c(3,3,3,3),
         #                                start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'lrf_1991') - 10,
         #                                start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'lrf_1991') + 10,
         #                                lower = get_lower_lims(.x$temp, .x$rate, model_name = 'lrf_1991'),
         #                                upper = get_upper_lims(.x$temp, .x$rate, model_name = 'lrf_1991'),
         #                                supp_errors = 'Y',
         #                                convergence_count = FALSE)),
         # weibull = map(data, ~nls_multstart_progress(rate~weibull_1995(temp = temp, a,topt,b,c),
         #                                             data = .x,
         #                                             iter = c(4,4,4,4),
         #                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
         #                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
         #                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
         #                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
         #                                             supp_errors = 'Y',
         #                                             convergence_count = FALSE)),
         rezende = map(data, ~nls_multstart(rate~rezende_2019(temp = temp, q10, a,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'rezende_2019') - 10,
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'rezende_2019') + 10,
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'rezende_2019'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'rezende_2019'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE))#, #sharpeschool gives error... to do with tref?
         # sharpeschoolfull = map(data, ~nls_multstart(rate~sharpeschoolfull_1981(temp = temp, r_tref,e,el,tl,eh,th, tref = 15),
         #                                             data = .x,
         #                                             iter = c(4,4,4,4,4,4),
         #                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolfull_1981') - 10,
         #                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolfull_1981') + 10,
         #                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolfull_1981'),
         #                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolfull_1981'),
         #                                             supp_errors = 'Y',
         #                                             convergence_count = FALSE))
  )



# create new list column of for high resolution data
d_preds_fe_sc <- mutate(d_fits_fe_sc, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian, rezende)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  select(spp, site, sex, model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

glimpse(d_preds_fe_sc)




# plot
ggplot(d_preds_fe_sc %>% filter(spp=="MB")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  #theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g) / hr',
       title = 'Feces fitted scaled TPCs')

ggplot(d_preds_fe_sc %>% filter(spp=="MS")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  #theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g) / hr',
       title = 'Feces fitted scaled TPCs')


# stack models and calculate extra params
d_fe_sc_params <- pivot_longer(d_fits_fe_sc, names_to = 'model_name', values_to = 'fit', c(gaussian, rezende)) %>% #removed beta,lrf
  mutate(params = map(fit, calc_params)) %>%
  select(spp, site, sex, model_name, params) %>%
  unnest(params)

# stack models and calculate extra params
# stack models  
d_stack_fe_sc <- pivot_longer(d_fits_fe_sc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,lrf, rezende))

# get parameters using tidy
d_fe_sc_params <- d_stack_fe_sc %>%
  mutate(params = map(fit, calc_params)) %>%
  select(spp, site, sex, model_name, params) %>%
  unnest(params)

d_ic_fe_sc <- d_stack_fe_sc %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(spp, site, sex, model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic_fe_sc <- d_ic_fe_sc %>% group_by(spp,site, sex) %>% mutate(., weight = MuMIn::Weights(AICc))

best_models <- d_ic_fe_sc %>% group_by(spp,site, sex) %>% summarize(best_mod=model_name[AICc == min(AICc)])




#now doing the bootstrap on only Rezende
#get coefs
d_fits <- d_fits_fe_sc %>% select(-beta, -weibull, -gaussian) %>% mutate(coefs = map(rezende, coef))

# fit with nlsLM instead
d_fits <- mutate(d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ rezende_2019(temp, q10, a,b,c),
                                                            data = .x,
                                                            start = .y,
                                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'rezende_2019'),
                                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'rezende_2019'))))


d_fits <- d_fits %>% mutate(bootstrap = list(rep(NA, n())))

# run for loop to bootstrap each refitted model
for(i in 1:nrow(d_fits)){
  temp_data <- d_fits$data[[i]]
  temp_fit <- nlsLM(rate ~ rezende_2019(temp, q10, a,b,c),
                    data = temp_data,
                    start = d_fits$coefs[[i]],
                    lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'rezende_2019'),
                    upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'rezende_2019'))
  boot <- Boot(temp_fit, method = 'residual')
  d_fits$bootstrap[[i]] <- boot
  rm(list = c('temp_fit', 'temp_data', 'boot'))
}



# get the raw values of each bootstrap
d_fits <- mutate(d_fits, output_boot = map(bootstrap, function(x) x$t))

# calculate predictions with a gnarly written function 
d_fits <- d_fits %>% mutate(preds = map2(output_boot, data, function(x, y){
  y <- as.data.frame(y)
  temp <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = rezende_2019(temp, q10, a,b,c))
  return(temp)
}))


# select, unnest and calculate 95% CIs of predictions
boot_conf_preds <- select(d_fits, spp,site, sex, preds) %>%
  unnest(preds) %>%
  group_by(spp,site,sex, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            .groups = 'drop')

# ggplot() +
#   geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(model_name=="rezende"), col = 'blue') +
#   geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds, fill = 'blue', alpha = 0.3) +
#   geom_jitter(aes(temp, rate), d_fe_sc, alpha=.25, width=.5, height=0) +
#   theme_bw(base_size = 12) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Rate') +
#   facet_grid(site~sex) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Dry feces mass (mg) / hopper mass (g)',
#        title = 'Feces fitted scaled rezende TPCs')

ggplot() +
  geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MB" & model_name=="rezende"), col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MB"), fill = 'blue', alpha = 0.3) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Rate') +
  facet_grid(site~sex) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g) / hr',
       title = 'MB feces fitted scaled rezende TPCs')

ggplot() +
  geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MS" & model_name=="rezende"), col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MS"), fill = 'blue', alpha = 0.3) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Rate') +
  facet_grid(site~sex) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g) / hr',
       title = 'MS feces fitted scaled rezende TPCs')

#getting parameter bounds

# get tidied parameters using broom::tidy
# get confidence intervals of parameters
#here's where it goes wrong
d_fits3 <- d_fits %>% mutate(params = map(nls_fit, broom::tidy), #3,6? doesn't work [c(1:3, 6:12),]... [c(1:2, 4:12),]
                             cis = map(bootstrap, function(x){
                               tempo <- confint(x, method = 'perc') %>% #using perc instead... bca doesn't work
                                 as.data.frame() %>%
                                 rename(conf_lower = 1, conf_upper = 2) %>%
                                 rownames_to_column(., var = 'term')
                               return(tempo)
                             }))


# # join parameter and confidence intervals in the same dataset
# left_join(select(d_fits3, spp, site, params) %>% unnest(params),
#           select(d_fits3, spp, site, cis) %>% unnest(cis)) %>%
#   ggplot(., aes(interaction(spp,site), estimate, color=site, shape=spp)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   facet_grid(~term, scales = 'free')


#MB plot
dfits1MB <- d_fits3 %>% filter(spp=="MB")
left_join(select(dfits1MB, sex, site, params) %>% unnest(params),
          select(dfits1MB, sex, site, cis) %>% unnest(cis)) %>%
  ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~term, scales = 'free')

#table instead
dfits1MB <- d_fits3 %>% filter(spp=="MB")
ljMB <- left_join(select(dfits1MB, sex, site, params) %>% unnest(params),
          select(dfits1MB, sex, site, cis) %>% unnest(cis))

wljMB <- ljMB %>% 
  pivot_wider(names_from=term, values_from=c(estimate, std.error, statistic, p.value, conf_lower, conf_upper))

#MS plot
dfits1MS <- d_fits3 %>% filter(spp=="MS")
left_join(select(dfits1MS, sex, site, params) %>% unnest(params),
          select(dfits1MS, sex, site, cis) %>% unnest(cis)) %>%
  ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~term, scales = 'free')

#table instead
dfits1MS <- d_fits3 %>% filter(spp=="MS")
ljMS <- left_join(select(dfits1MS, sex, site, params) %>% unnest(params),
          select(dfits1MS, sex, site, cis) %>% unnest(cis))




# create empty list column
d_fits <- mutate(d_fits3, ci_extra_params = list(rep(NA, n())))

# run for loop to bootstrap extra params from each model
for(i in 1:nrow(d_fits)){
  temp_data <- d_fits$data[[i]]
  temp_fit <- nlsLM(rate ~ rezende_2019(temp, q10, a,b,c),
                    data = temp_data,
                    start = d_fits$coefs[[i]],
                    lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'rezende_2019'),
                    upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'rezende_2019'))
  boot <- Boot(temp_fit, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(temp_fit)), R = 50, method = 'case') %>%
    confint(., method = 'bca') %>%
    as.data.frame() %>%
    rename(conf_lower = 1, conf_upper = 2) %>%
    rownames_to_column(., var = 'param')
  d_fits$ci_extra_params[[i]] <- boot
  rm(list = c('temp_fit', 'temp_data', 'boot'))
}


# calculate extra params for each model and put in long format to begin with
d_fits <- mutate(d_fits, extra_params = map(nls_fit, function(x){calc_params(x) %>% pivot_longer(everything(), names_to =  'param', values_to = 'estimate')}))


# left_join(select(d_fits, site, extra_params) %>% unnest(extra_params),
#           select(d_fits, site, ci_extra_params) %>% unnest(ci_extra_params)) %>%
#   ggplot(., aes(interaction(spp,site), estimate, color=site, shape=spp)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   #  labs(y = 'estimate', x = "curve id") +
#   facet_wrap(~param, scales = 'free') +
#   labs(title = 'Calculation of confidence intervals for extra parameters')

#MB plot
d_fitsMB <- d_fits %>% filter(spp=="MB")

left_join(select(d_fitsMB, site, sex, extra_params) %>% unnest(extra_params),
          select(d_fitsMB, site, sex, ci_extra_params) %>% unnest(ci_extra_params)) %>%
  ggplot(., aes(interaction(sex, site), estimate, color=site, shape=sex)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  #  labs(y = 'estimate', x = "curve id") +
  facet_wrap(~param, scales = 'free') +
  labs(title = 'MB feces -- calculation of confidence intervals for extra parameters')

ljMB2 <- left_join(select(d_fitsMB, site, sex, extra_params) %>% unnest(extra_params),
                   select(d_fitsMB, site, sex, ci_extra_params) %>% unnest(ci_extra_params))


#MS plot
d_fitsMS <- d_fits %>% filter(spp=="MS")

left_join(select(d_fitsMS, site, sex, extra_params) %>% unnest(extra_params),
          select(d_fitsMS, site, sex, ci_extra_params) %>% unnest(ci_extra_params)) %>%
  ggplot(., aes(interaction(sex, site), estimate, color=site, shape=sex)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  #  labs(y = 'estimate', x = "curve id") +
  facet_wrap(~param, scales = 'free') +
  labs(title = 'MS feces -- calculation of confidence intervals for extra parameters')

ljMS2 <- left_join(select(d_fitsMS, site, sex, extra_params) %>% unnest(extra_params),
                   select(d_fitsMS, site, sex, ci_extra_params) %>% unnest(ci_extra_params))


#n for params is artificially large

# # let's look at the many sets of params we got from the bootstrap and do anova/linreg -- MB
# full_df <- data.frame(q10=1, a=2, b=3, c=4,spp="mm", site="d2", sex='n')
# for(i in 1:nrow(d_fitsMB)){ 
#   spp <- d_fitsMB$spp[i] 
#   site <- d_fitsMB$site[i]
#   sex <- d_fitsMB$sex[i]
#   tack_on <- as.data.frame(d_fitsMB[[8]][[i]][["t"]])
#   tack_on$spp <- spp
#   tack_on$site <- site
#   tack_on$sex <- sex
#   full_df <-rbind(full_df, tack_on)
# }
# 
# bs_params_MB <- full_df[-1,]
# 
# ggplot(bs_params_MB, aes(x=q10, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MB, aes(x=a, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MB, aes(x=b, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MB, aes(x=c, color=sex)) + geom_density() + facet_wrap(~site)
# 
# mod <- lm(q10~sex*site, bs_params_MB)
# summary(mod)
# 
# 
# # let's look at the many sets of params we got from the bootstrap and do anova/linreg -- MS
# full_df <- data.frame(q10=1, a=2, b=3, c=4,spp="mm", site="d2", sex='n')
# for(i in 1:nrow(d_fitsMS)){ 
#   spp <- d_fitsMS$spp[i] 
#   site <- d_fitsMS$site[i]
#   sex <- d_fitsMS$sex[i]
#   tack_on <- as.data.frame(d_fitsMS[[8]][[i]][["t"]])
#   tack_on$spp <- spp
#   tack_on$site <- site
#   tack_on$sex <- sex
#   full_df <-rbind(full_df, tack_on)
# }
# 
# bs_params_MS <- full_df[-1,]
# 
# ggplot(bs_params_MS, aes(x=q10, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MS, aes(x=a, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MS, aes(x=b, color=sex)) + geom_density() + facet_wrap(~site)
# 
# ggplot(bs_params_MS, aes(x=c, color=sex)) + geom_density() + facet_wrap(~site)
# 
# mod <- lm(q10~sex*site, bs_params_MS)
# summary(mod)




# #now doing the bootstrap on only Gaussian
# #get coefs
# d_fits <- d_fits_fe_sc %>% select(-beta, -weibull, -rezende) %>% mutate(coefs = map(gaussian, coef))
# 
# # fit with nlsLM instead
# d_fits <- mutate(d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
#                                                             data = .x,
#                                                             start = .y,
#                                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
#                                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'))))
# 
# d_fits <- d_fits %>% mutate(bootstrap = list(rep(NA, n())))
# 
# # run for loop to bootstrap each refitted model
# for(i in 1:nrow(d_fits)){
#   temp_data <- d_fits$data[[i]]
#   temp_fit <- nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
#                     data = temp_data,
#                     start = d_fits$coefs[[i]],
#                     lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'),
#                     upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'))
#   boot <- Boot(temp_fit, method = 'residual')
#   d_fits$bootstrap[[i]] <- boot
#   rm(list = c('temp_fit', 'temp_data', 'boot'))
# }
# 
# # get the raw values of each bootstrap
# d_fits <- mutate(d_fits, output_boot = map(bootstrap, function(x) x$t))
# 
# # calculate predictions with a gnarly written function THIS IS WHERE IT GOES WRONG
# d_fits <- d_fits %>% mutate(preds = map2(output_boot, data, function(x, y){
#   y <- as.data.frame(y)
#   temp <- as.data.frame(x) %>%
#     drop_na() %>%
#     mutate(iter = 1:n()) %>%
#     group_by_all() %>%
#     do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
#     ungroup() %>%
#     mutate(pred = gaussian_1987(temp, rmax, topt, a))
#   return(temp)
# }))
# 
# 
# # select, unnest and calculate 95% CIs of predictions
# boot_conf_preds <- select(d_fits, spp,site,sex, preds) %>%
#   unnest(preds) %>%
#   group_by(spp,site,sex, temp) %>%
#   summarise(conf_lower = quantile(pred, 0.025),
#             conf_upper = quantile(pred, 0.975),
#             .groups = 'drop')
# 
# ggplot() +
#   geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MB" & model_name=="gaussian"), col = 'blue') +
#   geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MB"), fill = 'blue', alpha = 0.3) +
#   geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
#   theme_bw(base_size = 12) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Rate') +
#   facet_grid(site~sex) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Dry feces mass (mg) / (hopper mass (g))^.9',
#        title = 'MB feces fitted scaled gaussian TPCs')
# 
# ggplot() +
#   geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MS" & model_name=="gaussian"), col = 'blue') +
#   geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MS"), fill = 'blue', alpha = 0.3) +
#   geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
#   theme_bw(base_size = 12) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Rate') +
#   facet_grid(site~sex) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Dry feces mass (mg) / (hopper mass (g))^.9',
#        title = 'MS feces fitted scaled gaussian TPCs')
# 
# #getting parameter bounds
# 
# # get tidied parameters using broom::tidy
# # get confidence intervals of parameters
# #here's where it goes wrong
# d_fits1 <- d_fits[c(1:3, 6:12),] %>% mutate(params = map(nls_fit, broom::tidy), #4,5 doesn't work 
#                                             cis = map(bootstrap, function(x){
#                                               tempo <- confint(x, method = 'bca') %>%
#                                                 as.data.frame() %>%
#                                                 rename(conf_lower = 1, conf_upper = 2) %>%
#                                                 rownames_to_column(., var = 'term')
#                                               return(tempo)
#                                             }))
# 
# 
# # join parameter and confidence intervals in the same dataset
# #MB plot
# dfits1MB <- d_fits1 %>% filter(spp=="MB")
# left_join(select(dfits1MB, sex, site, params) %>% unnest(params),
#           select(dfits1MB, sex, site, cis) %>% unnest(cis)) %>%
#   ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   facet_wrap(~term, scales = 'free')
# 
# #MS plot
# dfits1MS <- d_fits1 %>% filter(spp=="MS")
# left_join(select(dfits1MS, sex, site, params) %>% unnest(params),
#           select(dfits1MS, sex, site, cis) %>% unnest(cis)) %>%
#   ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   facet_wrap(~term, scales = 'free')
# 
# 
# # create empty list column
# d_fits <- mutate(d_fits1, ci_extra_params = list(rep(NA, n())))
# 
# # run for loop to bootstrap extra params from each model
# for(i in 1:nrow(d_fits)){
#   temp_data <- d_fits$data[[i]]
#   temp_fit <- nlsLM(rate ~ gaussian_1987(temp = temp, rmax,topt,a),
#                     data = temp_data,
#                     start = d_fits$coefs[[i]],
#                     lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'),
#                     upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'))
#   boot <- Boot(temp_fit, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(temp_fit)), R = 20, method = 'case') %>%
#     confint(., method = 'bca') %>%
#     as.data.frame() %>%
#     rename(conf_lower = 1, conf_upper = 2) %>%
#     rownames_to_column(., var = 'param')
#   d_fits$ci_extra_params[[i]] <- boot
#   rm(list = c('temp_fit', 'temp_data', 'boot'))
# }
# 
# 
# # calculate extra params for each model and put in long format to begin with
# d_fits <- mutate(d_fits, extra_params = map(nls_fit, function(x){calc_params(x) %>% pivot_longer(everything(), names_to =  'param', values_to = 'estimate')}))
# 
# #MB plot
# d_fitsMB <- d_fits %>% filter(spp=="MB")
# 
# left_join(select(d_fitsMB, sex, site, extra_params) %>% unnest(extra_params),
#           select(d_fitsMB, sex, site, ci_extra_params) %>% unnest(ci_extra_params)) %>%
#   ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   #  labs(y = 'estimate', x = "curve id") +
#   facet_wrap(~param, scales = 'free') +
#   labs(title = 'Calculation of confidence intervals for extra parameters')
# 
# 
# #MS plot
# d_fitsMS <- d_fits %>% filter(spp=="MS")
# 
# left_join(select(d_fitsMS, sex, site, extra_params) %>% unnest(extra_params),
#           select(d_fitsMS, sex, site, ci_extra_params) %>% unnest(ci_extra_params)) %>%
#   ggplot(., aes(interaction(sex,site), estimate, color=site, shape=sex)) +
#   geom_point(size = 4) +
#   geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
#   theme_bw() +
#   #  labs(y = 'estimate', x = "curve id") +
#   facet_wrap(~param, scales = 'free') +
#   labs(title = 'Calculation of confidence intervals for extra parameters')


## CAN PROB STOP HERE

#WORKING WITH FECES NOT SCALED BY HOPPER WEIGHT 

##need to drop a bunch of columns (I think this may be necessary for nesting)
d_fe_unsc <- d_fe_og %>% select(spp, site, sex, temp, rate)


# start progress bar and estimate time it will take
number_of_models <- 3
number_of_curves <- nrow(unique(d_fe_unsc %>%select(spp, site, sex)))

# setup progress bar
pb <- progress::progress_bar$new(total = number_of_curves*number_of_models,
                                 clear = FALSE,
                                 format ="[:bar] :percent :elapsedfull")

# fit three chosen model formulation in rTPC
d_fits_fe_unsc <- nest(d_fe_unsc, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart_progress(rate~gaussian_1987(temp = temp, rmax,topt,a),
                                                      data = .x,
                                                      iter = c(4,4,4),
                                                      start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                                      start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                                      lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      supp_errors = 'Y',
                                                      convergence_count = FALSE)),
         beta = map(data, ~nls_multstart_progress(rate~beta_2012(temp = temp, a, b, c, d, e),
                                                  data = .x,
                                                  iter = c(6,6,6,6,6),
                                                  start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                                  start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                                  lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                  upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                  supp_errors = 'Y',
                                                  convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart_progress(rate~weibull_1995(temp = temp, a,topt,b,c),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)))



# create new list column of for high resolution data
d_preds_fe_unsc <- mutate(d_fits_fe_unsc, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  select(spp, site, sex, model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

glimpse(d_preds_fe_unsc)




# plot
ggplot(d_preds_fe_unsc %>% filter(spp=="MB")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_unsc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg)',
       title = 'MB feces fitted unscaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')


ggplot(d_preds_fe_unsc %>% filter(spp=="MS")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_unsc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg)',
       title = 'MS feces fitted unscaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')



# stack models and calculate extra params
# stack models  
d_stack_fe_unsc <- pivot_longer(d_fits_fe_unsc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull))
  
# get parameters using tidy
d_fe_unsc_params <- d_stack_fe_unsc %>%
  mutate(params = map(fit, calc_params)) %>%
  select(spp, site, sex, model_name, params) %>%
  unnest(params)

d_ic_fe_unsc <- d_stack_fe_unsc %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(spp, site, sex,model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic_fe_unsc <- d_ic_fe_unsc %>% group_by(spp,site,sex) %>% mutate(., weight = MuMIn::Weights(AICc))

best_models <- d_ic_fe_unsc %>% group_by(spp,site,sex) %>% summarize(best_mod=model_name[AICc == min(AICc)])



#WORKING WITH WG NOT SCALED BY HOPPER WEIGHT

##need to drop a bunch of columns (I think this may be necessary for nesting)
d_wg_unsc <- d_wg_og %>% select(spp, site, sex, temp, rate)


# fit three chosen model formulation in rTPC
#trying removing the one that I think gives errors
d_wg_unsc2 <- d_wg_unsc %>% filter(spp!="MB" | site!="A1" | sex!="M")

d_fits_wg_unsc <- nest(d_wg_unsc2, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart_progress(rate~gaussian_1987(temp = temp, rmax,topt,a),
                                                      data = .x,
                                                      iter = c(4,4,4),
                                                      start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                                      start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                                      lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                      supp_errors = 'Y',
                                                      convergence_count = FALSE)),
         beta = map(data, ~nls_multstart_progress(rate~beta_2012(temp = temp, a, b, c, d, e),
                                                  data = .x,
                                                  iter = c(6,6,6,6,6),
                                                  start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                                  start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                                  lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                  upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                  supp_errors = 'Y',
                                                  convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart_progress(rate~weibull_1995(temp = temp, a,topt,b,c),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)))



# create new list column of for high resolution data
d_preds_wg_unsc <- mutate(d_fits_wg_unsc, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  select(spp, site, sex, model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

glimpse(d_preds_wg_unsc)




# plot
ggplot(d_preds_wg_unsc %>% filter(spp=="MB")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_wg_unsc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry wheatgrass mass (mg)',
       title = 'MB consumption fitted unscaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')


ggplot(d_preds_wg_unsc %>% filter(spp=="MS")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_wg_unsc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry wheatgrass mass (mg)',
       title = 'MS consumption fitted unscaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')



# stack models and calculate extra params
d_wg_unsc_params <- pivot_longer(d_fits_wg_unsc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
  mutate(params = map(fit, calc_params)) %>%
  select(spp, site, sex, model_name, params) %>%
  unnest(params)


#WORKING WITH WG SCALED BY HOPPER WEIGHT

##need to drop a bunch of columns (I think this may be necessary for nesting)
d_wg_sc <- d_wg_og %>% mutate(rate=rate/mass) %>% select(spp, site, sex, temp, rate)
d_wg_sc2 <- d_wg_sc %>% filter(spp!="MB" | site!="A1" | sex!="M")


# # fit three chosen model formulation in rTPC
# d_fits_wg_sc <- nest(d_wg_sc2, data = c(temp, rate)) %>%
#   mutate(gaussian = map(data, ~nls_multstart_progress(rate~gaussian_1987(temp = temp, rmax,topt,a),
#                                                       data = .x,
#                                                       iter = c(4,4,4),
#                                                       start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 1,
#                                                       start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 1,
#                                                       #lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
#                                                       #upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
#                                                       supp_errors = 'N',
#                                                       convergence_count = FALSE)),
#          beta = map(data, ~nls_multstart_progress(rate~beta_2012(temp = temp, a, b, c, d, e),
#                                                   data = .x,
#                                                   iter = c(6,6,6,6,6),
#                                                   start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 1,
#                                                   start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 1,
#                                                   #lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
#                                                   #upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
#                                                   supp_errors = 'N',
#                                                   convergence_count = FALSE)),
#          weibull = map(data, ~nls_multstart_progress(rate~weibull_1995(temp = temp, a,topt,b,c),
#                                                      data = .x,
#                                                      iter = c(4,4,4,4),
#                                                      start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 1,
#                                                      start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 1,
#                                                      #lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
#                                                      #upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
#                                                      supp_errors = 'N',
#                                                      convergence_count = FALSE)))
# 
# 
# 
# # create new list column of for high resolution data
# d_preds_wg_sc <- mutate(d_fits_wg_sc, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
#   # get rid of original data column
#   select(., -data) %>%
#   # stack models into a single column, with an id column for model_name
#   pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
#   # create new list column containing the predictions
#   # this uses both fit and new_data list columns
#   mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
#   # select only the columns we want to keep
#   select(spp, site, sex, model_name, preds) %>%
#   # unlist the preds list column
#   unnest(preds)
# 
# glimpse(d_preds_wg_sc)
# 
# 
# 
# 
# # plot
# ggplot(d_preds_wg_sc %>% filter(spp=="MB")) +
#   geom_line(aes(temp, .fitted, col = model_name)) +
#   geom_jitter(aes(temp, rate), d_wg_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
#   facet_grid(site~sex) +
#   theme_bw() +
#   theme(legend.position = 'none') +
#   scale_color_brewer(type = 'qual', palette = 2) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Dry wheatgrass mass (mg) / hopper mass (g)',
#        title = 'MB consumption fitted scaled TPCs',
#        subtitle = 'gaussian in green; beta in orange; weibull in blue')
# 
# 
# ggplot(d_preds_wg_sc %>% filter(spp=="MS")) +
#   geom_line(aes(temp, .fitted, col = model_name)) +
#   geom_jitter(aes(temp, rate), d_wg_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
#   facet_grid(site~sex) +
#   theme_bw() +
#   theme(legend.position = 'none') +
#   scale_color_brewer(type = 'qual', palette = 2) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Dry wheatgrass mass (mg) / hopper mass (g)',
#        title = 'MS consumption fitted scaled TPCs',
#        subtitle = 'gaussian in green; beta in orange; weibull in blue')

ggplot() +
  geom_jitter(aes(temp, rate), d_wg_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Dry wheatgrass mass (mg) / hopper mass (g)',
       title = 'MB consumption fitted scaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')


ggplot() +
  geom_jitter(aes(temp, rate), d_wg_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Dry wheatgrass mass (mg) / hopper mass (g)',
       title = 'MS consumption fitted scaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')





# # stack models and calculate extra params
# d_wg_sc_params <- pivot_longer(d_fits_wg_sc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
#   mutate(params = map(fit, calc_params)) %>%
#   select(spp, site, sex, model_name, params) %>%
#   unnest(params)


