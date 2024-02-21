library(tidyverse)
library(lme4)


dry <- read_csv("TPC/DigestionDryMasses_rough.csv")
wet <- read_csv("TPC/MB_WetMasses_rough.csv")
trial <- read_csv("TPC/MB_AllSites_rough.csv")
hoppers <- read_csv("TPC/MB_Individual_data_rough.csv")

#merge hopper data with their trials
trial_more <- merge(trial, hoppers, by="full_ID")

#merge wet and dry info
wg <- merge(wet, dry, by.x="wg_ID", by.y="sample_num")

#Wet-to-dry control (first just MB)
w2dctrl <- wg %>% filter(startsWith(wg_ID, "WG_"))
w2dctrl$wet_mass <- as.numeric(w2dctrl$wet_mass)
w2dctrl$`wg_drymass (mg)` <- as.numeric(w2dctrl$`wg_drymass (mg)`)

ggplot(w2dctrl, aes(x=wet_mass, y=`wg_drymass (mg)`)) + geom_point() + geom_smooth(method="lm") 

mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrl)
summary(mod)

#reading in wet masses for MS (to use the controls)
wet2 <- read_csv("TPC/MS_WetMasses_rough.csv")

wg2 <- merge(wet2, dry, by.x="wg_ID", by.y="sample_num")

w2dctrl2 <- wg2 %>% filter(startsWith(wg_ID, "WG_"))

w2dctrl2$wet_mass <- as.numeric(w2dctrl2$wet_mass)
w2dctrl2$`wg_drymass (mg)` <- as.numeric(w2dctrl2$`wg_drymass (mg)`)

ggplot(w2dctrl2, aes(x=wet_mass, y=`wg_drymass (mg)`, color=plot_ID)) + geom_point() #+ geom_smooth(method=lm) 

mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrl2)
summary(mod)

#gets a lot better R2 with plot ID (i.e. which container of wheat grass it came from) 
#it's sort of ordered and sort of not (because I went through the containers one by one)
#could include the (alphabetical) order of plots, 
#could also for each sample have time since purchase 
#could also look at order of samples within plots (started cutting at the edges)
#Another question -- for the best predictions of dry mass does it make sense to have a bunch of different models?
#i.e. one for each plot ID


#let's combine both
w2dctrl <- w2dctrl %>% dplyr::select(wg_ID, wet_mass, `wg_drymass (mg)`, plot_ID)
w2dctrl2 <- w2dctrl2 %>% dplyr::select(wg_ID, wet_mass, `wg_drymass (mg)`, plot_ID)

#spp is just a placeholder for time when I did this (July vs August)... 
#maybe the wheatgrass selection at the grocery store differed at these two times  
w2dctrl$spp <- "MB"
w2dctrl2$spp <- "MS"
w2d <- rbind(w2dctrl,w2dctrl2)

ggplot(w2d, aes(x=wet_mass, y=`wg_drymass (mg)`, color=spp)) + geom_point() + geom_smooth(method=lm) 


mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2d) #could look into nesting in spp... didn't seem to have an effect
summary(mod)

#is best treatment a lme with nested random effect?

#for now let's see what the size of "predict" range is

CL=.95

#these are quite wide predictions :(
predictions <- predict(mod, data.frame(wg %>% dplyr::select(wet_mass, plot_ID)), 
                      interval = "prediction", level = CL)


#add predicted dry masses
wgnpred <- cbind(wg, predictions)

#calculated range of dry mass eaten (using full CI range of predicted dry mass)
tpcmb <- wgnpred %>% mutate(eaten=fit-as.numeric(`wg_drymass (mg)`), eaten_l=lwr-as.numeric(`wg_drymass (mg)`), eaten_h=upr-as.numeric(`wg_drymass (mg)`)) #need to connect to temps

#merge with the trials (now we have temperatures)
mrg <- merge(trial_more, tpcmb, by.x="wg", by.y="wg_ID")

#if not "Y" for discolored then that means it was not discolored
mrg$`discolored (Y/N)`[is.na(mrg$`discolored (Y/N)`)] <- 'N'

#all sexes
ggplot(mrg, aes(x=temp, y=eaten)) + geom_point() + geom_smooth() 

#by sex
ggplot(mrg, aes(x=temp, y=eaten, color=sex)) + geom_point() + geom_smooth() 

#if is_valid is N(o) or M(aybe), then that means the hopper died either during or after the trial
ggplot(mrg, aes(x=temp, y=eaten, color=is_valid)) + geom_point() + geom_smooth() 

#see whether discoloration had an effect... seems like at least sometimes (see 43)
ggplot(mrg %>% filter(is.na(is_valid)), aes(x=temp, y=eaten, color=`discolored (Y/N)`=="Y")) + geom_point(alpha=.5) #+ geom_smooth() 

#removing discolored and invalid (due to hopper death) observations 
mrg2 <- mrg %>% filter(is.na(is_valid) & `discolored (Y/N)`=='N')
                       
#plot by sex and site
ggplot(mrg2, aes(x=temp, y=eaten, color=site, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("dried wg eaten w site")

#plot by sex only
ggplot(mrg2, aes(x=temp, y=eaten, color=sex)) + geom_point(alpha=.5) + geom_smooth() + ggtitle("dried wg eaten")


#divided by hopper mass
ggplot(mrg2, aes(x=temp, y=eaten/mass, color=sex)) + geom_point() + geom_smooth() + ggtitle("mass eaten / hopper mass")


#now let's see about feces vs eaten

ggplot(mrg2, aes(x=eaten, y=as.numeric(`feces_drymass (mg)`), color=temp)) + geom_point() + ggtitle("feces mass scales with mass eaten(?)")

ggplot(mrg2, aes(x=temp, y=as.numeric(`feces_drymass (mg)`)/eaten)) + geom_point(alpha=.5) + geom_smooth() + ggtitle("feces/eaten with temp")

ggplot(mrg2, aes(x=temp, y=as.numeric(`feces_drymass (mg)`))) + geom_point(alpha=.5) + geom_smooth() + ggtitle("feces with temp")

ggplot(mrg2, aes(x=temp, y=as.numeric(`feces_drymass (mg)`), color=sex)) + geom_point(alpha=.5) + geom_smooth() + ggtitle("feces with temp by sex")

ggplot(mrg2, aes(x=temp, y=as.numeric(`feces_drymass (mg)`), color=site, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("feces with temp by sex and site")



#working with mrg2 to plot upper and lower bounds -- kinda bleak
mrg2_range <- mrg2 %>% 
  pivot_longer(cols=c(eaten, eaten_l, eaten_h), names_to="CI", values_to="eaten_all")
  

ggplot(mrg2_range, aes(x=temp, y=eaten_all, color=CI, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth() + ggtitle("eaten with CI")




#seeing if lmer helps... not much difference it seems
mod_lme <- lmer(`wg_drymass (mg)` ~ wet_mass + (1|spp/plot_ID), w2d)
summary(mod_lme)

plot(mod_lme)

qqnorm(resid(mod_lme))
qqline(resid(mod_lme))  
#they fall off on the high end
wg$spp <- "MB"
p2 <- predict.merMod(mod_lme, data.frame(wg %>% select(wet_mass, spp, plot_ID)), 
        interval = "prediction", level = CL)

new_dat <- wg %>% dplyr::select(wet_mass, spp, plot_ID)

pred_ints <- predictInterval(mod_lme, 
                             newdata = new_dat, 
                             n.sims = 1000,
                             returnSims = TRUE, 
                             seed = 123, 
                             level = 0.95)

#similar fits and CIs for lmer

summar <- mrg2 %>% group_by(site, sex, temp) %>% summarize(n())


wetMS <- read_csv("TPC/MS_WetMasses_rough.csv")
trialMS <- read_csv("TPC/MS_AllSites_rough.csv")
hoppersMS <- read_csv("TPC/MS_Individual_data_rough.csv")