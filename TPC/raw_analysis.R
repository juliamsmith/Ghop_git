library(tidyverse)
library(lme4)


dry <- read_csv("TPC/DigestionDryMasses_rough.csv")
wetMB <- read_csv("TPC/MB_WetMasses_rough.csv")
wetMS <- read_csv("TPC/MS_WetMasses_rough.csv")
trialsMB <- read_csv("TPC/MB_AllSites_rough.csv")
trialsMS <- read_csv("TPC/MS_AllSites_rough.csv")
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

wet <- rbind(wetMS, wetMB)

dryctrl <- dry %>% filter(startsWith(sample_num, "WG_"))

avgbeforedry <- mean(as.numeric(dryctrl$`wg_drymass (mg)`))

#merge hopper data with their trials
trials_more <- merge(trials, hoppers, by=c("full_ID","spp"))


mrg <- merge(trials_more, dry, by.x="wg", by.y="sample_num")

mrg <- mrg %>% mutate(site=factor(mrg$site, levels=c("Eldo", "A1", "B1", "C1")), wgdry=as.numeric(`wg_drymass (mg)`), temp=as.numeric(temp), fecesdry=as.numeric(`feces_drymass (mg)`), discolored=replace_na(`discolored (Y/N)`, "N"))

mrg$wgdry[mrg$wgdry<20] <- NA

ggplot(mrg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All raw dry mass") + facet_grid(site~spp)

ggplot(mrg, aes(x=temp, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All feces dry mass") + facet_grid(site~spp)


ggplot(mrg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, color=discolored, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("All raw dry mass (w/ discolored)") + facet_grid(site~spp)

mrg_refinedforwg <- mrg %>% filter(discolored=="N" & (is.na(`wg_mold (Y/N/1-4)`) | `wg_mold (Y/N/1-4)`=="N") & is.na(is_valid))

ggplot(mrg_refinedforwg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined raw dry mass") + facet_grid(site~spp)

mrg_refinedforfeces <- mrg %>% filter((is.na(`feces_mold (Y/N/1-4)`) | `feces_mold (Y/N/1-4)`=="N") & is.na(is_valid))

ggplot(mrg_refinedforfeces, aes(x=temp, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined feces dry mass") + facet_grid(site~spp)


#one more thing to consider -- any errors there may have been in duration
ggplot(mrg_refinedforwg, aes(x=temp, color=start_date, y=(avgbeforedry-wgdry)/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined raw dry mass colored by trial") + facet_grid(site~spp)

ggplot(mrg_refinedforfeces, aes(x=temp, color=start_date, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined feces dry mass colored by trial") + facet_grid(site~spp)

#assimilation
ggplot(mrg_refinedforwg, aes(x=temp, y=fecesdry/(avgbeforedry-wgdry), linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined assimilation") + facet_grid(site~spp)

#note... could put in the range of -5 to 5 and seeh ow it looks then 
ggplot(mrg_refinedforwg %>% filter(fecesdry/(avgbeforedry-wgdry)>-5 & fecesdry/(avgbeforedry-wgdry)<5), aes(x=temp, y=fecesdry/(avgbeforedry-wgdry), linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined assimilation") + facet_grid(site~spp)




## NOW I'd like to do account for plot and wet mass
wg <- merge(wet, dry, by.x="wg_ID", by.y="sample_num")

#Wet-to-dry control (first just MB)
w2dctrl <- wg %>% filter(startsWith(wg_ID, "WG_"))
w2dctrl$wet_mass <- as.numeric(w2dctrl$wet_mass)
w2dctrl$`wg_drymass (mg)` <- as.numeric(w2dctrl$`wg_drymass (mg)`)
w2dctrl$wet_mass <- as.numeric(w2dctrl$wet_mass)
w2dctrl$wet_mass[!is.numeric(w2dctrl$wet_mass)] <- NA

ggplot(w2dctrl, aes(x=wet_mass, y=`wg_drymass (mg)`)) + geom_point() + geom_smooth(method="lm") 

mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrl)
summary(mod)

#these are quite wide predictions :(
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

mrg <- mrg %>% mutate(site=factor(mrg$site, levels=c("Eldo", "A1", "B1", "C1")),fecesdry=as.numeric(mrg$`feces_drymass (mg)`), discolored=replace_na(`discolored (Y/N)`, "N"), temp=as.numeric(temp))

ggplot(mrg, aes(x=temp, y=eaten/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All eaten dry mass") + facet_grid(site~spp)

ggplot(mrg, aes(x=temp, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All feces dry mass") + facet_grid(site~spp)


ggplot(mrg, aes(x=temp, y=eaten/mass, color=discolored, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("All eaten dry mass (w/ discolored)") + facet_grid(site~spp)

mrg_refinedforwg <- mrg %>% filter(discolored=="N" & (is.na(`wg_mold (Y/N/1-4)`) | `wg_mold (Y/N/1-4)`=="N") & is.na(is_valid))

ggplot(mrg_refinedforwg, aes(x=temp, y=eaten/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined eaten dry mass") + facet_grid(site~spp)

mrg_refinedforfeces <- mrg %>% filter((is.na(`feces_mold (Y/N/1-4)`) | `feces_mold (Y/N/1-4)`=="N") & is.na(is_valid))

ggplot(mrg_refinedforfeces, aes(x=temp, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined feces dry mass") + facet_grid(site~spp)


#one more thing to consider -- any errors there may have been in duration
ggplot(mrg_refinedforwg, aes(x=temp, color=start_date, y=eaten/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined eaten dry mass colored by trial") + facet_grid(site~spp)

ggplot(mrg_refinedforfeces, aes(x=temp, color=start_date, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined feces dry mass colored by trial") + facet_grid(site~spp)

#assimilation
ggplot(mrg_refinedforwg, aes(x=temp, y=fecesdry/eaten, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined assimilation") + facet_grid(site~spp)

#note... rule of <40 and see how it looks then 
ggplot(mrg_refinedforwg %>% filter(fecesdry/eaten<40), aes(x=temp, y=fecesdry/eaten, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined assimilation") + facet_grid(site~spp)

mrg_refinedforwg <- mrg_refinedforwg %>% filter(!is.na(fecesdry), !is.na(eaten))

ARs <- mrg_refinedforwg %>% group_by(spp, site, temp) %>% summarize(AR=mean(fecesdry/eaten))