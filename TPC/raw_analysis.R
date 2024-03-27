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

#read in w2d see wettodrylm
setwd("G:/Shared drives/RoL_FitnessConstraints/projects/DigestionTPC")
w2d <- read_csv("Data/WetToDryJune2023.csv")

setwd("C:/Users/smith/Desktop/Ghop_git/TPC")

w2dadd <- w2d %>% select(-WG_num) %>% rename(wet_mass="Wet_mass_g", `wg_drymass (mg)`="Dry_mass_mg", plot_ID="change_q")

w2dctrlmore <- rbind(w2dctrl %>% select(wet_mass, `wg_drymass (mg)`, plot_ID), w2dadd)

ggplot(w2dctrlmore, aes(x=wet_mass, y=`wg_drymass (mg)`)) + geom_point() + geom_smooth(method="lm") 

mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrlmore)
summary(mod)


ggplot(w2dctrl, aes(x=wet_mass, y=`wg_drymass (mg)`)) + geom_point() + geom_smooth(method="lm") 

mod2<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrl)
summary(mod2)

morew2d <- read_csv("Morew2d.csv")

morew2d <- morew2d %>% select(-ID_prime, -rng, -notes)

w2dctrlmoremore <- rbind(morew2d %>% select(wet_mass, `wg_drymass (mg)`, plot_ID), w2dctrlmore)

ggplot(w2dctrlmoremore, aes(x=wet_mass, y=as.numeric(`wg_drymass (mg)`))) + geom_point() + geom_smooth(method="lm") 

ggplot(morew2d,aes(x=wet_mass, y=as.numeric(`wg_drymass (mg)`), color=plot_ID)) + geom_point() + geom_smooth(method="lm")

ggplot(morew2d,aes(x=wet_mass, y=as.numeric(old_dry), color=plot_ID)) + geom_point() + geom_smooth(method="lm")

mod<- lm(`wg_drymass (mg)`~ wet_mass + plot_ID, w2dctrlmoremore)
summary(mod)

mod<- lm(`wg_drymass (mg)`~ 0 + wet_mass + plot_ID, w2dctrlmoremore) #trying to force it through 0
summary(mod)

ggplot(morew2d,aes(x=wet_mass, y=as.numeric(`wg_drymass (mg)`), color=plot_ID)) + geom_point() + geom_smooth(method="lm",formula=y~0+x)

ggplot(w2dctrlmoremore, aes(x=wet_mass, y=as.numeric(`wg_drymass (mg)`), color=plot_ID)) + geom_point() + geom_smooth(method="lm",formula=y~0+x)

newdf <- data.frame(wg %>% select(wet_mass, plot_ID))
newdf$wet_mass <- as.numeric(newdf$wet_mass)
predictions <- predict(mod, newdf, 
                       interval = "prediction", level = .95) #+/- 15mg
predictions2 <- predict(mod2, newdf, 
                        interval = "prediction", level = .95) #also +/- 15, general range of predictions is p similar

#geom_smooth(method="lm",formula=y~0+x)


#these are quite wide predictions :(
newdf <- data.frame(wg %>% select(wet_mass, plot_ID))
newdf$wet_mass <- as.numeric(newdf$wet_mass)
predictions <- predict(mod, newdf, 
                       interval = "prediction", level = .95)
predictions2 <- predict(mod2, newdf, 
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
ggplot(mrg_refinedforwg, aes(x=temp, y=(eaten-fecesdry)/eaten, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("Refined assimilation") + facet_grid(site~spp)

#note... rule of <40 and see how it looks then 
ggplot(mrg_refinedforwg %>% filter(fecesdry/eaten<40), aes(x=temp, y=(eaten-fecesdry)/eaten, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined assimilation") + facet_grid(site~spp)

mrg_refinedforwg <- mrg_refinedforwg %>% filter(!is.na(fecesdry), !is.na(eaten))

ARs <- mrg_refinedforwg %>% group_by(spp, site, temp) %>% summarize(AR=mean((eaten-fecesdry)/eaten))

# library(devtools)
# install_github("mdjbru-R-packages/thermPerf")
library(thermPerf)

#adding 0 at CTmax doesn't fix it... possibly makes it worse
inputs <- data.frame(spp=rep(NA, 16), site=rep(NA, 16), sex=rep(NA, 16))
i=0
CTmax=50
for(sppi in c("MS", "MB")) {
  for(sitei in c("Eldo", "A1", "B1", "C1")) {
    for(sexi in c("M", "F")) {
      i=i+1
      inputs$spp[i] <- spp
      inputs$site[i] <- site
      inputs$sex[i] <- sex
      dat <- mrg_refinedforwg %>% filter(site==sitei & spp==sppi & sex==sexi)
      if(dim(dat)[1]!=0) {
        fits = fitModels(models, c(as.numeric(dat$temp), CTmax), c(as.numeric(dat$fecesdry), 0))
        plot(fits, main=paste(sppi, sexi, sitei))
      }
    }
  }
}




fits = fitModels(getModelLibrary(), temp, growth)
plot(fits)

library(scales)


f <- rescale(as.numeric(dat$fecesdry), to=c(0,1))
fits = fitModels(getModelLibrary(), dat$temp, f)

s <- dat %>% group_by(temp) %>% summarize(m=mean(fecesdry))
fits = fitModels(getModelLibrary(), s$temp, s$m)



mFunction = function(x, params) {
  # params model parameters, a0, a1, a2, a3
  alpha = params[["alpha"]]
  beta = params[["beta"]]
  gam = params[["gam"]]
  b = params[["b"]]
  c = params[['c']]
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}
mName="Gamma function TPC"
mFormula = y~(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
mParams=c('alpha', 'beta', 'gam', 'b','c')
mStarting=list(alpha=0, beta=.3, gam=0.7, b=55,c=1)
myModel=buildModel(mFunction, mName, mFormula, mParams, mStarting)

# Fit the model, along with some models from the model library
models = getModelLibrary()[c("linearFit")]
models[["myModel"]] = myModel

temp = c(16,23,30,37,40,43, 50)
growth = c(0.2,2.1,4.6,8.6,8.4,5.9,0)

female_fits = fitModels(models, temp,growth)
female_fits$myModel #this shows fitted parameters of the curve
plot(female_fits)