#load needed packages
library(lme4)
library(nlme)
library(MuMIn)
library(tidyverse)
library(rTPC)
library(nls.multstart)
library(broom)


setwd("C:/Users/smith/Desktop/Ghop_git") #set wd to project directory

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
# 
# dryctrl <- dry %>% filter(startsWith(sample_num, "WG_"))
# 
# avgbeforedry <- mean(as.numeric(dryctrl$`wg_drymass (mg)`))
# 

#merge hopper data with their trials
trials_more <- merge(trials, hoppers, by=c("full_ID","spp"))

# 
# 
# mrg <- merge(trials_more, dry, by.x="wg", by.y="sample_num")
# 
# mrg <- mrg %>% mutate(site=factor(mrg$site, levels=c("Eldo", "A1", "B1", "C1")), wgdry=as.numeric(`wg_drymass (mg)`), temp=as.numeric(temp), fecesdry=as.numeric(`feces_drymass (mg)`), discolored=replace_na(`discolored (Y/N)`, "N"))
# 
# mrg$wgdry[mrg$wgdry<20] <- NA
# 
# ggplot(mrg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All raw dry mass") + facet_grid(site~spp)
# 
# ggplot(mrg, aes(x=temp, y=fecesdry/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("All feces dry mass") + facet_grid(site~spp)
# 
# 
# ggplot(mrg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, color=discolored, linetype=sex, shape=sex)) + geom_point(alpha=.5) + ggtitle("All raw dry mass (w/ discolored)") + facet_grid(site~spp)
# 
# mrg_refinedforwg <- mrg %>% filter(discolored=="N" & (is.na(`wg_mold (Y/N/1-4)`) | `wg_mold (Y/N/1-4)`=="N") & is.na(is_valid))
# 
# ggplot(mrg_refinedforwg, aes(x=temp, y=(avgbeforedry-wgdry)/mass, linetype=sex, shape=sex)) + geom_point(alpha=.5) + geom_smooth(se=FALSE) + ggtitle("Refined raw dry mass") + facet_grid(site~spp)
# 
# mrg_refinedforfeces <- mrg %>% filter((is.na(`feces_mold (Y/N/1-4)`) | `feces_mold (Y/N/1-4)`=="N") & is.na(is_valid))

## NOW I'd like to do account for plot and wet mass
wg <- merge(wet, dry, by.x="wg_ID", by.y="sample_num")

morew2d <- read_csv("TPC/Morew2d.csv")

morew2d <- morew2d %>% select(-ID_prime, -rng, -notes)

mod<- lm(`wg_drymass (mg)`~ 0+ wet_mass, morew2d)
summary(mod)

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

mrg_refinedforwg <- mrg %>% filter(discolored=="N" & (is.na(`wg_mold (Y/N/1-4)`) | `wg_mold (Y/N/1-4)`=="N") & is.na(is_valid))

mrg_refinedforfeces <- mrg %>% filter((is.na(`feces_mold (Y/N/1-4)`) | `feces_mold (Y/N/1-4)`=="N") & is.na(is_valid))

#for now we'll just work with means?
#attach(mrg_refinedforfeces)
#detach(mrg_refinedforfeces)

fecesmeans <- mrg_refinedforfeces %>% group_by(spp, temp, sex, site) %>% summarize(rate=mean(fecesdry, na.rm=TRUE)) %>% ungroup()

exmeans <- fecesmeans %>% filter(spp=="MB" & site=="B1" & sex=="M") %>% select(temp,rate)

exall <- mrg_refinedforfeces %>% filter(spp=="MB" & site=="B1" & sex=="M") #%>% select(temp,rate)
exall <- exall %>% mutate(rate=fecesdry) %>% select(temp,rate)
exall$rate[is.na(exall$rate)] <- 0

beta <- nest(exall, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)))

mod= beta$beta[[1]]
coef(mod)



fecesmeanslump <- mrg_refinedforfeces %>% group_by(spp, temp) %>% summarize(rate=mean(fecesdry, na.rm=TRUE)) %>% ungroup()

MBmeans <- fecesmeanslump %>% filter(spp=="MB") %>% select(temp,rate)


beta <- nest(MBmeans, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)))


mod= beta$beta[[1]]
tpc.beta = coef(mod)

plot(10:50, beta_2012(10:50, tpc.beta[1], tpc.beta[2], tpc.beta[3], tpc.beta[4], tpc.beta[5]), type="l", ylim=c(0,15))

d_stack <- dplyr::select(beta, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', beta)

# # get parameters using tidy
# params <- d_stack %>%
#   mutate(., est = map(fit, tidy)) %>%
#   dplyr::select(-fit) %>%
#   unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(MBmeans$temp), max(MBmeans$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

# plot
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), MBmeans) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)



#and now with the same but more fits
d <- MBmeans
d_fits <- nest(d, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart(rate~weibull_1995(temp = temp, a,topt,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)))

d_stack <- dplyr::select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', beta:weibull)

# # get parameters using tidy
# params <- d_stack %>%
#   mutate(., est = map(fit, tidy)) %>%
#   dplyr::select(-fit) %>%
#   unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

# plot
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), d) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

#fits well, but then again we get a lot of params to do it with

## same as above but with consumed wheatgrass
wgmeanslump <- mrg_refinedforwg %>% group_by(spp, temp) %>% summarize(rate=mean(eaten, na.rm=TRUE)) %>% ungroup()

MBmeans <- wgmeanslump %>% filter(spp=="MB") %>% select(temp,rate)

#instead showing/using all the data
d <- mrg_refinedforwg %>% filter(spp=="MB")
d$rate <- d$eaten
d_fits <- nest(d, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart(rate~weibull_1995(temp = temp, a,topt,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)))

d_stack <- dplyr::select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', beta:weibull)

# # get parameters using tidy
# params <- d_stack %>%
#   mutate(., est = map(fit, tidy)) %>%
#   dplyr::select(-fit) %>%
#   unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

# plot
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), d) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

#note to self: 2) If TPCs are being fit to averages of multiple replicates, then weighted NLLS can be used that reduce parameter bias.



#try some simpler code
spps <- c("MB", "MS")
sites <- c("Eldo", "A1", "B1", "C1")
sexes <- c("F", "M")
# spps <- c("MS")
# sites <- c("Eldo")
# sexes <- c("F")
df <- mrg_refinedforfeces

#this is a choice!!! but I think it is correct:
df$fecesdry[is.na(df$fecesdry)]=0

df$rate=df$fecesdry
betadf <- data.frame(spp="LA", site="F1", sex="N", a=1.1, b=2.2, c=3.3, d=4.4, e=5.5)
for(sp in spps){
  for(si in sites) {
    for(se in sexes){
      dfi <- df %>% filter(spp==sp & site==si & sex==se) %>% select(temp,rate)
      if(nrow(dfi)!=0){
beta <- nest(dfi, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)))

mod=beta$beta[[1]]
tpc.beta = coef(mod)
betai = data.frame(spp=sp, site=si, sex=se, a=tpc.beta[1], b=tpc.beta[2], c=tpc.beta[3], d=tpc.beta[4], e=tpc.beta[5])
betadf <- rbind(betadf,betai)
assign(paste("mod", sp, si, se, sep="_"), mod)
}}}}




for(sp in spps){
  for(si in sites) {
    betadf <- filter()
  plot(10:50, beta_2012(10:50, tpc.beta[1], tpc.beta[2], tpc.beta[3], tpc.beta[4], tpc.beta[5]), type="l", ylim=c(0,15))
  }
}


#try some simpler code
spps <- c("MB", "MS")
sites <- c("Eldo", "A1", "B1", "C1")
sexes <- c("F", "M")
# spps <- c("MS")
# sites <- c("Eldo")
# sexes <- c("F")
df <- mrg_refinedforfeces

#this is a choice!!! but I think it is correct:
df$fecesdry[is.na(df$fecesdry)]=0

df$rate=df$fecesdry
betadf <- data.frame(spp="LA", site="F1", sex="N", a=1.1, b=2.2, c=3.3, d=4.4, e=5.5)
for(sp in spps){
  for(si in sites) {
    for(se in sexes){
      dfi <- df %>% filter(spp==sp & site==si & sex==se) %>% select(temp,rate)
      if(nrow(dfi)!=0){
beta <- nest(dfi, data = c(temp, rate)) %>%
  mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)))

mod=beta$beta[[1]]
tpc.beta = coef(mod)
betai = data.frame(spp=sp, site=si, sex=se, a=tpc.beta[1], b=tpc.beta[2], c=tpc.beta[3], d=tpc.beta[4], e=tpc.beta[5])
betadf <- rbind(betadf,betai)
assign(paste("mod", sp, si, se, sep="_"), mod)
}}}}    
 
betadf <- betadf[-1,]   
    
# for(i in 1:nrow(betadf)){
#   plot(10:50, beta_2012(10:50, betadf[i,"a"], betadf[i,"b"], betadf[i,"c"], betadf[i,"d"], betadf[i,"e"]), type="l", ylim=c(0,25))  
# }


df <- mrg_refinedforfeces

#this is a choice!!! but I think it is correct:
df$fecesdry[is.na(df$fecesdry)]=0

df$rate=df$fecesdry
 
for(sp in spps){
  for(si in sites) {
      dfi <- df %>% filter(spp==sp & site==si)
      dfif <- betadf %>% filter(spp==sp & site==si &sex=="F")
      dfim <- betadf %>% filter(spp==sp & site==si & sex=="M")
      p <- ggplot(dfi) + 
        geom_jitter(aes(x=temp, y=fecesdry, color=sex), alpha=.5, width=.5, height=0)+
        xlim(10,50) + 
        geom_function(fun=beta_2012, args=list(a=dfif$a, b=dfif$b, c=dfif$c, d=dfif$d, e=dfif$e), color="red")+
        geom_function(fun=beta_2012, args=list(a=dfim$a, b=dfim$b, c=dfim$c, d=dfim$d, e=dfim$e), color="blue") +
        labs(title=paste(sp, si, "feces"))
      
      print(p)
    }
  }


#now let's fit it to /g and /hr

   
    
#now for wg -- it all works except for male A1 MB
spps <- c("MB", "MS")
sites <- c("Eldo", "A1", "B1", "C1")
sexes <- c("F", "M")
# spps <- c("MS")
# sites <- c("Eldo")
# sexes <- c("F")
df <- mrg_refinedforwg %>% filter(!is.na(temp) & !is.na(eaten))

#this is a choice!!! but I think it is correct:
df$fecesdry[is.na(df$fecesdry)]=0

df$rate=df$eaten
betadfwg <- data.frame(spp="LA", site="F1", sex="N", a=1.1, b=2.2, c=3.3, d=4.4, e=5.5)
for(sp in spps){
  for(si in sites) {
    for(se in sexes){
      dfi <- df %>% filter(spp==sp & site==si & sex==se) %>% select(temp,rate)
      if(nrow(dfi)!=0 & !(sp=="MB" & si=="A1" & se=="M")){
        beta <- nest(dfi, data = c(temp, rate)) %>%
          mutate(beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                                 data = .x,
                                                 iter = c(6,6,6,6,6),
                                                 start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                                 start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                                 lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                 upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                                 supp_errors = 'Y',
                                                 convergence_count = FALSE)))
        
        mod=beta$beta[[1]]
        tpc.beta = coef(mod)
        betai = data.frame(spp=sp, site=si, sex=se, a=tpc.beta[1], b=tpc.beta[2], c=tpc.beta[3], d=tpc.beta[4], e=tpc.beta[5])
        betadfwg <- rbind(betadfwg,betai)
        assign(paste("wgmod", sp, si, se, sep="_"), mod)
      }}}}    


betadfwg <- betadfwg[-1,]



for(sp in spps){
  for(si in sites) {
    dfi <- df %>% filter(spp==sp & site==si)
    dfif <- betadfwg %>% filter(spp==sp & site==si &sex=="F")
    dfim <- betadfwg %>% filter(spp==sp & site==si & sex=="M")
    p <- ggplot(dfi) + 
      geom_jitter(aes(x=temp, y=eaten, color=sex), alpha=.5, width=.5, height=0)+
      xlim(10,50) + 
      geom_function(fun=beta_2012, args=list(a=dfif$a, b=dfif$b, c=dfif$c, d=dfif$d, e=dfif$e), color="red")+
      geom_function(fun=beta_2012, args=list(a=dfim$a, b=dfim$b, c=dfim$c, d=dfim$d, e=dfim$e), color="blue") +
      labs(title=paste(sp, si, "consumed wg"))
    
    print(p)
  }
}


