library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(boot)
library(car)
library(patchwork)
library(minpack.lm)

#vignette("fit_many_curves") #first just following this

# load in data
data("chlorella_tpc")
d <- chlorella_tpc

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

#merge hopper data with their trials
trials_more <- merge(trials, hoppers, by=c("full_ID","spp"))


# account for plot and wet mass
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

d_fe_og <- mrg_refinedforfeces %>% rename(rate=fecesdry)
d_fe_og$rate[is.na(d_fe_og$rate)]=0

d_wg_og <- mrg_refinedforwg %>% rename(rate=eaten) %>% filter(!is.na(temp) & !is.na(rate))


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


#WORKING WITH FECES NOT SCALED BY HOPPER WEIGHT (can skip when trying to fix bootstrap)

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


#WORKING WITH FECES SCALED BY HOPPER WEIGHT

##need to drop a bunch of columns (I think this may be necessary for nesting)
d_fe_sc <- d_fe_og %>% mutate(rate=rate/mass) %>% select(spp, site, sex, temp, rate)


# start progress bar and estimate time it will take
number_of_models <- 3
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
d_preds_fe_sc <- mutate(d_fits_fe_sc, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
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

glimpse(d_preds_fe_sc)




# plot
ggplot(d_preds_fe_sc %>% filter(spp=="MB")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g)',
       title = 'MB feces fitted scaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')


ggplot(d_preds_fe_sc %>% filter(spp=="MS")) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  facet_grid(site~sex) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g)',
       title = 'MS feces fitted scaled TPCs',
       subtitle = 'gaussian in green; beta in orange; weibull in blue')



# stack models and calculate extra params
d_fe_sc_params <- pivot_longer(d_fits_fe_sc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull)) %>%
  mutate(params = map(fit, calc_params)) %>%
  select(spp, site, sex, model_name, params) %>%
  unnest(params)

# stack models and calculate extra params
# stack models  
d_stack_fe_sc <- pivot_longer(d_fits_fe_sc, names_to = 'model_name', values_to = 'fit', c(gaussian,beta,weibull))

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
  select(spp, site, sex,model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic_fe_sc <- d_ic_fe_sc %>% group_by(spp,site,sex) %>% mutate(., weight = MuMIn::Weights(AICc))

best_models <- d_ic_fe_sc %>% group_by(spp,site,sex) %>% summarize(best_mod=model_name[AICc == min(AICc)])


#now doing the bootstrap on only Gaussian
d_fits <- d_fits_fe_sc %>% select(-beta, -weibull) %>% mutate(coefs = map(gaussian, coef))

# fit with nlsLM instead
d_fits <- mutate(d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                                                            data = .x,
                                                            start = .y,
                                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'))))

d_fits <- d_fits %>% mutate(bootstrap = list(rep(NA, n())))

# run for loop to bootstrap each refitted model
for(i in 1:nrow(d_fits)){
  temp_data <- d_fits$data[[i]]
  temp_fit <- nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                    data = temp_data,
                    start = d_fits$coefs[[i]],
                    lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'),
                    upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'))
  boot <- Boot(temp_fit, method = 'residual')
  d_fits$bootstrap[[i]] <- boot
  rm(list = c('temp_fit', 'temp_data', 'boot'))
}

# get the raw values of each bootstrap
d_fits <- mutate(d_fits, output_boot = map(bootstrap, function(x) x$t))

# # calculate predictions with a gnarly written function THIS IS WHERE IT GOES WRONG
# d_fits <- d_fits %>% mutate(preds = map2(output_boot, data, function(x, y){
#   y <- as.data.frame(y)
#   tempo <- as.data.frame(x) %>%
#     drop_na() %>%
#     mutate(iter = 1:n()) %>%
#     group_by_all() %>%
#     do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
#     ungroup() %>%
#     mutate(pred = gaussian_1987(temp, rmax, topt, a))
#   return(tempo)
# }))

##^this is approximately what there was in the vignette and I was trying to fix


#THE FOR LOOP IS WHERE IT BREAKS
#d_fits becomes blank and the console doesn't work after
d_fits$preds <- d_fits$data

for(i in 1:length(d_fits$preds)){
  x <- d_fits$output_boot[i]
  y <- d_fits$data[i]
  y <- as.data.frame(y)
  tempo <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))
  d_fits[[10]][[i]]<- tempo
  #print(as.list(as_tibble(tempo)))
}

#d_fits_fe_sc$preds[[i]] <- tempo


x <- d_fits$output_boot[1]
y <- d_fits$data[1]
y <- as.data.frame(y)
temp <- as.data.frame(x)  %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = gaussian_1987(temp, rmax, topt, a))


# select, unnest and calculate 95% CIs of predictions
boot_conf_preds <- select(d_fits, spp,site,sex, preds) %>%
  unnest(preds) %>%
  group_by(spp,site,sex, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            .groups = 'drop')

ggplot() +
  geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MB" & model_name=="gaussian"), col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MB"), fill = 'blue', alpha = 0.3) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MB"), alpha=.25, width=.5, height=0) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Rate') +
  facet_grid(site~sex) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g)',
       title = 'MB feces fitted scaled gaussian TPCs')

ggplot() +
  geom_line(aes(temp, .fitted), d_preds_fe_sc %>% filter(spp=="MS" & model_name=="gaussian"), col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot_conf_preds %>% filter(spp=="MS"), fill = 'blue', alpha = 0.3) +
  geom_jitter(aes(temp, rate), d_fe_sc %>% filter(spp=="MS"), alpha=.25, width=.5, height=0) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Rate') +
  facet_grid(site~sex) +
  labs(x = 'Temperature (ºC)',
       y = 'Dry feces mass (mg) / hopper mass (g)',
       title = 'MS feces fitted scaled gaussian TPCs')

#getting parameter bounds

# get tidied parameters using broom::tidy
# get confidence intervals of parameters
d_fits <- mutate(d_fits, params = map(nls_fit, broom::tidy),
                 cis = map(bootstrap, function(x){
                   temp <- confint(x, method = 'bca') %>%
                     as.data.frame() %>%
                     rename(conf_lower = 1, conf_upper = 2) %>%
                     rownames_to_column(., var = 'term')
                   return(temp)
                 }))

# join parameter and confidence intervals in the same dataset 
left_join(select(d_fits, spp, sex, site, params) %>% unnest(params),
          select(d_fits, spp, sex, site, cis) %>% unnest(cis)) %>%
  ggplot(., aes(interaction(spp,sex,site), estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~term, scales = 'free')


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


