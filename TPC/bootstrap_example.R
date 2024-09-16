# load packages
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)

##from here
#vignette("bootstrapping_models")



# load in data
data("chlorella_tpc")

# keep just a single curve
d <- filter(chlorella_tpc, curve_id <= 3)

# fit
d_fits <- nest(d, data = c(rate, temp)) %>%
  mutate(gaussian = map(data, ~nls_multstart(rate ~ gaussian_1987(temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(3,3,3),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 1,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 1,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)))

# create high resolution predictions
d_preds <- mutate(d_fits, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  select(., -data) %>%
  mutate(preds = map2(gaussian, new_data, ~augment(.x, newdata = .y))) %>%
  select(curve_id, growth_temp, process, flux, preds) %>%
  unnest(preds)

# # show the data
# ggplot(d, aes(temp, rate)) +
#   geom_point(size = 2) +
#   geom_line(aes(temp, .fitted), d_preds) +
#   theme_bw(base_size = 12) +
#   labs(x = 'Temperature (ºC)',
#        y = 'Metabolic rate',
#        title = 'Metabolic rate across temperatures') +
#   facet_wrap(~curve_id)


d_fits <- mutate(d_fits, coefs = map(gaussian, coef))

# fit with nlsLM instead
d_fits <- mutate(d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                                                            data = .x,
                                                            start = .y,
                                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'))))


# create empty list column
d_fits <- mutate(d_fits, bootstrap = list(rep(NA, n())))


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

d_fits <- mutate(d_fits, output_boot = map(bootstrap, function(x) x$t))

# calculate predictions with a gnarly written function
x <-d_fits$output_boot[1]
y <- d_fits$data[1]
y<-as.data.frame(y)
  tempo <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))





# calculate predictions with a gnarly written function
d_fits <- mutate(d_fits, preds = map2(output_boot, data, function(x, y){
  y<-as.data.frame(y)
  temp <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))
  return(temp)
}))

#d_fits$preds <- d_fits$data

pars <- list()

for(i in 1:nrow(d_fits)){
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
  pars[[i]] <- tempo
  #d_fits[[11]][[i]]<- tempo
  #print(as.list(as_tibble(tempo)))
}

my_dataframe <- do.call(data.frame, my_list)
d_fits$preds <- pars

i <- 1
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
#d_fits[[11]][[i]]<- tempo


# select, unnest and calculate 95% CIs of predictions
boot_conf_preds <- select(d_fits, curve_id, preds) %>%
  unnest(preds) %>%
  group_by(curve_id, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            .groups = 'drop')

d_fits <- mutate(d_fits, params = map(nls_fit, broom::tidy),
                 cis = map(bootstrap, function(x){
                   temp <- confint(x, method = 'bca') %>%
                     as.data.frame() %>%
                     rename(conf_lower = 1, conf_upper = 2) %>%
                     rownames_to_column(., var = 'term')
                   return(temp)
                 }))

# join parameter and confidence intervals in the same dataset 
left_join(select(d_fits, curve_id, growth_temp, flux, params) %>% unnest(params),
          select(d_fits, curve_id, growth_temp, flux, cis) %>% unnest(cis)) %>%
  ggplot(., aes(curve_id, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~term, scales = 'free')
#> Joining with `by = join_by(curve_id, growth_temp, flux, term)`