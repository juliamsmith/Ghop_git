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
d_fits <- mutate(d_fits, preds = map2(output_boot, data, function(x, y){
  temp <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))
  return(temp)
}))
