#start with fdalb, run through line 256 (actually a bit beyond) of behavior_analysis.R to get there
#(see additional analysis currently at the end of that file... this was me trying to check assumptions and then do it tidily but I didn't get far)

#test the assumptions of multinomial logistic regression https://resources.nu.edu/statsresources/Multinomiallogistic

##independence of observations -- not quite bc same hoppers being returned to

##categories of outcome var are mutuall exlusive and exhaustive -- yup, good enough

##no multicolinearity between independent variables (Site, Temp, Species... there's gonna be some)

##no outliers or highly influential points

##linear relationship -- see these two sources
#https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
#https://bookdown.org/sarahwerth2024/CategoricalBook/logistic-regression-r.html (tried to apply assumption testing to MLR)

library(nnet)

# Select only outcome and numeric predictors
# drop missing and save in new dataset
fit_full <- test <- multinom(Activity2 ~ T_1.00+Species+Site, data = fdalb)

df_model <- fdalb %>% 
  select(T_1.00, Species, Site) %>% # enter numeric variables 
  drop_na(T_1.00, Species, Site) # drop cases with missing values on these variables. 

# save names of predictors to plug into command below. 
predictors <- colnames(df_model) 

# Save predicted probabilities
df_model$probabilities <- fit_full$fitted.values

# Manually calculate the logit values and tidy data for the plot
df_model <- df_model %>%
  mutate(logitF = log(probabilities[,"FEED"]/probabilities[,"STAT"])) %>%
  select(-probabilities) %>% 
  gather(key = "predictors", value = "predictor.value", -logitF) 

ggplot(df_model, aes(y = logitF, x = predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")

#aha it doesn't get that the airtemps are numeric -- trying with just airtemp for a sec
#is the way the data looks bad?
fit_full <- multinom(Activity2 ~ T_1.00+Species+Site, data = fdalb) #if I remove other predictors here the line looks suspiciously perfect

df_model <- fdalb %>% 
  select(T_1.00) %>% # enter numeric variables 
  drop_na(T_1.00) # drop cases with missing values on these variables. 

# save names of predictors to plug into command below. 
predictors <- colnames(df_model) 

# Save predicted probabilities
df_model$probabilities <- fit_full$fitted.values

# Manually calculate the logit values and tidy data for the plot
df_model <- df_model %>%
  mutate(logitF = log(probabilities[,"FEED"]/probabilities[,"STAT"])) %>%
  select(-probabilities) %>% 
  gather(key = "predictors", value = "predictor.value", -logitF) 

ggplot(df_model, aes(y = logitF, x = predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")


#So I can't tell
