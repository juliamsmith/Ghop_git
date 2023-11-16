#load packages, probably overkill
library(tidyverse)
library(gsubfn)
library(lubridate)
library(TrenchR)
library(ggnewscale)
library(tibbletime)


d <- readRDS("biophys/pairedcagews.RDS")

#apply a filter for now (may be able to suss things out later)
d <- d %>% filter(!is.na(Activity))

## Looking at activity with climate data (rather than Tb) just to play around

#temperatures at which stat is observed
ggplot(d, aes(x=T_soil, color=factor(STAT))) +
  geom_density() + facet_grid(vars(Species), vars(Site))

l <- lm(T_soil~STAT, d) #significant
summary(l)



#temperatures at which feed is observed
ggplot(d, aes(x=T_soil, color=factor(FEED))) +
  geom_density() + facet_grid(vars(Species), vars(Site))

# l2 <- lm(T_soil~FEED+Species+Site, d) 
# summary(l2)
# note: this was silly -- ofc site will explain most of the temperature
l2 <- lm(T_soil~FEED, d) #not significant -- interesting
summary(l2)

#modeling notes: x and y kind of flipped rn just for ease, but should do a better job





