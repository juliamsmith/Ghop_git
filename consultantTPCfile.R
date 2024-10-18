library(tidyverse)
library(brms)

d_fe_og <- read_csv("TPCforconsultants.csv")

#columns: species, feeding temperature, individual grasshopper ID, 
#site of origin, sex, mass of hopper (g), mass of feces (mg), eating time (hr),
#elevation of site of origin (m)

#make column fec_tadj: calculate the mg feces / hr eat time
d_fe_og <- d_fe_og %>% mutate(fec_tadj = fec/as.numeric(eat_time))

#adjust for brms
d_fe_og <- d_fe_og %>% mutate(sex=case_match(sex, "F"~-0.5, "M"~0.5))

d_fe_og <- d_fe_og %>% mutate(fec_tadj=ifelse(fec_tadj==0, .001, fec_tadj))

