#soil temp modeling ideas

Tavg=20
A0=10
w=pi/12
z=.05
D=.06

t=0:24

calc0 <- Tavg+A0*exp(0/D)*sin(w*(t-8)-0/D)

calc.05 <- Tavg+A0*exp(-z/D)*sin(w*(t-8)-z/D)

calc.02 <- Tavg+A0*exp(-.02/D)*sin(w*(t-8)-.02/D)

plot(t, calc.05)

plot(t, calc0)

A.05 <- (max(calc.05)-min(calc.05))/2
calc0 <- Tavg+A.05/exp(-.05/D)*sin(w*(t-8))


#ideas -- get max and min for each day, calculate average damping and let that be the case for all days?


#nvm the above for now... trying a different strategy


#getting some NEON data

#read in key to vars
pos_key <- read_csv("NEON.D13.NIWO.DP1.00041.001.2022-07.basic.20240127T000425Z.RELEASE-2024/NEON.D13.NIWO.DP1.00041.001.sensor_positions.20221208T155132Z.csv")

file_names <- list.files("NEON.D13.NIWO.DP1.00041.001.2022-07.basic.20240127T000425Z.RELEASE-2024", pattern="*.csv") 
thirtyminobs <- file_names[str_detect(file_names, "30_minute")]
l <- length(thirtyminobs)
var_names <- rep(NA, l)
sample <- read_csv(paste0("NEON.D13.NIWO.DP1.00041.001.2022-07.basic.20240127T000425Z.RELEASE-2024/", thirtyminobs[1]))
sample <- sample %>% mutate(TOD="neither", SP="2", DepthM=5)
obs <- sample
#colnames(obs) <- c(colnames(sample), "TOD", "SP", "DepthM")
for(i in 1:l){
  name <- thirtyminobs[i]
  splt <- str_split_1(thirtyminobs[i], "\\.")
  h.v <- paste0(splt[7], ".", splt[8])
  print(h.v)
  var_name <- paste0("neonjul22.", h.v)
  var_names[l] <- var_name
  current <- read_csv(paste0("NEON.D13.NIWO.DP1.00041.001.2022-07.basic.20240127T000425Z.RELEASE-2024/", name),
                      col_types = list(soilTempMean = col_double()))
  assign(var_name, current)
  night_row <- current %>% filter(startDateTime=="2022-07-15 00:30:00")
  night_row$TOD <- "night"
  night_row$SP <- splt[7]
  night_row$DepthM <- pos_key$zOffset[pos_key$HOR.VER==h.v]
  day_row <- current %>% filter(startDateTime=="2022-07-15 12:30:00")
  day_row$TOD <- "day"
  day_row$SP <- splt[7]
  day_row$DepthM <- pos_key$zOffset[pos_key$HOR.VER==h.v]
  obs [nrow(obs) + 1,] <- night_row
  obs [nrow(obs) + 1,] <- day_row
  }

obs <- obs %>% filter(TOD!="neither" & !is.na(soilTempMean))
#choose 7/15 at 1:00 and 13:00

ggplot(obs, aes(x=soilTempMean, y=DepthM, shape=TOD, color=SP)) + geom_point()

thermcondk <- function(){
  
}

fw <- 1/(1+(theta/theta0)^(-q))

ka <- ...

kg <- ka + lambda*delta*hr*fw*rhohat*Dv/(pa-ea)

xig <- 2/(3*(1+ga*(kg/kf-1))) + 1/(3*(1+gc*(kg/kf-1)))

xiw <- 2/(3*(1+ga*(kw/kf-1))) + 1/(3*(1+gc*(kw/kf-1)))

xim <- 2/(3*(1+ga*(km/kf-1))) + 1/(3*(1+gc*(km/kf-1)))



####
soil_temperature(
  z_r.intervals = 12,
  z_r,
  z,
  T_a,
  u,
  Tsoil0,
  z0,
  SSA,
  TimeIn,
  S,
  water_content = 0.2,
  air_pressure,
  rho_so = 1620,
  shade = FALSE
)

temp_vector       <- runif(48, min = -10, max = 10)
wind_speed_vector <- runif(48, min = 0, max = 0.4)
time_vector       <- rep(1:24, 2)
solrad_vector     <- rep(c(rep(0, 6), 
                           seq(10, 700, length.out = 6), 
                           seq(700, 10, length.out = 6), 
                           rep(0, 6)),
                         2)

soil_temperature(z_r.intervals = 12, 
                 z_r           = 1.5, 
                 z             = 2, 
                 T_a           = temp_vector, 
                 u             = wind_speed_vector, 
                 Tsoil0        = 20, 
                 z0            = 0.02, 
                 SSA           = 0.7, 
                 TimeIn        = time_vector, 
                 S             = solrad_vector, 
                 water_content = 0.2, 
                 air_pressure  = 85, 
                 rho_so        = 1620, 
                 shade         = FALSE)

C23early <- C1binds %>% filter(year(dt)==2023, dt < "2023-07-09 19:31:08 MST")

ggplot(C1bind2, aes(x=sr_lt, y=sr)) + geom_point() + geom_smooth(method="lm")
mod <- lm(sr~sr_lt, C1bind2)
summary(mod) #.8841 R2

C23early <- C23early %>% mutate(sr_new=mod$coefficients[1] + sr_lt*mod$coefficients[2])

T_soilest <- soil_temperature(
  z_r.intervals = 12,
  z_r=1,
  z=2, #what do the intervals mean?
  T_a=C23early$T_1.00,
  u=C23early$ws,
  Tsoil0=C23early$T_1.00[1],
  z0=surf_r,
  SSA=.7, #guess
  TimeIn=hour(C23early$dt),
  S=C23early$sr_new,
  water_content = 0.2, #.2 here
  air_pressure=71,
  rho_so = 1620,
  shade = TRUE
)

C23early$T_soilest <- T_soilest

ggplot(C23early, aes(x=dt)) + geom_line(aes(y=T_soil), color="darkgreen") + geom_line(aes(y=T_soilest), color="lightgreen")

ggplot(C23early, aes(x=dt, y=T_soil-T_soilest)) + geom_line()

ggplot(C23early %>% filter(dt<"2023-06-18 19:31:08 MST"), aes(x=dt)) + geom_line(aes(y=T_soil), color="darkgreen") + geom_line(aes(y=T_soilest), color="lightgreen") + geom_line(aes(y=T_1.00), color="blue")

ggplot(C23early %>% filter(dt<"2023-06-18 19:31:08 MST"), aes(x=dt, y=T_soil-T_soilest)) + geom_line()


ggplot(C23early, aes(x=T_soil, y=T_soilest)) + geom_point()
mod <- lm(T_soilest~T_soil, C23early)
summary(mod) #.7954

