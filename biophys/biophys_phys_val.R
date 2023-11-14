library(readr)
library(tidyverse)
library(gsubfn)
library(TrenchR)


#read in A1 climate data
wsda1 <- read_csv("G:/Shared drives/RoL_FitnessConstraints/projects/ReciprocalTransplant_2023/WS/A1/A1_08162023_mod.csv", col_names=TRUE) %>% select(dt, T_soil, T_0.25, T_0.50, T_0.75, T_1.00, T_1.25, ws, sr)
wsda1$site <- "A1"
wsd_less <- wsda1 %>% group_by(dt, site) %>% summarize(sr=mean(sr), 
                                                     ws=mean(ws), 
                                                     T_soil=mean(T_soil), 
                                                     T_0.25=mean(T_0.25),
                                                     T_0.50=mean(T_0.50),
                                                     T_0.75=mean(T_0.75),
                                                     T_1.00=mean(T_1.00),
                                                     T_1.25=mean(T_1.25))

wsd_less$dt <- as.POSIXct(wsd_less$dt, format="%m/%d/%Y %H:%M", tz = "MST" ) + 60*60 #plus 1hr

get_psi <- function(dt, site){
  if(site=="Eldo"){
    lat <- 39.9436
    lon <- -105.262
  } else if(site=="A1") {
    lat <- 40.015
    lon <- -105.376
  } else if(site=="B1") {
    lat <- 40.019
    lon <- -105.455
  } else {
    lat <- 40.0301
    lon <- -105.541
  }
  date <- as.POSIXct(dt, format= "%Y-%m-%d")
  hour <- as.numeric(format(as.POSIXct(dt), format="%H"))
  doy <- day_of_year(day=as.POSIXct(dt, format= "%Y-%m-%d"), format ="%Y-%m-%d")
  zenith <- zenith_angle(doy, 
                         lat, 
                         lon, 
                         hour)
  return(zenith)
}

#solve for surface roughness with a sampling of 2022 wind profile data -- COULD BE IMPROVED
surf <- mean(surface_roughness(u_r=c(0.46,	0.59,	1.18), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.03,	0,	0.95), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(1.21,	1.54,	1.57), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.82,	1.34,	1.37), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.69,	1.18,	1.37), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.52,	1.28,	1.41), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(2.81,	3.08,	3.17), zr=c(.57, .82, 1.05)),
             surface_roughness(u_r=c(0.36,	0.46,	0.82), zr=c(.57, .82, 1.05)))
#plot it
wsdlessTb <- wsd_less %>% rowwise() %>%
  mutate(bTbsoilsun_.01=Tb_grasshopper(
        T_a=air_temp_profile_neutral(T_r = T_0.25,
                                     zr  = 0.25,
                                     z0  = surf,
                                     z   = .001, # .001, #had .01 at some point
                                     T_s = T_soil), #not positive abt T_s
        T_g=T_soil, #air, #soil, #ground temp
        u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .001), #wind speed... needs work #.001 -- make ws=0 -> .001
        S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
        K_t=.7, #clearness index (???)... needs work, but for now just guessing
        psi=get_psi(dt, site), #solar zenith angle... needs work
        l=.03, #grasshopper length, rn just guessing 3cm and not varying
        Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
      )) %>%
  mutate(bTbsoilshade_.01=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .001, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .001), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0.01 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) %>%
  mutate(bTbcagesun=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .3), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  )) %>%
  mutate(bTbcageshade=Tb_grasshopper(
    T_a=air_temp_profile_neutral(T_r = T_0.25,
                                 zr  = 0.25,
                                 z0  = surf,
                                 z   = .3, # .001, #had .01 at some point
                                 T_s = T_soil), #not positive abt T_s
    T_g=T_soil, #air, #soil, #ground temp
    u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .3), #wind speed... needs work #.001 -- make ws=0 -> .001
    S=0, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
    K_t=.7, #clearness index (???)... needs work, but for now just guessing
    psi=get_psi(dt, site), #solar zenith angle... needs work
    l=.03, #grasshopper length, rn just guessing 3cm and not varying
    Acondfact=0 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
  ))

#read in physical model data

# fs <- list.files("WeatherStationData", pattern="^[0-9].*.txt")
# 
# for(i in 1:length(fs)){
#   line <- fs[i]
#   site <- strsplit(line, "\\_|\\.")[[1]][3] #note the site (will add as a column)
#   datafile <- readLines(paste0("WeatherStationData/", line)) #read file in
#   colstr <- read.pattern(text=datafile, pattern="^Date Time.*")[2,] 
#   cols <- trimws(strsplit(colstr, split=",")[[1]])
#   df <- read.pattern(text=datafile, pattern="^2022.*")
#   assign(paste0("set", i), df %>% separate(col=V1, into=cols, sep=",") %>% mutate(Site = site))
#   #tempsite <- df %>% separate(V1, into=cols, sep=",")
#   #morecols <- setdiff(allcols, cols)
# }
# 
# #split up because of a weird difference between my computer and Monica's
# for(i in 13:length(fs)){
#   line <- fs[i]
#  site <- strsplit(line, "\\_|\\.")[[1]][3] #note the site (will add as a column)
datafile <- readLines("biophys/2023-09-02_physicalmodels_mod.txt") #read file in
cols <- c("dt", "pTbcageE1", "pTbcageE2", "pTbcageW1", "pTbcageW2", 
               "pTbsoil1", "pTbsoil2", "pTbvegL") #accidentally didn't log on the second vegL phy model
df <- read.pattern(text=datafile, pattern="^2023.*")
df2 <- df %>% separate(col=V1, into=cols, sep=",")
df2$dt <- as.POSIXct(df2$dt, format="%Y/%m/%d %H:%M:%S", tz = "MST") 
A1phys <- df2 %>% filter(as.POSIXct(dt, format="%m/%d/%Y %H:%M", tz = "MST")<as.POSIXct("8/22/2023 12:53", format="%m/%d/%Y %H:%M", tz = "MST") & as.POSIXct(dt, format="%m/%d/%Y %H:%M")>as.POSIXct("8/12/2023 12:40", format="%m/%d/%Y %H:%M", tz = "MST"))    
#note this data already seems to be on CO time
A1phys$site <- "A1"
A1 <- full_join(A1phys, wsdlessTb)


A1zoom <- A1 %>% filter(as.POSIXct(dt, format="%m/%d/%Y %H:%M", tz = "MST")<as.POSIXct("8/12/2023 21:05", format="%m/%d/%Y %H:%M", tz = "MST"))

A1zoom_g <- A1zoom %>% gather("clim_var", "value", -dt, -site)
#ggplot(wsdzoom %>% filter(clim_var=="sr"), aes(x=dt, y=value, col=site)) + geom_line() +ylab("solar radiation")

A1zoom_g$value <- as.numeric(A1zoom_g$value)

#justify doing some averaging/collapsing
ggplot(A1zoom_g %>% filter(clim_var=="pTbcageE1" | 
                             clim_var=="pTbcageE2" | 
                             clim_var=="pTbcageW1" | 
                             clim_var=="pTbcageW2" | 
                             clim_var=="pTbsoil1" |
                             clim_var=="pTbsoil2" ) %>%
         na.omit(),
       aes(x=dt, y=value, col=clim_var)) +
  geom_line()

A1zoom_avg <- A1zoom %>% mutate(pTbcage=(as.numeric(pTbcageE1)+as.numeric(pTbcageE2)+as.numeric(pTbcageW1)+as.numeric(pTbcageW2))/4, 
                                pTbsoil = (as.numeric(pTbsoil1)+as.numeric(pTbsoil2))/2)
A1zoom_g_avg <- A1zoom_avg %>% gather("clim_var", "value", -dt, -site)
A1zoom_g_avg$value <- as.numeric(A1zoom_g_avg$value)

ggplot(A1zoom_g_avg %>% filter(clim_var=="pTbcage" | 
                           clim_var=="pTbsoil" |
                           clim_var=="pTbvegL" | #%>% #|
                           clim_var=="bTbsoilsun" |
                           clim_var=="T_soil") %>%
       na.omit(),
       aes(x=dt, y=value, col=clim_var)) +
  geom_line()


ggplot(A1zoom_g_avg %>% filter(clim_var=="pTbcage" | 
                                 clim_var=="pTbsoil" |
                                 clim_var=="pTbvegL" | #%>% #|
                                 clim_var=="bTbsoilsun_.01" |
                                 clim_var=="bTbsoilshade_.01" |
                                 clim_var=="bTbcagesun" |
                                 clim_var=="bTbcageshade") %>%
         na.omit(),
       aes(x=dt, y=value, col=clim_var)) +
  geom_line()



# A1zoomTb <- A1zoom %>% rowwise() %>%
#   mutate(bTbsoilsun=Tb_grasshopper(
#     wsdtb_s <- wsdzoom_s2 %>% rowwise() %>%
#       mutate(Tb_pred=Tb_grasshopper(
#         T_a=air_temp_profile_neutral(T_r = T_0.25,
#                                      zr  = 0.25,
#                                      z0  = surf,
#                                      z   = .001, # .001, #had .01 at some point
#                                      T_s = T_soil), #not positive abt T_s
#         T_g=T_soil, #air, #soil, #ground temp
#         u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001 -- make ws=0 -> .001
#         S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
#         K_t=.7, #clearness index (???)... needs work, but for now just guessing
#         psi=get_psi(dt, site), #solar zenith angle... needs work
#         l=.03, #grasshopper length, rn just guessing 3cm and not varying
#         Acondfact=.25 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
#       ))
#   ))
#   
#   
#   
#   wsdtb_s <- wsdzoom_s2 %>% rowwise() %>%
#   mutate(Tb_pred=Tb_grasshopper(
#     T_a=air_temp_profile_neutral(T_r = T_0.25,
#                                  zr  = 0.25,
#                                  z0  = surf,
#                                  z   = .001, # .001, #had .01 at some point
#                                  T_s = T_soil), #not positive abt T_s
#     T_g=T_soil, #air, #soil, #ground temp
#     u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .001), #wind speed... needs work #.001 -- make ws=0 -> .001
#     S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
#     K_t=.7, #clearness index (???)... needs work, but for now just guessing
#     psi=get_psi(dt, site), #solar zenith angle... needs work
#     l=.03, #grasshopper length, rn just guessing 3cm and not varying
#     Acondfact=.25 #0.25 -> 0 made a huge difference... but at 0 sr takes over and get really hot tbs
#   )) #%>% #note -- many more warnings if Acond=0
# # mutate(Tb_pred0.50=Tb_grasshopper(
# #   T_a=air_temp_profile_neutral(T_r = T_0.50,
# #                                zr  = 0.5,
# #                                z0  = surf,
# #                                z   = .01, # .001,
# #                                T_s = T_soil), #not positive abt T_s
# #   T_g=T_soil, #air, #soil, #ground temp
# #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
# #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
# #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
# #   psi=get_psi(dt, site), #solar zenith angle... needs work
# #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
# #   Acondfact=0 #0.25 -> 0 made a huge difference
# # )) %>%
# # mutate(Tb_pred0.75=Tb_grasshopper(
# #   T_a=air_temp_profile_neutral(T_r = T_0.75,
# #                                zr  = 0.75,
# #                                z0  = surf,
# #                                z   = .01, # .001,
# #                                T_s = T_soil), #not positive abt T_s
# #   T_g=T_soil, #air, #soil, #ground temp
# #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
# #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
# #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
# #   psi=get_psi(dt, site), #solar zenith angle... needs work
# #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
# #   Acondfact=0 #0.25 -> 0 made a huge difference
# # )) %>%
# # mutate(Tb_pred1.00=Tb_grasshopper(
# #   T_a=air_temp_profile_neutral(T_r = T_1.00,
# #                                zr  = 1,
# #                                z0  = surf,
# #                                z   = .01, # .001,
# #                                T_s = T_soil), #not positive abt T_s
# #   T_g=T_soil, #air, #soil, #ground temp
# #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
# #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
# #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
# #   psi=get_psi(dt, site), #solar zenith angle... needs work
# #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
# #   Acondfact=0 #0.25 -> 0 made a huge difference
# # )) %>%
# # mutate(Tb_pred1.25=Tb_grasshopper(
# #   T_a=air_temp_profile_neutral(T_r = T_1.25,
# #                                zr  = 1.25,
# #                                z0  = surf,
# #                                z   = .01, # .001,
# #                                T_s = T_soil), #not positive abt T_s
# #   T_g=T_soil, #air, #soil, #ground temp
# #   u=wind_speed_profile_neutral(ifelse(ws==0, .001, ws), 1, surf, .01), #wind speed... needs work #.001
# #   S=sr, #solar radiation CHANGED FROM S to H since tutorial  sensor WATTS/SQM... see about correcting for area exposed
# #   K_t=.7, #clearness index (???)... needs work, but for now just guessing
# #   psi=get_psi(dt, site), #solar zenith angle... needs work
# #   l=.03, #grasshopper length, rn just guessing 3cm and not varying
# #   Acondfact=0 #0.25 -> 0 made a huge difference, not true with .05
# # ))