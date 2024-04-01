

#load required packages
library(raster)
library(TrenchR)
library(microclima)
library(NicheMapR)
library(maptools)
library(plyr)
library(thermPerf)


##### Fitting thermal performance curves #####
#(using curve from Asbury and Angilletta 2010 paper, AmNat)
#see analysis3.0 files for info on statistical models and most likely means of consuption, feces at each temp
#assuming mass of female is .656 g, which is the artithmetic mean of all females Mb in experiment
#assuming mass of male is 0.555 g, for same as above

#fit TPC for feces production assuming average consumption of female
#ontaining TPC parameters with nonlinear function using themrperf which uses nls()
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

temps=c(27, 33 ,40, 46, 50.6)

#below values of feces production came from different assumptions about consumption
#assuming consumption changes with temperature based on multi-model means
female_feces=c(.0194, .0287, .0446, .0291,0)*1000/6
male_feces=c(.0230, .0303, .0406, .0330,0)*1000/6

#assuming consumption is always arithmetic mean (null)
male_feces_nullcons=c(.0232,.0286,.0373,.0340,0)*1000/6
female_feces_nullcons=c(.0202, .0267, .0346, .0324,0)*1000/6

#assuming consumption is higher (arithmetic mean + sd)
female_feces_plussd=c(.0300, .0442, .0652, .0505, 0)*1000/6
male_feces_plussd = c(.0256, .0390,.0583, .0451, 0)*1000/6

#assuming consumption is lower (arithmetic mean - sd)
female_feces_minussd = c(.0108, .0093, .0066, .0143, 0)*1000/6
male_feces_minussd = c(.0208, .0182, .0164, .0230, 0)*1000/6

# Fit the model, along with some models from the model library
models = getModelLibrary()[c("linearFit")]
models[["myModel"]] = myModel

female_fits = fitModels(models, temps,female_feces)
female_fits$myModel #this shows fitted parameters of the curve

male_fits = fitModels(models, temps,male_feces)
male_fits$myModel

nullcons_female_fits=fitModels(models, temps, female_feces_nullcons)
nullcons_female_fits$myModel

nullcons_male_fits=fitModels(models, temps, male_feces_nullcons)
nullcons_male_fits$myModel

female_fits_plussd=fitModels(models, temps, female_feces_plussd)
female_fits_plussd$myModel

male_fits_plussd=fitModels(models, temps, male_feces_plussd)
male_fits_plussd$myModel

female_fits_minussd=fitModels(models, temps, female_feces_minussd)
female_fits_minussd$myModel

male_fits_minussd=fitModels(models, temps, male_feces_minussd)
male_fits_minussd$myModel


#plot fit to check realism
plot(female_fits, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(male_fits, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(nullcons_female_fits, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(nullcons_male_fits, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Feces produced (g)')

plot(female_fits_plussd, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(male_fits_plussd, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(female_fits_minussd, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')

plot(male_fits_minussd, xlim=c(-10,55),ylim=c(0,10), xlab='Temperature (deg. C)',
     ylab='Defecation rate (mg/hr)')


############plotting all fitted TPCs for males and females ###############

#define function that calls on the previously obtained parameters of the nonlinear function
female_fit_tpc = function(x,alpha=-2.3942, beta=0.1165,gam=0.7342, b=52.9942,c=2.5707) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

male_fit_tpc = function(x,alpha=-2.0885, beta=0.1693 ,gam=0.7089, b=52.6885,c=2.9125) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

nullcons_female_fit_tpc = function(x,alpha=-2.0124, beta=0.1809 ,gam=0.7154, b=52.6124,c=2.6115) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

nullcons_male_fit_tpc = function(x,alpha=-2.0910, beta=0.1795,gam=0.7117, b=52.6910,c=2.8101) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}


plussd_female_fit_tpc = function(x,alpha=-2.1085, beta=0.1388,gam=0.7306, b=52.7085,c=4.1322) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

plussd_male_fit_tpc = function(x,alpha=-2.1060, beta=0.1339,gam=0.7346, b=52.7060,c=3.6241) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

minussd_female_fit_tpc = function(x,alpha=-16.6869, beta=0.2443,gam=0.6881, b=67.2869,c=0.9626) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

minussd_male_fit_tpc = function(x,alpha=-2.1585, beta=0.2204,gam=0.6875, b=52.7585,c=1.7407) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

nullcons_female_fit_tpc_cold = function(x,alpha=-4.0124, beta=0.1809 ,gam=0.7154, b=52.6124,c=2.6115) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}

nullcons_female_fit_tpc_hot = function(x,alpha=-0.0124, beta=0.1809 ,gam=0.7154, b=52.6124,c=2.6115) {
  # params model parameters, a0, a1, a2, a3
  return(c*(((x-alpha)/b)^((gam/beta)-1)*(1-(x-alpha)/b)^(((1-gam)/beta)-1)*gamma(1/beta))/(gamma(gam/beta)*gamma((1-gam)/beta)))
}


#test function
temp=seq(0,55,.1)
temps=c(27, 33 ,40, 46, 50.6)


#Plotting Figure S3
windows()
par(mfrow=c(1,2))
plot(nullcons_male_fit_tpc(temp)~temp,type='l', lwd=3, ylab='Defecation rate (mg/hr)',col='black', xlab='Tb', ylim =c(0,20))
lines(plussd_male_fit_tpc(temp)~temp, type='l',lty='dashed',col='black', lwd=3)
lines(minussd_male_fit_tpc(temp)~temp, type='l',lty='dotted',lwd=3)
plot(nullcons_female_fit_tpc(temp)~temp,type='l', lwd=3, ylab='Defecation rate (mg/hr)',col='red', xlab='Tb', ylim =c(0,20))
lines(plussd_female_fit_tpc(temp)~temp, col='red',lty='dashed',lwd=3)
lines(minussd_female_fit_tpc(temp)~temp, col='red',lty='dotted',lwd=3)


########### Running the mechanistic model for current climates ################

#read in the coordinates of all pixels in map at res of approx 19km2 per pixel
predictors=stack("macroclimatic predictors for SDMs.grd")

#setting study area extent
e=extent(c(-78.16667, -50, -40, -15))
predictors=crop(predictors, e)

#pulling the geographic coordinates from the center of each cell in the predictors
samp <- sampleRandom(predictors$bio1, ncell(predictors$bio1), xy = TRUE, sp=TRUE, na.rm = TRUE)
grid=data.frame(samp@coords)


#Before running the model you must modify the Tb_grasshopper function.
# The modified version of the function is titled "modified Tb_grasshopper function.txt"
fix(Tb_grasshopper)

#set up empty vector for model outputs
model_output=vector()
operative=vector()

#model will take several hours to run through all 17568 grid cells
for (i in 1:length(grid$x)) {
  #downloading average day of average month hourly microclimate data from NicheMapr (see New et al. 2002 for climate layers; Kearney et al. 2014/2017/2020 for model info)
  start_time <- Sys.time()
  loc=c(grid$x[i],grid$y[i])#long, lat
  micro=micro_global(loc=loc,  Usrhyt = .01, minshade=0, maxshade=90)

  metout=data.frame(micro$metout)
  shadmet=data.frame(micro$shadmet)

  metout=data.frame(cbind(micro$metout, difprop=difprop(rad=metout$SOLR,julian = metout$DOY, localtime=metout$TIME/60, lat=grid$y[i], long=grid$x[i] , hourly = T, watts=T,dst=-3)))
  shadmet=data.frame(cbind(micro$shadmet, difprop=difprop(rad=shadmet$SOLR,julian = shadmet$DOY, localtime=shadmet$TIME/60, lat=grid$y[i], long=grid$x[i] , hourly = T, watts=T,dst=-3)))

  soil=as.data.frame(micro$soil)
  shadsoil=as.data.frame(micro$shadsoil)
  metout_final =data.frame(cbind(metout,T_g=soil$D0cm))
  shadmet_final=data.frame(cbind(shadmet, T_g=shadsoil$D0cm))

  te_full_sun = vector()

  for (j in 1:length(metout_final$TALOC)){
    te_full_sun[j]=  Tb_grasshopper(T_a=metout_final$TALOC[j], T_g=metout_final$T_g[j], u=metout_final$VLOC[j],
                                    H=metout_final$SOLR[j], Hdir=metout_final$SOLR[j]*(1-metout_final$difprop[j]),
                                    Hdif=metout_final$SOLR[j]*metout_final$difprop[j],
                                    psi=metout_final$ZEN[j], L=0.02, Acondfact=0, z=.01, abs=0.7, r_g=0.3)
  }



  te_90_shade = vector()

  for (k in 1:length(shadmet_final$TALOC)){
    te_90_shade[k]=  Tb_grasshopper(T_a=shadmet_final$TALOC[k], T_g=shadmet_final$T_g[k], u=shadmet_final$VLOC[k],
                                    H=shadmet_final$SOLR[k], Hdir=shadmet_final$SOLR[k]*(1-shadmet_final$difprop[k]),
                                    Hdif=shadmet_final$SOLR[k]*shadmet_final$difprop[k],
                                    psi=shadmet_final$ZEN[k], L=0.02, Acondfact=0, z=.01, abs=0.7, r_g=0.3)
  }

  hist(te_full_sun)
  metout_with_te=data.frame(cbind(metout_final, te_full_sun))
  shadmet_with_te=data.frame(cbind(shadmet_final, te_90_shade))
  te_by_time=data.frame(time=metout_final$TIME, DOY=metout_final$DOY,te_full_sun, te_90_shade)



  #use this to calculate microclimate mech layers
  prelim_operative=data.frame(cbind(long=grid$x[i],
                                    lat=grid$y[i],
                                    operative_tmax_0shade=max(te_by_time$te_full_sun),
                                    operative_tmin_0shade=min(te_by_time$te_full_sun),
                                    operative_mean_0shade=mean(te_by_time$te_full_sun),
                                    operative_tmax_90shade=max(te_by_time$te_90_shade),
                                    operative_tmin_90shade=min(te_by_time$te_90_shade),
                                    operative_mean_90shade=mean(te_by_time$te_90_shade)))
  operative=rbind(operative, prelim_operative)

  #calculating Tbs with thermoregulation
  #arithmetic mean of active Tb for 5th instar = 38.6
  #arithmetic sd of active Tb for 5th instar = 2.3


  #run this for loop to assume no thermoregulation (random selection of Tb's between 'min' and 'max' temps)
  randomtb=vector()
  for (l in 1:length(te_by_time$te_full_sun)){
    if (te_by_time$te_full_sun[l]>te_by_time$te_90_shade[l]){
      randomtb[l] = runif(1, min=te_by_time$te_90_shade[l], max=te_by_time$te_full_sun[l])
    }
    else
      randomtb[l] = runif(1, min=te_by_time$te_full_sun[l], max=te_by_time$te_90_shade[l])
  }

  #run this for loop to assume locust selects 38.6 if it is available and seeks shade above 40.9
  tb=vector()
  for (l in 1:length(te_by_time$te_full_sun)){
   if ((te_by_time$te_full_sun[l]>=38.6)&(te_by_time$te_90_shade[l]<=38.6)){
    tb[l] = 38.6
  } else if ( (te_by_time$te_full_sun[l]<=40.9)&(te_by_time$te_full_sun[l]>te_by_time$te_90_shade[l])) {
    tb[l] = te_by_time$te_full_sun[l]
  } else if (te_by_time$te_full_sun[l]>40.9){
  tb[l] = te_by_time$te_90_shade[l]
  } else if (te_by_time$te_full_sun[l]<te_by_time$te_90_shade[l]){
    tb[l]=te_by_time$te_90_shade[l]
  } else
    tb[l]=10000
}
  te_by_time=as.data.frame(cbind(te_by_time, tb, randomtb))
  head(te_by_time)
  print("Tb's calculated")


  ##Now we're going to calculate consumption and poop based on Tb

  poop_rate_tb = vector()

  #poop rate is mg/hr for average sized female
  for (m in 1:length(te_by_time$tb)){
    if ( (te_by_time$tb[m]<50.6)&(te_by_time$tb[m]>=-2.0124)){
      poop_rate_tb[m]=(nullcons_female_fit_tpc(te_by_time$tb[m]))
    } else
      poop_rate_tb[m] = 0
  }

  poop_rate_randomtb = vector()

  for (m in 1:length(te_by_time$tb)){
    if ( (te_by_time$randomtb[m]<50.6)&(te_by_time$randomtb[m]>=-2.0124) ){
      poop_rate_randomtb[m]=(nullcons_female_fit_tpc(te_by_time$randomtb[m]))
    } else
      poop_rate_randomtb[m] = 0
  }

  te_by_time=as.data.frame(cbind(te_by_time, poop_rate_tb, poop_rate_randomtb))

  total_poop_tb=data.frame(ddply(te_by_time,.(DOY),summarize,total_poop_tb=(sum(poop_rate_tb))))
  total_poop_randomtb=data.frame(ddply(te_by_time,.(DOY),summarize,total_poop_randomtb=(sum(poop_rate_randomtb))))


  prelim_output=data.frame(cbind(long=rep(grid$x[i], length(total_poop_tb$total_poop_tb)),
                                 lat=rep(grid$y[i], length(total_poop_tb$total_poop_tb)),
                                 total_poop_tb,
                                 total_poop_randomtb=total_poop_randomtb$total_poop_randomtb))
  print("This is run number:")
  print(i)

  model_output=rbind(model_output, prelim_output)

  end_time <- Sys.time()
  print("This run lasted:")
  print(end_time-start_time)
}

########### Cleaning output from mechanistic model ##########

#clean model output
#label each day with its respective month
d=length(model_output$DOY)/12
model_output3=cbind(model_output, month=rep(1:12, d))

#this function calculates total feces per month by multiplying the 'average' day by the # of days in the month
calc_monthly=function(df){
  if (df$month==1||df$month==3||df$month==5||df$month==7||df$month==8||df$month==10||df$month==12){
    df[4:5]=df[,4:5]*31
  } else if (df$month==4||df$month==6||df$month==9||df$month==11){
    df[4:5]=df[,4:5]*30
  } else df[4:5]=df[,4:5]*28.25
  return(df)
}


#calc monthly feces
model_output4=calc_monthly(model_output3)

#split data set into wet and dry seasons because we suspect seasonal variation is important (Williams et al. 2015)
wet_season_poop1=subset(model_output4, model_output4$month>8|model_output4$month<4)
dry_season_poop1=subset(model_output4, model_output4$month<9&model_output4$month>3)

#calculate total feces over season at each pixel
wet_season_poop2_tb=data.frame(ddply(wet_season_poop1,.(long,lat),summarize,wet_season_poop_tb=(sum(total_poop_tb))))
dry_season_poop2_tb=data.frame(ddply(dry_season_poop1,.(long,lat),summarize,dry_season_poop_tb=(sum(total_poop_tb))))
wet_season_poop2_randomtb=data.frame(ddply(wet_season_poop1,.(long,lat),summarize,wet_season_poop_randomtb=(sum(total_poop_randomtb))))
dry_season_poop2_randomtb=data.frame(ddply(dry_season_poop1,.(long,lat),summarize,dry_season_poop_randomtb=(sum(total_poop_randomtb))))

#separate operative layers
operative_tmax_0shade1=cbind(operative$long, operative$lat, operative$operative_tmax_0shade)
operative_tmin_0shade1=cbind(operative$long, operative$lat, operative$operative_tmin_0shade)
operative_tmean_0shade1=cbind(operative$long, operative$lat, operative$operative_mean_0shade)

#convert all layers to raster map with same resolution as macro variables
wet_season_full=rasterFromXYZ(wet_season_poop2_tb,res=res(predictors))
dry_season_full=rasterFromXYZ(dry_season_poop2_tb, res=res(predictors))
wet_season_randomTb=rasterFromXYZ(wet_season_poop2_randomtb,res=res(predictors))
dry_season_randomTb=rasterFromXYZ(dry_season_poop2_randomtb, res=res(predictors))
operative_tmax_0shade=rasterFromXYZ(operative_tmax_0shade1, res=res(predictors))
operative_tmin_0shade=rasterFromXYZ(operative_tmin_0shade1, res=res(predictors))
operative_tmean_0shade=rasterFromXYZ(operative_tmean_0shade1, res=res(predictors))

#splitting up data by month
jan_full1=subset(model_output4, month==1)
feb_full1=subset(model_output4, month==2)
mar_full1=subset(model_output4, month==3)
apr_full1=subset(model_output4, month==4)
may_full1=subset(model_output4, month==5)
jun_full1=subset(model_output4, month==6)
jul_full1=subset(model_output4, month==7)
aug_full1=subset(model_output4, month==8)
sep_full1=subset(model_output4, month==9)
oct_full1=subset(model_output4, month==10)
nov_full1=subset(model_output4, month==11)
dec_full1=subset(model_output4, month==12)

#creating monthly datasheets with predictions from full model
jan_full2=cbind(jan_full1[1:2], jan_full1[4])
feb_full2=cbind(feb_full1[1:2], feb_full1[4])
mar_full2=cbind(mar_full1[1:2], mar_full1[4])
apr_full2=cbind(apr_full1[1:2], apr_full1[4])
may_full2=cbind(may_full1[1:2], may_full1[4])
jun_full2=cbind(jun_full1[1:2], jun_full1[4])
jul_full2=cbind(jul_full1[1:2], jul_full1[4])
aug_full2=cbind(aug_full1[1:2], aug_full1[4])
sep_full2=cbind(sep_full1[1:2], sep_full1[4])
oct_full2=cbind(oct_full1[1:2], oct_full1[4])
nov_full2=cbind(nov_full1[1:2], nov_full1[4])
dec_full2=cbind(dec_full1[1:2], dec_full1[4])

#creating monthly datasheets with predictions from random shade model
jan_random2=cbind(jan_full1[1:2], jan_full1[5])
feb_random2=cbind(feb_full1[1:2], feb_full1[5])
mar_random2=cbind(mar_full1[1:2], mar_full1[5])
apr_random2=cbind(apr_full1[1:2], apr_full1[5])
may_random2=cbind(may_full1[1:2], may_full1[5])
jun_random2=cbind(jun_full1[1:2], jun_full1[5])
jul_random2=cbind(jul_full1[1:2], jul_full1[5])
aug_random2=cbind(aug_full1[1:2], aug_full1[5])
sep_random2=cbind(sep_full1[1:2], sep_full1[5])
oct_random2=cbind(oct_full1[1:2], oct_full1[5])
nov_random2=cbind(nov_full1[1:2], nov_full1[5])
dec_random2=cbind(dec_full1[1:2], dec_full1[5])

#creating montly rasters from full model
jan_full=rasterFromXYZ(jan_full2,res=res(predictors))
feb_full=rasterFromXYZ(feb_full2,res=res(predictors))
mar_full=rasterFromXYZ(mar_full2,res=res(predictors))
apr_full=rasterFromXYZ(apr_full2,res=res(predictors))
may_full=rasterFromXYZ(may_full2,res=res(predictors))
jun_full=rasterFromXYZ(jun_full2,res=res(predictors))
jul_full=rasterFromXYZ(jul_full2,res=res(predictors))
aug_full=rasterFromXYZ(aug_full2,res=res(predictors))
sep_full=rasterFromXYZ(sep_full2,res=res(predictors))
oct_full=rasterFromXYZ(oct_full2,res=res(predictors))
nov_full=rasterFromXYZ(nov_full2,res=res(predictors))
dec_full=rasterFromXYZ(dec_full2,res=res(predictors))

#creating monthly rasters from random shade model
jan_random=rasterFromXYZ(jan_random2,res=res(predictors))
feb_random=rasterFromXYZ(feb_random2,res=res(predictors))
mar_random=rasterFromXYZ(mar_random2,res=res(predictors))
apr_random=rasterFromXYZ(apr_random2,res=res(predictors))
may_random=rasterFromXYZ(may_random2,res=res(predictors))
jun_random=rasterFromXYZ(jun_random2,res=res(predictors))
jul_random=rasterFromXYZ(jul_random2,res=res(predictors))
aug_random=rasterFromXYZ(aug_random2,res=res(predictors))
sep_random=rasterFromXYZ(sep_random2,res=res(predictors))
oct_random=rasterFromXYZ(oct_random2,res=res(predictors))
nov_random=rasterFromXYZ(nov_random2,res=res(predictors))
dec_random=rasterFromXYZ(dec_random2,res=res(predictors))

#combinging rasters into stacks
full_monthly = stack(jan_full,
                     feb_full,
                     mar_full,
                     apr_full,
                     may_full,
                     jun_full,
                     jul_full,
                     aug_full,
                     sep_full,
                     oct_full,
                     nov_full,
                     dec_full)

random_monthly = stack(jan_random,
                       feb_random,
                       mar_random,
                       apr_random,
                       may_random,
                       jun_random,
                       jul_random,
                       aug_random,
                       sep_random,
                       oct_random,
                       nov_random,
                       dec_random)

plot(wet_season_full)
plot(dry_season_full)
plot(wet_season_randomTb)
plot(dry_season_randomTb)
