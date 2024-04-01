
#set working directory to folder with datasheet



#load needed packages
library(lme4)
library(nlme)
library(MuMIn)

#set wd to source file location
setwd("C:/Users/smith/Desktop/Ghop_git/TPC/Youngblood_ex")

#read in data
data=read.csv('datasheet for analyses of consumption and defecation_EcoMono.csv', header=T)

#check data
str(data)
attach(data)

#change to factors for analysis
treatment=as.factor(treatment)

#checking if random effect improves the full model
feces_full_model_random=lme(feces_dry_mass~treatment*sex*total_consumption*hopper_mass_before, random=~1|block, na.action='na.fail')
feces_full_model=lm(feces_dry_mass~treatment*sex*total_consumption*hopper_mass_before, na.action='na.fail')
AIC(feces_full_model_random,feces_full_model) #model with no random effect is better by far

#view summary of full model
summary(feces_full_model)

#fit all possible subsets of the full model and view output
d=dredge(feces_full_model)
d

#perform multimodel averaging and view output
model_set=get.models(d, subset= T)
model_set
avg=model.avg(model_set)
summary(avg)

#creating dummy dataset to predict relationship between defecation and body mass for each sex at every temperature (assuming mean consumption at all temps)
newdata=data.frame(cbind(hopper_mass_before=rep(hopper_mass_before,8)),
                   sex=c(rep('f', length(hopper_mass_before)*4),rep('m',length(hopper_mass_before)*4)),
                   total_consumption=rep(mean(data$total_consumption),length(hopper_mass_before)*8),
                   treatment=c(rep('27', length(hopper_mass_before)),
                               rep('33', length(hopper_mass_before)),
                               rep('40', length(hopper_mass_before)),
                               rep('46', length(hopper_mass_before)),
                               rep('27', length(hopper_mass_before)),
                               rep('33', length(hopper_mass_before)),
                               rep('40', length(hopper_mass_before)),
                               rep('46', length(hopper_mass_before))))

#detach original dataset and attach 1st dummy dataset
detach(data)
attach(newdata)
str(newdata)

#predict consumption based on model-averaged parameters and dummy dataset
p=predict(avg, type='response', newdata=newdata, fit=T, se.fit = T)

#combine predictions into dataframe
prediction=data.frame(cbind(newdata, p))

#plot to check realism
plot(prediction$fit[prediction$sex=='f']~prediction$hopper_mass_before[prediction$sex=='f'],ylim=c(0,.1))
points(data$hopper_mass_before, data$feces_dry_mass, pch=16)

#write csv file with model-averaged predictions
write.csv(prediction, file='mumin_defecation by mass for sexes and temps_12-1-20.csv')


#now we are going to do the same thing, except predict relationship between consumption and defecation for every sex and temperature (assuming mean Mb for each sex)
#defining variables with the mean mass of each sex
favg_mass=mean(data$hopper_mass_before[data$sex=='f'])
mavg_mass=mean(data$hopper_mass_before[data$sex=='m'])

#creating 2nd dummy dataset to predict relationship between consumption and defecation for every sex and temperature (assuming mean Mb for each sex)
newdata2=data.frame(cbind(hopper_mass_before=c(rep(favg_mass,length(data$total_consumption)*4),rep(mavg_mass,length(data$total_consumption)*4)),
                   sex=c(rep('f', length(data$total_consumption)*4),rep('m',length(data$total_consumption)*4)),
                   total_consumption=rep(data$total_consumption,8),
                   treatment=c(rep('27', length(data$total_consumption)),
                               rep('33', length(data$total_consumption)),
                               rep('40', length(data$total_consumption)),
                               rep('46', length(data$total_consumption)),
                               rep('27', length(data$total_consumption)),
                               rep('33', length(data$total_consumption)),
                               rep('40', length(data$total_consumption)),
                               rep('46', length(data$total_consumption)))))

#detaching 1st dummy dataset and attaching 2nd dummy dataset
detach(newdata)
attach(newdata2)
str(newdata2)

#correcting structure of 2nd dummy dataset
newdata2$hopper_mass_before=as.numeric(as.character(newdata2$hopper_mass_before))
newdata2$total_consumption=as.numeric(as.character(newdata2$total_consumption))
str(newdata2)

#predict consumption based on model-averaged parameters and dummy dataset
p2=predict(avg, type='response', newdata=newdata2, fit=T, se.fit = T)

#combine predictions into dataframe
prediction2=data.frame(cbind(newdata2, p2))

#plot to check realism
plot(prediction2$fit[prediction2$sex=='m']~prediction2$total_consumption[prediction2$sex=='f'])

#write csv file with model-averaged predictions
write.csv(prediction2, file='mumin_defecation by consumption for sexes and temps_12-1-20.csv')

#creating 3rd dummy dataset to predict mean defecation at each temperature for each sex
newdata3=data.frame(cbind(hopper_mass_before=c(rep(favg_mass,4),rep(mavg_mass,4)),
                          sex=c(rep('f', 4),rep('m',4)),
                          total_consumption=rep(mean(data$total_consumption),8),
                          treatment=c('27',
                                      '33',
                                      '40',
                                      '46',
                                      '27',
                                      '33',
                                      '40',
                                      '46')))

#detaching 2nd dummy dataset and attaching 3rd dummy dataset
detach(newdata2)
attach(newdata3)
str(newdata3)

#correcting structure of 2nd dummy dataset
newdata3$hopper_mass_before=as.numeric(as.character(newdata3$hopper_mass_before))
newdata3$total_consumption=as.numeric(as.character(newdata3$total_consumption))
str(newdata3)

#predict consumption based on model-averaged parameters and dummy dataset
p3=predict(avg, type='response', newdata=newdata3, fit=T, se.fit = T)

#combine predictions into dataframe
prediction3=data.frame(cbind(newdata3, p3))

#plot to check realism
plot(prediction3$fit[prediction3$sex=='m']~prediction3$treatment[prediction3$sex=='f'])

#write csv file with model-averaged predictions
write.csv(prediction3, file='mumin_mean defecation for sexes and temps (assuming mean consumption)_12-1-20.csv')

#creating 4th dummy dataset to predict mean defecation at each temperature for each sex (now assuming mean + sd consumption)
newdata4=data.frame(cbind(hopper_mass_before=c(rep(favg_mass,4),rep(mavg_mass,4)),
                          sex=c(rep('f', 4),rep('m',4)),
                          total_consumption=rep(mean(data$total_consumption)+sd(data$total_consumption),8),
                          treatment=c('27',
                                      '33',
                                      '40',
                                      '46',
                                      '27',
                                      '33',
                                      '40',
                                      '46')))

#detaching 2nd dummy dataset and attaching 3rd dummy dataset
detach(newdata3)
attach(newdata4)
str(newdata4)

#correcting structure of 2nd dummy dataset
newdata4$hopper_mass_before=as.numeric(as.character(newdata4$hopper_mass_before))
newdata4$total_consumption=as.numeric(as.character(newdata4$total_consumption))
str(newdata4)

#predict consumption based on model-averaged parameters and dummy dataset
p4=predict(avg, type='response', newdata=newdata4, fit=T, se.fit = T)

#combine predictions into dataframe
prediction4=data.frame(cbind(newdata4, p4))

#plot to check realism
plot(prediction4$fit[prediction3$sex=='m']~prediction4$treatment[prediction4$sex=='m'])

#write csv file with model-averaged predictions
write.csv(prediction4, file='mumin_mean defecation for sexes and temps (assuming mean + sd consumption)_12-1-20.csv')


#creating 5th dummy dataset to predict mean defecation at each temperature for each sex (now assuming mean + sd consumption)
newdata5=data.frame(cbind(hopper_mass_before=c(rep(favg_mass,4),rep(mavg_mass,4)),
                          sex=c(rep('f', 4),rep('m',4)),
                          total_consumption=rep(mean(data$total_consumption)-sd(data$total_consumption),8),
                          treatment=c('27',
                                      '33',
                                      '40',
                                      '46',
                                      '27',
                                      '33',
                                      '40',
                                      '46')))

#detaching 2nd dummy dataset and attaching 3rd dummy dataset
detach(newdata4)
attach(newdata5)
str(newdata5)

#correcting structure of 2nd dummy dataset
newdata5$hopper_mass_before=as.numeric(as.character(newdata5$hopper_mass_before))
newdata5$total_consumption=as.numeric(as.character(newdata5$total_consumption))
str(newdata5)

#predict consumption based on model-averaged parameters and dummy dataset
p5=predict(avg, type='response', newdata=newdata5, fit=T, se.fit = T)

#combine predictions into dataframe
prediction5=data.frame(cbind(newdata5, p5))

#plot to check realism
plot(prediction5$fit[prediction5$sex=='f']~prediction5$treatment[prediction5$sex=='f'])

#write csv file with model-averaged predictions
write.csv(prediction5, file='mumin_mean defecation for sexes and temps (assuming mean - sd consumption)_12-1-20.csv')


