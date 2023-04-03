#setwd("/Volumes/Bär/Projekte/Eisvogel/DatenUndSkripte_EisvogelCara")
#rm(list=ls())
#load("Eisvogel20220111.RData")
#install.packages("performance")
library("performance")
library("lme4")
library("visreg")
#############################
## let the statistic begin ##
#############################
# remove NAs
# 1) Time with no observation
DF_all<-DF_all[!is.na(DF_all$n.people),] #remove all na
# DF_all<-DF_all[!is.na(DF_all$time.since.last.entry),]
# nrow(DF_all)

# Remove Bach on 2021-06-05 (bird not feeding anymore)
DF_all <-DF_all[!(DF_all$location=="Bach" & DF_all$StudyPeriod_Dates=="2021-06-05"),] #hatchlings left
# Remove Aue on 2021-05-28 (bird not feeding anymore)
DF_all <-DF_all[!(DF_all$location=="Aue" & DF_all$StudyPeriod_Dates=="2021-05-28"),] #hatchlings left

DF_all$minutes<-scale(DF_all$minutes) #"scale" sets standard error =1 and mean =0
DF_all$until_fledging <-scale(DF_all$until_fledging)
# DF_all$time.since.last.entry <-scale(DF_all$time.since.last.entry)
head(DF_all, 30)

#mod1<-glm(entry  ~time.since.last.entry+ location,data=DF_all ,family=binomial)
#visreg(mod1,xvar="time.since.last.entry",scale="response")

##############################################
## test effect with observations per minute ##
##############################################
# Number of people # Model 1
mod1<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people + (1|location),data=DF_all ,family=binomial) #run Model, then take out variable that is the least significant until only significant variables are left
mod1<-glmer(entry  ~  temperature + until_fledging + I(until_fledging^2) +  n.people + (1|location),data=DF_all ,family=binomial)
summary(mod1) 
r2(mod1) #Marginal Effects R in table
DF_new<-DF_all
DF_new$n.people <-0
a<-mean(predict(mod1,type="response"))
b<-mean(predict(mod1,newdata= DF_new,type="response"),na.rm=T)
#round((1-b/a)*100,2) # % change -> effect of humans on wether or not bird shows up
round((1-a/b)*100,2)
a #propability a bird appears (mean of model)
b #propability a bird appears when leaving out people
###
mod2<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people + (1|location),data=DF_all ,family=binomial)# Vitis seem to be show reality better than entries
mod2<-glmer(visit  ~  temperature  + I(until_fledging^2) + minutes + n.people + (1|location),data=DF_all ,family=binomial)
summary(mod2)
r2(mod2)
DF_new<-DF_all
DF_new$n.people <-0
a<-mean(predict(mod2,type="response"))
b<-mean(predict(mod2,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
a #propability a bird appears (mean of model)
b #propability a bird appears when leaving out people
###

###########
# Distance # Model 2
mod3<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance + (1|location),data=DF_all ,family=binomial)
mod3<-glmer(entry  ~ distance + (1|location),data=DF_all ,family=binomial)
summary(mod3)
r2(mod3)#result but error warning
DF_new<-DF_all
DF_new$distance <-25
a<-mean(predict(mod3,type="response"))
b<-mean(predict(mod3,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
a
b
###
mod4<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance + (1|location),data=DF_all ,family=binomial)
#mod4<-glmer(visit  ~ rain+ until_fledging + distance + (1|location),data=DF_all ,family=binomial)
mod4<-glmer(visit  ~  until_fledging + I(until_fledging^2) + (1|location),data=DF_all ,family=binomial)
summary(mod4)
r2(mod4)
DF_new<-DF_all
DF_new$distance <-0 #here different distances can be typed in
a<-mean(predict(mod4,type="response"))
b<-mean(predict(mod4,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
a
b

###########
# Single effects #
mod5<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian +biker +boats +fisher +swimmer +dog +music+ (1|location),data=DF_all ,family=binomial)
mod5<-glmer(entry  ~  temperature + until_fledging + I(until_fledging^2) +biker +music+ (1|location),data=DF_all ,family=binomial)
summary(mod5)
r2(mod5)
DF_new<-DF_all
DF_new$biker <-0
DF_new$music <-0
a<-mean(predict(mod5,type="response"))
b<-mean(predict(mod5,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
a
b
###
mod6<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian +biker +boats +fisher +swimmer +dog +music+ (1|location),data=DF_all ,family=binomial)
#mod6<-glmer(visit  ~  temperature +  I(until_fledging^2) + minutes  + boats +fisher +music+ (1|location),data=DF_all ,family=binomial)
mod6<-glmer(visit  ~  temperature + I(until_fledging^2) + minutes +boats +fisher +swimmer +music+ (1|location),data=DF_all ,family=binomial)
summary(mod6)
r2(mod6)
DF_new<-DF_all
DF_new$boats <-0
DF_new$fisher <-0
DF_new$swimmer <-0
DF_new$music <-0
a<-mean(predict(mod6,type="response"))
b<-mean(predict(mod6,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
a
b
###

###################################
## glm for the specific Nesting Sites ##
###################################
view(DF_all)

###Nesting Site 1 - Park
# number of people #
Par1<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Park"), family=binomial)
Par1<-glm(entry~ minutes ,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par1)
r2(Par1)
nobs(Par1)

Par2<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Park"), family=binomial)
Par2<-glm(visit~ until_fledging + I(until_fledging^2) + minutes ,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par2)
r2(Par2)
nobs(Par2)
###
# distance #
Par3<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Park"), family=binomial)
Par3<-glm(entry~ minutes ,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par3)
r2(Par3)
nobs(Par3)

Par4<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Park"), family=binomial)
Par4<-glm(visit~ until_fledging + I(until_fledging^2) + minutes  ,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par4)
r2(Par4)
nobs(Par4)
###
# single activities #
Par5<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Park"), family=binomial)
Par5<-glm(entry~ minutes ,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par5)
r2(Par5)
nobs(Par5)

Par6<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Park"), family=binomial)
Par6<-glm(visit~ until_fledging + I(until_fledging^2) + minutes,data=subset(DF_all, location =="Park"), family=binomial)
summary(Par6)
r2(Par6)
nobs(Par6)


###Nesting Site 2 - River (Pettstadt)
# number of people #
Pet1<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet1<-glm(entry~ temperature +until_fledging +n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet1)
r2(Pet1)
nobs(Pet1)

Pet2<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet2<-glm(visit~ temperature + until_fledging + I(until_fledging^2) + minutes + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet2)
r2(Pet2)
nobs(Pet2)
###
# distance #
Pet3<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet3<-glm(entry~ 1 ,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet3)#nothing is significant
r2(Pet3)
nobs(Pet3)

Pet4<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet4<-glm(visit~  distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet4)
r2(Pet4)
nobs(Pet4)
###
# single activities #
Pet5<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet5<-glm(entry~ temperature +until_fledging +  pedestrian  ,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet5)
r2(Pet5)
nobs(Pet5)

Pet6<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet6<-glm(visit~ temperature + until_fledging + I(until_fledging^2) + I(minutes ^2) + pedestrian+ swimmer,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet6)
r2(Pet6)
nobs(Pet6)
####


###Nesting Site 3 - Meadow (Aue)
# number of people #
Aue1<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Aue"), family=binomial)
Aue1<-glm(entry~ rain + I(minutes ^2) ,data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue1)
r2(Aue1)
nobs(Aue1)

Aue2<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Aue"), family=binomial)
Aue2<-glm(visit~ I(until_fledging^2) + minutes + I(minutes ^2) ,data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue2)
r2(Aue2)
nobs(Aue2)
###
# distance #
Aue3<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Aue"), family=binomial)
Aue3<-glm(entry~ distance,data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue3) #nothing is significant
r2(Aue3)
nobs(Aue3)

Aue4<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Aue"), family=binomial)
Aue4<-glm(visit~ minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue4)
r2(Aue4)
nobs(Aue4)
###
# single activities #
Aue5<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Aue"), family=binomial)
Aue5<-glm(entry~ rain+  I(until_fledging^2) + minutes + I(minutes ^2) +dog ,data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue5)
r2(Aue5)
nobs(Aue5)

Aue6<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Aue"), family=binomial)
Aue6<-glm(visit~ I(until_fledging^2) + minutes + I(minutes ^2),data=subset(DF_all, location =="Aue"), family=binomial)
summary(Aue6)
r2(Aue6)
nobs(Aue6)
###


####Nesting Site 4 - Stream (Bach)
# number of people #
Par1<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Bach"), family=binomial)
Par1<-glm(entry~ until_fledging + I(until_fledging^2) + minutes,data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par1)
r2(Par1)
nobs(Par1)

Par2<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Bach"), family=binomial)
Par2<-glm(visit~ temperature + until_fledging ,data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par2) #Intercept is not significant
r2(Par2)
nobs(Par2)
###
# distance #
Par3<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Bach"), family=binomial)
Par3<-glm(entry~ minutes ,data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par3)
r2(Par3)
nobs(Par3)

Par4<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Bach"), family=binomial)
Par4<-glm(visit~ I(minutes ^2),data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par4)
r2(Par4)
nobs(Par4)
###
# single activities #
Par5<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Bach"), family=binomial)
Par5<-glm(entry~ until_fledging + I(until_fledging^2) + minutes,data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par5)
r2(Par5)
nobs(Par5)

Par6<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Bach"), family=binomial)
Par6<-glm(visit~ temperature + until_fledging,data=subset(DF_all, location =="Bach"), family=binomial)
summary(Par6)#Intercept not significant
r2(Par6)
nobs(Par6)
###


percentage.entries<-function(x){
d<-na.omit((DF_all$entry)[DF_all$location==x])	
round(sum(d)/length(d)*100,2)
}
percentage.entries("Park")
percentage.entries("Pettstadt")
percentage.entries("Aue")
percentage.entries("Bach")


###################################
## glmer propability on hourly basis ##
###################################
#######
nrow(DF_hour) # 245
DF_hour<-subset(DF_hour,!(min<50)) #All hours that are not at least 50 min long are left out here
nrow(DF_hour) # 177
####### Number of People, entries
m1<-glmer(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
m1<-glmer(entries ~ until_fledging + I(until_fledging ^2) + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
summary(m1)
r2(m1)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$n.people<-500 #here number of people can be set
real<-mean(predict(m1,newdata= DF_hour,type="response")) #real = a
null<-mean(predict(m1,newdata= DF_hour_new,type="response")) #null = b
real
null
(1-null/real)*100
visreg(m1,xvar="n.people",scale="response")

########### Number of People, visits
m2<-glmer(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
m2<-glmer(visits ~ until_fledging + I(until_fledging ^2) + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
summary(m2)
r2(m2)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$n.people<-500
real<-mean(predict(m2,newdata= DF_hour,type="response"))
null<-mean(predict(m2,newdata= DF_hour_new,type="response"))
real
null
#(1-real/null)*100
(1-null/real)*100
visreg(m2,xvar="n.people",scale="response")

###For hourly basis there is no model looking at distance.

# Single activities #
m3<-glmer(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer  +(1|location) , data=DF_hour,family="poisson")
m3<-glmer(entries ~ until_fledging + I(until_fledging ^2) + temperature  +boats+fisher+(1|location) , data=DF_hour,family="poisson")
summary(m3)
r2(m3)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$boats <-0
DF_hour_new$fisher <-0
real<-mean(predict(m3,newdata= DF_hour,type="response"))
null<-mean(predict(m3,newdata= DF_hour_new,type="response"))
real
null
#(1-real/null)*100
(1-null/real)*100
visreg(m3,xvar="fisher",scale="response")

# 
m4<-glmer(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer +(1|location) , data=DF_hour,family="poisson")
m4<-glmer(visits ~ until_fledging + I(until_fledging ^2) +  temperature +boats  +fisher +(1|location) , data=DF_hour,family="poisson")
m4<-glmer(visits ~ until_fledging + I(until_fledging ^2) + temperature +boats +fisher +(1|location) , data=DF_hour,family="poisson")
summary(m4)
r2(m4)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$boats <-100
DF_hour_new$fisher <-100
real<-mean(predict(m4,newdata= DF_hour,type="response"))
null<-mean(predict(m4,newdata= DF_hour_new,type="response"))
real
null
#(1-real/null)*100
(1-null/real)*100
visreg(m4,xvar="boats",scale="response")

###Hour-based data at specific locations
###Nesting Site 1 - Park###
# number of people #
Par1<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Park"), family="poisson")
Par1<-glm(entries ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Park"), family="poisson")
summary(Par1)
r2(Par1)
nobs(Par1)

Par2<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Park"), family="poisson")
Par2<-glm(visits ~ until_fledging + I(until_fledging ^2) + temperature + n.people,data=subset(DF_hour, location =="Park"), family="poisson")
summary(Par2)
r2(Par2)
nobs(Par2)

# single activities #
Par3<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Park"), family="poisson")
Par3<-glm(entries ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Park"), family="poisson")
summary(Par3)
r2(Par3)
nobs(Par3)

Par4<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Park"), family="poisson")
Par4<-glm(visits ~ until_fledging + I(until_fledging ^2) +pedestrian +fisher +dog,data=subset(DF_hour, location =="Park"), family="poisson")
summary(Par4)
r2(Par4)
nobs(Par4)

###Nesting Site 2 - River (Pettstadt)###
# number of people #
Pett1<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett1<-glm(entries ~ until_fledging + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett1)
r2(Pett1)
nobs(Pett1)

Pett2<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett2<-glm(visits ~ until_fledging + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett2)
r2(Pett2)
nobs(Pett2)

# single activities #
Pett3<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett3<-glm(entries ~ until_fledging + temperature +pedestrian,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett3)
r2(Pett3)
nobs(Pett3)

Pett4<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett4<-glm(visits ~ until_fledging + I(until_fledging ^2) + temperature +boats +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett4)
r2(Pett4)
nobs(Pett4)

###Nesting Site 3 - Meadow (Aue)###
# number of people #
Aue1<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Aue"), family="poisson")
Aue1<-glm(entries ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Aue"), family="poisson")
summary(Aue1)
r2(Aue1)
nobs(Aue1)

Aue2<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Aue"), family="poisson")
Aue2<-glm(visits ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Aue"), family="poisson")
summary(Aue2)
r2(Aue2)
nobs(Aue2)

# single activities #
Aue3<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Aue"), family="poisson")
Aue3<-glm(entries ~ until_fledging + I(until_fledging ^2) + biker +pedestrian +dog,data=subset(DF_hour, location =="Aue"), family="poisson")
summary(Aue3)
r2(Aue3)
nobs(Aue3)

Aue4<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Aue"), family="poisson")
Aue4<-glm(visits ~ until_fledging + I(until_fledging ^2) + biker +pedestrian +dog,data=subset(DF_hour, location =="Aue"), family="poisson")
summary(Aue4)
r2(Aue4)
nobs(Aue4)

####Nesting Site 4 - Stream (Bach)###
# number of people #
Bach1<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Bach"), family="poisson")
Bach1<-glm(entries ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Bach"), family="poisson")
summary(Bach1)
r2(Bach1)
nobs(Bach1)

Bach2<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Bach"), family="poisson")
Bach2<-glm(visits ~ I(until_fledging ^2) + temperature,data=subset(DF_hour, location =="Bach"), family="poisson")
summary(Bach2)
r2(Bach2)
nobs(Bach2)

# single activities #
Bach3<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Bach"), family="poisson")
Bach3<-glm(entries ~ until_fledging + I(until_fledging ^2),data=subset(DF_hour, location =="Bach"), family="poisson")
summary(Bach3)
r2(Bach3)
nobs(Bach3)

Bach4<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Bach"), family="poisson")
Bach4<-glm(visits ~ I(until_fledging ^2) + temperature,data=subset(DF_hour, location =="Bach"), family="poisson")
summary(Bach4)
r2(Bach4)
nobs(Bach4)
