
setwd("/Volumes/Bär/Projekte/Eisvogel/DatenUndSkripte_EisvogelCara")
rm(list=ls())
load("Eisvogel20220111.RData")
head(DF_all)
length(DF_all$StudyPeriod_Dates)/60
head(DF_hour)
unique(rownames(DF_hour))
#install.packages("performance")
library("performance")
library("lme4")
nrow(DF_all)
table(DF_all$location)
head(DF_all)
table(DF_all$location[DF_all$n.people>0])/table(DF_all$location)
sum(DF_all$music>0)
biker boats fisher swimmer dog music

tapply(DF_all$n.people,DF_all$location,mean)*60
table(DF_all$location)
DF_all$n.people>0

all<-c("pedestrian", "dog", "biker","music","swimmer", "boats", "fisher")
namen<-c("Pedestrian", "Dog", "Biker","Music","Swimmer", "Boats", "Fisher")
lims<-c(0,1400)
cols<-c("orange","orange","orange","grey","steelblue4","steelblue4","steelblue4")
quartz(,6,6)
par(mfrow=c(2,2))
barplot(colSums(DF_all[DF_all$location=="Park", all]>0),horiz=T,las=1,xlim=lims,col=cols,names=namen,main="Park")
barplot(colSums(DF_all[DF_all$location=="Bach", all]>0),horiz=T,las=1,xlim=lims,col=cols,names=namen,main="Stream")
barplot(colSums(DF_all[DF_all$location=="Aue", all]>0),horiz=T,las=1,xlim=lims,col=cols,names=namen,main="Meadow ")
barplot(colSums(DF_all[DF_all$location=="Pettstadt", all]>0),horiz=T,las=1,xlim=lims,col=cols,names=namen,main="River")
par(mfrow=c(1,1))
rm(all)



#############################
## let the statistic begin ##
#############################
# remove NAs
# 1) Time with no observation
DF_all<-DF_all[!is.na(DF_all$n.people),]
# DF_all<-DF_all[!is.na(DF_all$time.since.last.entry),]
# nrow(DF_all)

# Remove Bach on 2021-06-05 (bird not feeding anymore)
DF_all <-DF_all[!(DF_all$location=="Bach" & DF_all$StudyPeriod_Dates=="2021-06-05"),]
# Remove Aue on 2021-05-28 (bird not feeding anymore)
DF_all <-DF_all[!(DF_all$location=="Aue" & DF_all$StudyPeriod_Dates=="2021-05-28"),]

DF_all$minutes<-scale(DF_all$minutes)
# DF_all$until_fledging <-scale(DF_all$until_fledging)
# DF_all$time.since.last.entry <-scale(DF_all$time.since.last.entry)

#mod1<-glm(entry  ~time.since.last.entry+ location,data=DF_all ,family=binomial)
#visreg(mod1,xvar="time.since.last.entry",scale="response")

##############################################
## test effect with observations per minute ##
##############################################
# Number of people #
mod1<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people + (1|location),data=DF_all ,family=binomial)
mod1<-glmer(entry  ~  temperature + until_fledging + I(until_fledging^2) +  n.people + (1|location),data=DF_all ,family=binomial)
summary(mod1)
r2(mod1)
DF_new<-DF_all
DF_new$n.people <-0
a<-mean(predict(mod1,type="response"))
b<-mean(predict(mod1,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
###
library("sjPlot")
quartz(,7,5)
plot_model(mod1, type = "pred", terms = "n.people [all]",axis_title.x=20,
           axis.title = c("Number of people [per minute]","Chance of entering the nest"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

library("sjPlot")
quartz(,7,5)
plot_model(mod1, type = "pred", terms = "temperature [all]",axis_title.x=20,
           axis.title = c("Temperature [°C]","Chance of entering the nest"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

quartz(,7,5)
plot_model(mod1, type = "pred", terms = "until_fledging [all]",axis_title.x=20,
           axis.title = c("Time until fledging [days]","Chance of entering the nest"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)




mod2<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people + (1|location),data=DF_all ,family=binomial)
mod2<-glmer(visit  ~  temperature + I(until_fledging^2) + minutes + n.people + (1|location),data=DF_all ,family=binomial)
summary(mod2)
r2(mod1)
DF_new<-DF_all
DF_new$n.people <-0
a<-mean(predict(mod2,type="response"))
b<-mean(predict(mod2,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
###

###########
# Distance #
mod3<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance + (1|location),data=DF_all ,family=binomial)
mod3<-glmer(entry  ~ distance + (1|location),data=DF_all ,family=binomial)
summary(mod3)
r2(mod3)
DF_new<-DF_all
DF_new$distance <-25
a<-mean(predict(mod3,type="response"))
b<-mean(predict(mod3,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change

quartz(,7,5)
plot_model(mod3, type = "pred", terms = "distance [all]",axis_title.x=20,
           axis.title = c("Distance of activity [m]","Chance of entering the nest"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)


###
mod4<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance + (1|location),data=DF_all ,family=binomial)
mod4<-glmer(visit  ~ rain+ until_fledging + distance + (1|location),data=DF_all ,family=binomial)
summary(mod4)
r2(mod4)
DF_new<-DF_all
DF_new$distance <-100
a<-mean(predict(mod4,type="response"))
b<-mean(predict(mod4,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change

###########
# Single effects #
mod5<-glmer(entry  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian +biker +boats +fisher +swimmer +dog +music+ (1|location),data=DF_all ,family=binomial)
mod5<-glmer(entry  ~  temperature + until_fledging + I(until_fledging^2) + biker    +music+ (1|location),data=DF_all ,family=binomial)
summary(mod5)
quartz(,7,5)
plot_model(mod5, type = "pred", terms = "music [all]",axis_title.x=20,
           axis.title = c("music [per minute]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

r2(mod5)
DF_new<-DF_all
DF_new$biker <-0
a<-mean(predict(mod5,type="response"))
b<-mean(predict(mod5,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
###
mod6<-glmer(visit  ~  temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian +biker +boats +fisher +swimmer +dog +music+ (1|location),data=DF_all ,family=binomial)
mod6<-glmer(visit  ~  temperature +  I(until_fledging^2) + minutes  + boats +fisher +swimmer  +music+ (1|location),data=DF_all ,family=binomial)
summary(mod6)
r2(mod6)
DF_new<-DF_all
DF_new$boats <-0
a<-mean(predict(mod6,type="response"))
b<-mean(predict(mod6,newdata= DF_new,type="response"),na.rm=T)
round((1-b/a)*100,2) # % change
###

###################################
## glm der einzelnen Standtorte ##
###################################
###Pettstadt
# number of people #
Pet1<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet1<-glm(entry~ temperature +until_fledging +n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet1)
r2(Pet1)
Pet2<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet2<-glm(visit~ temperature + until_fledging + I(until_fledging^2) + I(minutes ^2) + n.people,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet2)
r2(Pet2)
#####################
# distance #
Pet3<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet3<-glm(entry~ 1 ,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet3)
r2(Pet3)
Pet4<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet4<-glm(visit~  distance,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet4)
r2(Pet3)
head(DF_all)
#####################
# single activities #
Pet5<-glm(entry~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet5<-glm(entry~ temperature +until_fledging +  pedestrian  ,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet5)
r2(Pet5)
#####################
Pet6<-glm(visit~ temperature +rain+ until_fledging + I(until_fledging^2) + minutes + I(minutes ^2) + pedestrian+ biker +boats +fisher +swimmer +dog +music,data=subset(DF_all, location =="Pettstadt"), family=binomial)
Pet6<-glm(visit~ temperature + until_fledging + I(until_fledging^2) + I(minutes ^2) + pedestrian+ swimmer,data=subset(DF_all, location =="Pettstadt"), family=binomial)
summary(Pet6)
r2(Pet6)
#######################





###Park



###Aue


####Bach



percentage.entries<-function(x){
d<-na.omit((DF_all$entry)[DF_all$location==x])	
round(sum(d)/length(d)*100,2)
}
percentage.entries("Pettstadt")
percentage.entries("Aue")
percentage.entries("Bach")
percentage.entries("Park")


###################################
## glmer Häufigkeit in der Stunde ##
###################################
#######
nrow(DF_hour) # 245
DF_hour<-subset(DF_hour,!(min<50))
nrow(DF_hour) # 177
head(DF_hour)

tapply(DF_hour$entries, DF_hour$location,mean)
tapply(DF_hour$entries, DF_hour$location,sd)
tapply(DF_hour$entries, DF_hour$location,median)

tapply(DF_hour$visits, DF_hour$location,mean)
tapply(DF_hour$visits, DF_hour$location,sd)
tapply(DF_hour$visits, DF_hour$location,median)


#######
m1<-glmer(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
m1<-glmer(entries ~ until_fledging + I(until_fledging ^2) + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
summary(m1)
r2(m1)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$n.people<-500
real<-mean(predict(m1,newdata= DF_hour,type="response"))
null<-mean(predict(m1,newdata= DF_hour_new,type="response"))
(1-real/null)*100
quartz(,7,5)
plot_model(m1, type = "pred", terms = "n.people [all]",axis_title.x=20,
           axis.title = c("Number of people [per hour]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

quartz(,7,5)
plot_model(m1, type = "pred", terms = "temperature [all]",axis_title.x=20,
           axis.title = c("Temperature [°C]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

quartz(,7,5)
plot_model(m1, type = "pred", terms = "until_fledging [all]",axis_title.x=20,
           axis.title = c("Time until fledging [days]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)



visreg(m1,xvar="n.people",scale="response")
########### 
m2<-glmer(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
m2<-glmer(visits ~ until_fledging + I(until_fledging ^2) + temperature + n.people +(1|location) , data=DF_hour,family="poisson")
summary(m2)
r2(m2)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$n.people<-0
real<-mean(predict(m2,newdata= DF_hour,type="response"))
null<-mean(predict(m2,newdata= DF_hour_new,type="response"))
(1-real/null)*100
visreg(m2,xvar="n.people",scale="response")

# Single activities #
m3<-glmer(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer  +(1|location) , data=DF_hour,family="poisson")
m3<-glmer(entries ~ until_fledging + I(until_fledging ^2) + temperature  +boats+fisher+(1|location) , data=DF_hour,family="poisson")
summary(m3)
r2(m3)

quartz(,7,5)
plot_model(m3, type = "pred", terms = "boats [all]",axis_title.x=20,
           axis.title = c("Boats [minutes per hour]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
quartz(,7,5)
plot_model(m3, type = "pred", terms = "fisher [all]",axis_title.x=20,
           axis.title = c("Fishing [minutes per hour]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)


quartz(,7,5)
plot_model(m3, type = "pred", terms = "until_fledging [all]",axis_title.x=20,
           axis.title = c("Time until fledging [days]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)

quartz(,7,5)
plot_model(m3, type = "pred", terms = "temperature [all]",axis_title.x=20,
           axis.title = c("Temperature [°C]","Number of nest entries [per hour]"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)



###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$boats <-100
real<-mean(predict(m3,newdata= DF_hour,type="response"))
null<-mean(predict(m3,newdata= DF_hour_new,type="response"))
(1-real/null)*100
visreg(m3,xvar="fisher",scale="response")
# 
m4<-glmer(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer +(1|location) , data=DF_hour,family="poisson")
m4<-glmer(visits ~ until_fledging + I(until_fledging ^2) +  temperature +boats  +fisher +(1|location) , data=DF_hour,family="poisson")
summary(m4)
r2(m4)
###############
# quantifiing the effect in %:
DF_hour_new <-DF_hour
DF_hour_new$boats <-100
real<-mean(predict(m4,newdata= DF_hour,type="response"))
null<-mean(predict(m4,newdata= DF_hour_new,type="response"))
(1-real/null)*100
visreg(m4,xvar="boats",scale="response")

# Pettstadt
#####################
# number of people #
Pett1<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett1<-glm(entries ~ until_fledging +   temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett1)
r2(Pett1)
Pett2<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett2<-glm(visits ~ until_fledging +  temperature + n.people,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett2)
r2(Pett2)
# single activities #
Pett3<-glm(entries ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett3<-glm(entries ~ until_fledging +   temperature  +pedestrian   ,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett3)
r2(Pett3)
Pett4<-glm(visits ~ until_fledging + I(until_fledging ^2) + rain + temperature + biker +boats +pedestrian +fisher +music +dog +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
Pett4<-glm(visits ~ until_fledging + I(until_fledging ^2) +  temperature + boats +swimmer,data=subset(DF_hour, location =="Pettstadt"), family="poisson")
summary(Pett4)
r2(Pett4)

###Park



###Aue


####Bach


