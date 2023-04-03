source("Eisvogel_goes_R.R")

#######################################let the statistic begin##############################################

###########################################preparation###########################################
#transform character Standort & DayTime into factor
str(DF_all)
DF_all$Standort<-as.factor(as.character(DF_all$Standort))
DF_all$DayTime<-as.factor(as.character(DF_all$DayTime))
class(DF_all$DayTime)
#
#glmer
#tested Effekte mit dem Minutendatensatz
DF_all$dependent<-as.numeric(DF_all$in.>0)
DF_Pettstadt$dependent<-as.numeric(DF_Pettstadt$in.>0)
DF_Park$dependent<-as.numeric(DF_Park$in.>0)
DF_Aue$dependent<-as.numeric(DF_Aue$in.>0)
DF_Bach$dependent<-as.numeric(DF_Bach$in.>0)
DF_all$dep_v<-as.numeric(DF_all$v>0)
#ci<-subset(DF_all,select = c(28:39))
#cor(ci)# checks for correlation- however is not possible, as there are probably to many NAs in the dataset


######general probability of in.########
colSums(DF_all[,c(21:23)],na.rm=TRUE)
682/12806

#############################test different glmers###############################################################
#models after what is left in significance
summary(test1<-glmer(dependent~Entfernung.Störung..m.+(1|Standort),data=DF_all,family=binomial))
summary(test2<-glmer(dependent~Störung_n+(1|Standort),data=DF_all,family=binomial))
summary(test3<-glmer(dependent~F+M+(1|Standort),data=DF_all,family=binomial))
summary(test4<-glmer(dependent~Anz.Personen+(1|Standort),data=DF_all,family=binomial))

all_glmer<-outreg(list("Model 1" = test1, "Model 2" = test2,"Model3"=test3,"Model4"=test4), title="Modell Results: Minute|all locations", float = TRUE)

stargazer(test1, test2, test3, test4, title="Modell Results: all locations (Minute - Dataset)", align=TRUE)

#muss ich die Beobachter als random effect aufnehmen??? -- um pseudoreplication zu vermeiden?
#als scatterplot darstellen???

##################################glm der einzelnen Standtorte############################################################
#allgemein
#1<-glm(dependent~until_fledging+Entfernung.Störung..m.+Regen+Temp_C_MW+Störung_n,data=DF_...,family=binomial))
#2<-glm(dependent~until_fledging+Rain+Temp_C_MW+Störung_n,data=DF_...,family=binomial))#
#3<-glm(dependent~until_fledging+F+P+J+Fo+M+Kd+Kj+S+Fo+Car+H+SUP+A+Rain+Temp_C_MW,data=DF_...,family=binomial))
#4<-glm(dependent~until_fledging+Rain+Temp_C_MW+Anz.Personen,data=DF_...,family=binomial))

###Pettstadt
sum(is.na(DF_Bach$Regen))
DF_Pettstadt$Car[is.na(DF_Pettstadt$Car)]<-0
DF_Pettstadt$Fo[is.na(DF_Pettstadt$Fo)]<-0
DF_Pettstadt$J[is.na(DF_Pettstadt$J)]<-0

summary(Pet1<-glm(dependent~Störung_n,data=DF_Pettstadt,family=binomial))
summary(Pet2<-glm(dependent~until_fledging+Temp_C_MW+Störung_n,data=DF_Pettstadt,family=binomial))#
summary(Pet3<-glm(dependent~until_fledging+P+Temp_C_MW,data=DF_Pettstadt,family=binomial))
summary(Pet4<-glm(dependent~until_fledging+Temp_C_MW+Anz.Personen,data=DF_Pettstadt,family=binomial))

stargazer(Pet1, Pet2, Pet3, Pet4, title="Modell Results: Pettstadt", align=TRUE)


###Park
DF_Park$Car[is.na(DF_Park$Car)]<-0
DF_Park$SUP[is.na(DF_Park$SUP)]<-0
summary(Par1<-glm(dependent~Störung_n,data=DF_Park,family=binomial))#nicht signifikant
summary(Par2<-glm(dependent~Störung_n,data=DF_Park,family=binomial))#nicht signifikant
summary(Par3<-glm(dependent~F,data=DF_Park,family=binomial))#nicht signifikant
summary(Par4<-glm(dependent~Anz.Personen,data=DF_Park,family=binomial))#nicht signifikant

stargazer(Par1, Par2, Par3, Par4, title="Modell Results: Bamberg Park", align=TRUE)

###Aue

#summary(Aue1<-glm(dependent~Temp_C_MW,data=DF_Aue,family=binomial))--> nichts signifikant
summary(Aue2<-glm(dependent~until_fledging+Regen,data=DF_Aue,family=binomial))#
summary(Aue3<-glm(dependent~until_fledging+Regen,data=DF_Aue,family=binomial))
summary(Aue4<-glm(dependent~until_fledging+Regen,data=DF_Aue,family=binomial))

stargazer(Aue2, Aue3, Aue4, title="Modell Results: River Meadow", align=TRUE)


####Bach

#summary(Bach1<-glm(dependent~.,data=DF_Bach,family=binomial))-->nichts signifikant
summary(Bach2<-glm(dependent~Temp_C_MW,data=DF_Bach,family=binomial))#
summary(Bach3<-glm(dependent~Temp_C_MW,data=DF_Bach,family=binomial))
summary(Bach4<-glm(dependent~Temp_C_MW,data=DF_Bach,family=binomial))
stargazer(Bach2, Bach3, Bach4, title="Modell Results: River stream", align=TRUE)


#################################glm Häufigkeit in der Stunde#############################################################
DF_all$Stunde<-as.numeric(unlist(lapply(strsplit(as.character(DF_all$DayTime),":"),function(x){x[[1]]})))
DF_all$Stunde_id<-paste(DF_all$StudyPeriod_Dates,DF_all$Stunde,DF_all$Standort,sep = "_")


#für jeweilige Stunde die Anzahl an in
DF_hour<-data.frame(in.=tapply(DF_all$in., DF_all$Stunde_id, sum, na.rm=TRUE),
                    min=tapply(DF_all$in., DF_all$Stunde_id, function(x){sum(!is.na(x))}),#alle Beobachtungen in einer h
                    Tag=tapply(DF_all$Day,DF_all$Stunde_id, mean),
                    Anzahl_P=tapply(DF_all$Anz.Personen, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Störungen=tapply(DF_all$Störung, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Fahrrad=tapply(DF_all$F, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Kanu=tapply(DF_all$Kd, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Kajak=tapply(DF_all$Kj, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Person=tapply(DF_all$P, DF_all$Stunde_id, sum,na.rm=TRUE),
                    SUP=tapply(DF_all$SUP, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Angler=tapply(DF_all$A, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Musik=tapply(DF_all$M, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Jogger=tapply(DF_all$J, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Fotograf=tapply(DF_all$Fo, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Auto=tapply(DF_all$Car, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Hund=tapply(DF_all$H, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Schwimmer=tapply(DF_all$S, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Regen=tapply(DF_all$Regen, DF_all$Stunde_id, sum),
                    Standort=tapply(DF_all$Standort, DF_all$Stunde_id, function(x){as.character(x[1])}),
                    Stunde=tapply(DF_all$Stunde, DF_all$Stunde_id, mean))
#day=tapply(DF_all$Day, DF_all$Stunde_id, sum,na.rm=TRUE),

#erstelle Subset nur mit Stunden/60min
DF_hour_h<-subset(DF_hour,!(min<60))
head(DF_hour_h)
DF_hour_h$in.
DF_hour_Park<-subset(DF_hour_h, subset=Standort=="Park")
head(DF_hour_Park)
#DF_hour_Park[,5:16][DF_hour_Park[,5:16] == 0] <- NA
DF_hour_Pettstadt<-subset(DF_hour_h, subset=Standort=="Pettstadt")
#DF_hour_Pettstadt[,5:16][DF_hour_Pettstadt[,5:16] == 0] <- NA
DF_hour_Aue<-subset(DF_hour_h, subset=Standort=="Aue")
#DF_hour_Aue[,5:16][DF_hour_Aue[,5:16] == 0] <- NA
DF_hour_Bach<-subset(DF_hour_h, subset=Standort=="Bach")
#DF_hour_Bach[,5:16][DF_hour_Bach[,5:16] == 0] <- NA
#
DF_hour_Aue$until_fledging<-(yday(as.Date("2021-05-29"))-DF_hour_Aue$Tag)
DF_hour_Park$until_fledging<-(yday("2021-06-04")-DF_hour_Park$Tag)
DF_hour_Bach$until_fledging<-(yday("2021-06-05")-DF_hour_Bach$Tag)
DF_hour_Pettstadt$until_fledging<-(yday("2021-06-08")-DF_hour_Pettstadt$Tag)



#Fütterung über den Tagesverlauf alle Standtorte mit/ohne Störung
#Distance, Disturbance, Rain, Mean Temperature,(until-fledging)\\
#2. Disturbance, Rain, Mean Temperature,(until-fledging)\\
#3. the single disturbances, Rain, Mean Temperature,(until-fledging)\\
#4. Count of People Rain, Mean Temperature,(until-fledging)\\
#summary(modelAue1<-glm(in.~Störungen+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))
#summary(modelAue2<-glm(in.~Person+Fahrrad+Kanu+Kajak+Angler+Schwimmer+SUP+Auto+Jogger+Musik+Hund+Fotograf+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))
#summary(modelAue3<-glm(in.~Anzahl_P+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))



summary(model1<-glmer(in.~Störungen+Regen+Tag+I(Tag^2)+(1|Standort),data=DF_hour_h,family="poisson"))
summary(model2<-glmer(in.~Person+Kanu+Angler+Regen+Tag+I(Tag^2)+Stunde+I(Stunde^2)+(1|Standort),data=DF_hour_h,family="poisson"))
summary(model3<-glmer(in.~Anzahl_P+Regen+Tag+I(Tag^2)+(1|Standort),data=DF_hour_h,family="poisson"))

stargazer(model1, model2, model3,title="Model results: all locations (Hour - Dataset)",align = TRUE)

#Fütterung über den Tagesverlauf einzelne Standtorte mit/ohne Störungen

summary(modelAue1<-glm(in.~I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))
summary(modelAue2<-glm(in.~Fahrrad+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))
summary(modelAue3<-glm(in.~I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue,family="poisson"))

stargazer(modelAue1, modelAue2, modelAue3,title="Model results: River Meadow (Hour - Dataset)",align = TRUE)


summary(modelBach1<-glm(in.~Stunde+until_fledging+I(until_fledging^2),data=DF_hour_Bach,family="poisson"))
summary(modelBach2<-glm(in.~Stunde+until_fledging+I(until_fledging^2),data=DF_hour_Bach,family="poisson"))
summary(modelBach3<-glm(in.~Stunde+until_fledging+I(until_fledging^2),data=DF_hour_Bach,family="poisson"))

stargazer(modelBach1, modelBach2, modelBach3,title="Model results: River Stream (Hour - Dataset)",align = TRUE)


summary(modelPark1<-glm(in.~Stunde,data=DF_hour_Park,family="poisson"))
summary(modelPark2<-glm(in.~Stunde,data=DF_hour_Park,family="poisson"))
summary(modelPark3<-glm(in.~Stunde,data=DF_hour_Park,family="poisson"))

stargazer(modelPark1, modelPark2, modelPark3,title="Model results: Bamberg Park (Hour - Dataset)",align = TRUE)


summary(modelPettstadt1<-glm(in.~Störungen,data=DF_hour_Pettstadt,family="poisson"))
#summary(modelPettstadt2<-glm(in.~,data=DF_hour_Pettstadt,family="poisson"))
summary(modelPettstadt3<-glm(in.~Anzahl_P,data=DF_hour_Pettstadt,family="poisson"))

stargazer(modelPettstadt1, modelPettstadt3,title="Model results: Pettstadt (Hour - Dataset)",align = TRUE)


#visreg(model,xvar = "Tag",by="Störungen",scale = "response")
#visreg(model,xvar = "Störungen",by="Stunde",scale = "response")
#nach und nach nicht signifikante Variable rausschmeißen, bis alles signifikant ist!!!!!!
#Tage zu Day of the year, um sich anzuschauen wie sich die Frequenz im Brutverlauf verändert!!!!!
#Standorte vergleichen eg. Pettstadt und Park - wie unterschiedlich die Störung/Aktivität ist!!!!!!



####################weitere testmöglichkeiten############################
#sums von In`s
  sum(DF_hour_Bach$in.)
in.Aue.day<-DF_hour_Aue%>%group_by(Tag) %>% summarise(in. = sum(in.))
names(in.Aue.day)[names(in.Aue.day)=="in."] <- "in_Aue"
in.Park.day<-DF_hour_Park%>%group_by(Tag) %>% summarise(in. = sum(in.))
names(in.Park.day)[names(in.Park.day)=="in."] <- "in_Park"
in.Pett.day<-DF_hour_Pettstadt%>%group_by(Tag) %>% summarise(in. = sum(in.))
names(in.Pett.day)[names(in.Pett.day)=="in."] <- "in_Pettstadt"
in.Bach.day<-DF_hour_Bach%>%group_by(Tag) %>% summarise(in. = sum(in.))
names(in.Bach.day)[names(in.Bach.day)=="in."] <- "in_Bach"

in.all <- merge(in.Aue.day, in.Bach.day, by = 'Tag',all.x = TRUE,all.y = TRUE)
in.all <- merge(in.all, in.Pett.day, by = 'Tag',all.x = TRUE,all.y = TRUE)
in.all <- merge(in.all, in.Park.day, by = 'Tag',all.x = TRUE,all.y = TRUE)

######sums of the disturbances####
sum_all<-colSums(DF_all[,c(4,8:19,21,23)],na.rm = TRUE)
sum_Aue<-colSums(DF_Aue[,c(4,8:19,21,23)],na.rm = TRUE)
sum_Bach<-colSums(DF_Bach[,c(4,8:19,21,23)],na.rm = TRUE)
sum_Pettstadt<-colSums(DF_Pettstadt[,c(4,8:19,21,23)],na.rm = TRUE)
sum_Park<-colSums(DF_Park[,c(4,8:19,21,23)],na.rm=TRUE)

view(sum_Aue)
view(sum_Park)
view(sum_Pettstadt)
view(sum_Bach)
