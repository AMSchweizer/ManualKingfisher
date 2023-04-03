source("Eisvogel_goes_R.R")

###########################################preparation###########################################
#transform character Standort & DayTime into factor
str(DF_all)
DF_all$Standort<-as.factor(as.character(DF_all$Standort))
DF_all$DayTime<-as.factor(as.character(DF_all$DayTime))
class(DF_all$DayTime)
#
#tested Effekte mit dem Minutendatensatz
DF_all$dep<-as.numeric(DF_all$v>0)
DF_Pettstadt$dep<-as.numeric(DF_Pettstadt$v>0)
DF_Park$dep<-as.numeric(DF_Park$v>0)
DF_Aue$dep<-as.numeric(DF_Aue$v>0)
DF_Bach$dep<-as.numeric(DF_Bach$v>0)
DF_all$dep_v<-as.numeric(DF_all$v>0)
#ci<-subset(DF_all,select = c(28:39))
#cor(ci)# checks for correlation- however is not possible, as there are probably to many NAs in the dataset

#glmer

#1. summary(dep1<-glmer(dep~Entfernung.Störung..m.+Störung_n+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))
#2. summary(dep2<-glmer(dep~Störung_n+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))
#3. summary(dep3<-glmer(dep~P+F+SUP+M+S+Fo+Car+Kd+Kj+A+H+J+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))
#4. summary(dep4<-glmer(dep~Anz.Personen+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))

summary(dep1<-glmer(dep~Störung_n+Regen+(1|Standort),data=DF_all,family=binomial))
summary(dep2<-glmer(dep~Störung_n+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))
summary(dep3<-glmer(dep~M+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))
summary(dep4<-glmer(dep~Anz.Personen+Regen+Temp_C_MW+(1|Standort),data=DF_all,family=binomial))

stargazer(dep1,dep2,dep3,dep4,title = "Model Results: all locations")

#glm einzelne Standorte
#1. Distance, Disturbance, Rain, Mean Temperature,(until-fledging)\\
#2. Disturbance, Rain, Mean Temperature,(until-fledging)\\
#3. the single disturbances, Rain, Mean Temperature,(until-fledging)\\
#4. Count of People, Rain, Mean Temperature,(until-fledging)\\

#1. summary(dep1<-glm(dep~Entfernung.Störung..m.+Störung_n+Regen+Temp_C_MW+until_fledging,data=DF_all,family=binomial))
#2. summary(dep2<-glm(dep~Störung_n+Regen+Temp_C_MW+until_fledging,data=DF_all,family=binomial))
#3. summary(dep3<-glm(dep~P+F+SUP+M+S+Fo+Car+Kd+Kj+A+H+J+Regen+Temp_C_MW+until_fledging,data=DF_all,family=binomial))
#4. summary(dep4<-glm(dep~Anz.Personen+Regen+Temp_C_MW+until_fledging,data=DF_all,family=binomial))
sum(is.na(DF_Pettstadt$Regen))
summary(Pet_vor1<-glm(dep~Störung_n+Temp_C_MW,data=DF_Pettstadt,family=binomial))
summary(Pet_vor2<-glm(dep~Störung_n+Temp_C_MW+until_fledging,data=DF_Pettstadt,family=binomial))
summary(Pet_vor3<-glm(dep~P+Temp_C_MW+until_fledging,data=DF_Pettstadt,family=binomial))
summary(Pet_vor4<-glm(dep~Anz.Personen+Temp_C_MW+until_fledging,data=DF_Pettstadt,family=binomial))

stargazer(Pet_vor1,Pet_vor2,Pet_vor3,Pet_vor4,title = "Model Results: Pettstadt", align=TRUE)

summary(Par_vor1<-glm(dep~Regen+Temp_C_MW+until_fledging,data=DF_Park,family=binomial))
summary(Par_vor2<-glm(dep~Regen+Temp_C_MW+until_fledging,data=DF_Park,family=binomial))
summary(Par_vor3<-glm(dep~Regen+Temp_C_MW+until_fledging,data=DF_Park,family=binomial))
summary(Par_vor4<-glm(dep~Regen+Temp_C_MW+until_fledging,data=DF_Park,family=binomial))

stargazer(Par_vor1,Par_vor2,Par_vor3,Par_vor4,title = "Model Results: Park", align=TRUE)

sum(is.na(DF_Bach$Regen))
summary(Bach_vor1<-glm(dep~Temp_C_MW+until_fledging,data=DF_Bach,family=binomial))
summary(Bach_vor2<-glm(dep~Temp_C_MW+until_fledging,data=DF_Bach,family=binomial))
summary(Bach_vor3<-glm(dep~Temp_C_MW+until_fledging,data=DF_Bach,family=binomial))
summary(Bach_vor4<-glm(dep~Temp_C_MW+until_fledging,data=DF_Bach,family=binomial))

stargazer(Bach_vor1,Bach_vor2,Bach_vor3,Bach_vor4,title = "Model Results: River Stream", align=TRUE)


#summary(Aue_vor1<-glm(dep~Temp_C_MW,data=DF_Aue,family=binomial))
summary(Aue_vor2<-glm(dep~until_fledging,data=DF_Aue,family=binomial))
summary(Aue_vor3<-glm(dep~H+until_fledging,data=DF_Aue,family=binomial))
summary(Aue_vor4<-glm(dep~until_fledging,data=DF_Aue,family=binomial))

stargazer(Aue_vor2,Aue_vor3,Aue_vor4,title = "Model Results: River Meadow", align=TRUE)

#####Häufigkeiten in der Stunde
DF_all$Stunde<-as.numeric(unlist(lapply(strsplit(as.character(DF_all$DayTime),":"),function(x){x[[1]]})))
DF_all$Stunde_id<-paste(DF_all$StudyPeriod_Dates,DF_all$Stunde,DF_all$Standort,sep = "_")


DF_hour_vor<-data.frame(vor=tapply(DF_all$v, DF_all$Stunde_id, sum, na.rm=TRUE),
                    min=tapply(DF_all$in., DF_all$Stunde_id, function(x){sum(!is.na(x))}),#alle Beobachtungen in einer h
                    Tag=tapply(DF_all$Day,DF_all$Stunde_id, mean),
                    Störungen=tapply(DF_all$Störung, DF_all$Stunde_id, sum,na.rm=TRUE),
                    Anzahl_P=tapply(DF_all$Anz.Personen, DF_all$Stunde_id, sum,na.rm=TRUE),
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
DF_hour_h_vor<-subset(DF_hour_vor,!(min<60))

DF_hour_Park_vor<-subset(DF_hour_h_vor, subset=Standort=="Park")
#DF_hour_Park_vor[,5:16][DF_hour_Park_vor[,5:16] == 0] <- NA
DF_hour_Pettstadt_vor<-subset(DF_hour_h_vor, subset=Standort=="Pettstadt")
#DF_hour_Pettstadt_vor[,5:16][DF_hour_Pettstadt_vor[,5:16] == 0] <- NA
DF_hour_Aue_vor<-subset(DF_hour_h_vor, subset=Standort=="Aue")
#DF_hour_Aue_vor[,5:16][DF_hour_Aue_vor[,5:16] == 0] <- NA
DF_hour_Bach_vor<-subset(DF_hour_h_vor, subset=Standort=="Bach")
#DF_hour_Bach_vor[,5:16][DF_hour_Bach_vor[,5:16] == 0] <- NA


DF_hour_Aue_vor$until_fledging<-(yday(as.Date("2021-05-29"))-DF_hour_Aue_vor$Tag)
DF_hour_Park_vor$until_fledging<-(yday("2021-06-04")-DF_hour_Park_vor$Tag)
DF_hour_Bach_vor$until_fledging<-(yday("2021-06-05")-DF_hour_Bach_vor$Tag)
DF_hour_Pettstadt_vor$until_fledging<-(yday("2021-06-08")-DF_hour_Pettstadt_vor$Tag)


#Fütterung über den Tagesverlauf alle Standtorte mit/ohne Störung

#1. summary(dep_hour1<-glmer(vor~Störung_n+Regen+Tag+I(Tag^2)+Stunde+I(Stunde^2)+(1|Standort),data=DF_hour_h_vor,family=poisson))
#2. summary(dep_hour2<-glmer(vor~Person+Fahrrad+SUP+Musik+Schwimmer+Fotograf+Auto+Kanu+Kajak+Angler+Hund+Jogger+Regen+Tag+I(Tag^2)+Stunde+I(Stunde^2)+(1|Standort),data=DF_hour_h_vor,family=poisson))
#3. summary(dep_hour3<-glmer(vor~Anzahl_P+Regen+Tag+I(Tag^2)+Stunde+I(Stunde^2)+(1|Standort),data=DF_hour_h_vor,family=poisson))

summary(dep_hour1<-glmer(vor~Regen+Tag+I(Tag^2)+Stunde+(1|Standort),data=DF_hour_h_vor,family=poisson))
summary(dep_hour2<-glmer(vor~Person+Fahrrad+SUP+Schwimmer+Fotograf+Kanu+Kajak+Angler+Regen+Tag+I(Tag^2)+Stunde+(1|Standort),data=DF_hour_h_vor,family=poisson))
summary(dep_hour3<-glmer(vor~Regen+Tag+I(Tag^2)+Stunde+(1|Standort),data=DF_hour_h_vor,family=poisson))

stargazer(dep_hour1,dep_hour2,dep_hour3,title = "Model Results: all locations (Hour)", align=TRUE)


#Fütterung über den Tagesverlauf einzelne Standtorte mit/ohne Störungen
#1. summary(dep_hour1<-glm(vor~Störungen+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_h_vor,family=poisson))
#2. summary(dep_hour2<-glm(vor~Person+Fahrrad+SUP+Musik+Schwimmer+Fotograf+Auto+Kanu+Kajak+Angler+Hund+Jogger+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_h_vor,family=poisson))
#3. summary(dep_hour3<-glm(vor~Anzahl_P+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_h_vor,family=poisson))

sum(is.na(DF_hour_Aue_vor$Person))

summary(dep_hourAue1<-glm(vor~Störungen+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue_vor,family=poisson))
summary(dep_hourAue2<-glm(vor~Person+Fahrrad+Auto+Hund+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue_vor,family=poisson))
summary(dep_hourAue3<-glm(vor~Anzahl_P+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Aue_vor,family=poisson))

stargazer(dep_hourAue1,dep_hourAue2,dep_hourAue3,title = "Model Results: River Meadow (Hour)", align=TRUE)

summary(dep_hour_Bach1<-glm(vor~Störungen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Bach_vor,family=poisson))
summary(dep_hour_Bach2<-glm(vor~Auto+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Bach_vor,family=poisson))
summary(dep_hour_Bach3<-glm(vor~Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Bach_vor,family=poisson))

stargazer(dep_hour_Bach1,dep_hour_Bach2,dep_hour_Bach3,title = "Model Results: River Stream (Hour)", align=TRUE)

summary(dep_hour_Pettstadt1<-glm(vor~Störungen+Regen+I(Stunde^2)+I(until_fledging^2),data=DF_hour_Pettstadt_vor,family=poisson))
summary(dep_hour_Pettstadt2<-glm(vor~SUP+Kanu+Angler+Regen+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Pettstadt_vor,family=poisson))
summary(dep_hour_Pettstadt3<-glm(vor~Anzahl_P+Regen+I(Stunde^2)+I(until_fledging^2),data=DF_hour_Pettstadt_vor,family=poisson))

stargazer(dep_hour_Pettstadt1,dep_hour_Pettstadt2,dep_hour_Pettstadt3,title = "Model Results: Pettstadt (Hour)", align=TRUE)

summary(dep_hour_Park1<-glm(vor~Störungen+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Park_vor,family=poisson))
summary(dep_hour_Park2<-glm(vor~Person+Fotograf+Auto+Kanu+Hund+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Park_vor,family=poisson))
summary(dep_hour_Park3<-glm(vor~Anzahl_P+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Park_vor,family=poisson))

stargazer(dep_hour_Park1,dep_hour_Park2,dep_hour_Park3,title = "Model Results: Bamberg Park (Hour)", align=TRUE)

#sums of vor
sum(DF_hour_Pettstadt_vor$vor)

vor.Aue.day<-DF_hour_Aue_vor%>%group_by(Tag) %>% summarise(vor = sum(vor))
names(vor.Aue.day)[names(vor.Aue.day)=="vor"] <- "vor_Aue"

vor.Park.day<-DF_hour_Park_vor%>%group_by(Tag) %>% summarise(vor = sum(vor))
names(vor.Park.day)[names(vor.Park.day)=="vor"] <- "vor_Park"

vor.Pett.day<-DF_hour_Pettstadt_vor%>%group_by(Tag) %>% summarise(vor = sum(vor))
names(vor.Pett.day)[names(vor.Pett.day)=="vor"] <- "vor_Pettstadt"

vor.Bach.day<-DF_hour_Bach_vor%>%group_by(Tag) %>% summarise(vor = sum(vor))
names(vor.Bach.day)[names(vor.Bach.day)=="vor"] <- "vor_Bach"

vor.all <- merge(vor.Aue.day, vor.Bach.day, by = 'Tag',all.x = TRUE,all.y = TRUE)
vor.all <- merge(vor.all, vor.Pett.day, by = 'Tag',all.x = TRUE,all.y = TRUE)
vor.all <- merge(vor.all, vor.Park.day, by = 'Tag',all.x = TRUE,all.y = TRUE)

