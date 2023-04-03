rm(list=ls())
####Packages####
library(stargazer)
library(lubridate)
library(dplyr)
library(tidyverse)
#library(mapproj)
#library(reshape2)
library(lme4)
library(visreg)
#library(lattice)
library(sjPlot)
library(ggeffects)
#####Location#####

setwd("/Volumes/Bär/Projekte/Eisvogel/DatenUndSkripte_EisvogelCara/")
#####erstelle Dataframe mit kontinuierlicher Zeit#######

time_frame<- seq(ISOdatetime(2021,05,13,05,00,00), ISOdatetime(2021,06,07,21,30,00), by=("min"), tz="Europe/Berlin")

time_frame_DF<-data.frame(time_frame)
#time_frame_DF$time_frame    # just displays the column time_frame

#extract date separately for later merges
StudyPeriod_Dates <- as.Date(time_frame_DF$time_frame, tz="Europe/Berlin")
time_frame_DF <- cbind(time_frame_DF, StudyPeriod_Dates)


#extract time separately for later merges
DayTime <- strftime(time_frame_DF$time_frame, format="%H:%M", tz="Europe/Berlin")
time_frame_DF <- cbind(time_frame_DF, DayTime)

#############################################################################################################################

# read. Data
data<-read.csv(file="Daten_20230110.csv", header=TRUE, sep = ";")[,-28]
#create a Dataframe with Data, duplcates removed i 10.01.2023 (2 people observing, two lines for one entry etc.)
data_DF<-as.data.frame(data)
head(data_DF)
#convert date to a date R can recognise as a new column
Datum<-as.Date(data_DF$Dates, tz="Europe/Berlin")
#add the new date column to the data frame
data_DF<-cbind(data_DF,Datum)

#convert the time to a date-time format
fZeit <- strptime(data_DF$DayTime, format="%H:%M", tz="Europe/Berlin")
#add it to data frame
data_DF <- cbind(data_DF, fZeit)
#seperate the date annd the time again
cZeit<-strftime(data_DF$fZeit, format="%H:%M", tz="Europe/Berlin")
#add the time to the dataframe
data_DF<-cbind(data_DF,cZeit)

#subset the locations
sub_Aue<-subset(data_DF, subset=Standort=="Aue")
sub_Park<-subset(data_DF, subset=Standort=="Park")
sub_Pettstadt<-subset(data_DF, subset=Standort=="Pettstadt")
sub_Bach<-subset(data_DF, subset=Standort=="Bach")


#in a second step you can merge your data into this DF by date or time or date/time (depending on which format your data has)
#the column you want to merge on has to be the exact same format on both DFs
#newDF <- merge(time_frame_DF, data_DF, by.x = c("StudyPeriod_Dates","DayTime"), by.y = c("Datum","cZeit"), all.x = TRUE)
newDF_Aue<- merge(time_frame_DF, sub_Aue, by.x = c("StudyPeriod_Dates","DayTime"), by.y = c("Datum","cZeit"), all.x = TRUE)
newDF_Pettstadt<- merge(time_frame_DF, sub_Pettstadt, by.x = c("StudyPeriod_Dates","DayTime"), by.y = c("Datum","cZeit"), all.x = TRUE)
newDF_Park<- merge(time_frame_DF, sub_Park, by.x = c("StudyPeriod_Dates","DayTime"), by.y = c("Datum","cZeit"), all.x = TRUE)
newDF_Bach<- merge(time_frame_DF, sub_Bach, by.x = c("StudyPeriod_Dates","DayTime"), by.y = c("Datum","cZeit"), all.x = TRUE)

#remove DayTime: 21.30-4:59
#newDFs<-newDF[!(newDF$DayTime<"05:00"|newDF$DayTime>"21:30"),]
newDF_Aue<-newDF_Aue[!(newDF_Aue$DayTime<"05:00"|newDF_Aue$DayTime>"21:30"),]
newDF_Bach<-newDF_Bach[!(newDF_Bach$DayTime<"05:00"|newDF_Bach$DayTime>"21:30"),]
newDF_Pettstadt<-newDF_Pettstadt[!(newDF_Pettstadt$DayTime<"05:00"|newDF_Pettstadt$DayTime>"21:30"),]
newDF_Park<-newDF_Park[!(newDF_Park$DayTime<"05:00"|newDF_Park$DayTime>"21:30"),]

#fill in slots
#Park 30.05 wurde 15min zu früh angefangen--durch mich jetzt wahrscheinlich verschwunden (slot 4)--> ne in slot 3 umbenannt!
newDF_Aue[(newDF_Aue$DayTime >= "05:00" & newDF_Aue$DayTime <= "09:30"),"Slot"] <- "1"
newDF_Aue[(newDF_Aue$DayTime >= "09:30" & newDF_Aue$DayTime <= "14:00"),"Slot"] <- "2"
newDF_Aue[(newDF_Aue$DayTime >= "14:00" & newDF_Aue$DayTime <= "18:30"),"Slot"] <- "3"
newDF_Aue[(newDF_Aue$DayTime >= "18:30" & newDF_Aue$DayTime <= "21:30"),"Slot"] <- "4"

newDF_Bach[(newDF_Bach$DayTime >= "05:00" & newDF_Bach$DayTime <= "09:30"),"Slot"] <- "1"
newDF_Bach[(newDF_Bach$DayTime >= "09:30" & newDF_Bach$DayTime <= "14:00"),"Slot"] <- "2"
newDF_Bach[(newDF_Bach$DayTime >= "14:00" & newDF_Bach$DayTime <= "18:30"),"Slot"] <- "3"
newDF_Bach[(newDF_Bach$DayTime >= "18:30" & newDF_Bach$DayTime <= "21:30"),"Slot"] <- "4"

newDF_Pettstadt[(newDF_Pettstadt$DayTime >= "05:00" & newDF_Pettstadt$DayTime <= "09:30"),"Slot"] <- "1"
newDF_Pettstadt[(newDF_Pettstadt$DayTime >= "09:30" & newDF_Pettstadt$DayTime <= "14:00"),"Slot"] <- "2"
newDF_Pettstadt[(newDF_Pettstadt$DayTime >= "14:00" & newDF_Pettstadt$DayTime <= "18:30"),"Slot"] <- "3"
newDF_Pettstadt[(newDF_Pettstadt$DayTime >= "18:30" & newDF_Pettstadt$DayTime <= "21:30"),"Slot"] <- "4"

newDF_Park[(newDF_Park$DayTime >= "05:00" & newDF_Park$DayTime <= "09:30"),"Slot"] <- "1"
newDF_Park[(newDF_Park$DayTime >= "09:30" & newDF_Park$DayTime <= "14:00"),"Slot"] <- "2"
newDF_Park[(newDF_Park$DayTime >= "14:00" & newDF_Park$DayTime <= "18:30"),"Slot"] <- "3"
newDF_Park[(newDF_Park$DayTime >= "18:30" & newDF_Park$DayTime <= "21:30"),"Slot"] <- "4"

#remove unnessacary days and slots
newDF_Aue<-newDF_Aue[((newDF_Aue$StudyPeriod_Dates=="2021-05-13"&newDF_Aue$Slot=="1")|(newDF_Aue$StudyPeriod_Dates=="2021-05-14"&newDF_Aue$Slot=="4")|(newDF_Aue$StudyPeriod_Dates=="2021-05-15"&newDF_Aue$Slot=="2")|(newDF_Aue$StudyPeriod_Dates=="2021-05-16"&newDF_Aue$Slot=="3")|(newDF_Aue$StudyPeriod_Dates=="2021-05-20"&newDF_Aue$Slot=="3")|(newDF_Aue$StudyPeriod_Dates=="2021-05-21"&newDF_Aue$Slot=="2")|(newDF_Aue$StudyPeriod_Dates=="2021-05-22"&newDF_Aue$Slot=="4")|(newDF_Aue$StudyPeriod_Dates=="2021-05-23"&newDF_Aue$Slot=="1")|(newDF_Aue$StudyPeriod_Dates=="2021-05-27"&newDF_Aue$Slot=="1")|(newDF_Aue$StudyPeriod_Dates=="2021-05-28"&newDF_Aue$Slot=="4")),]
newDF_Bach<-newDF_Bach[((newDF_Bach$StudyPeriod_Dates=="2021-05-13"&newDF_Bach$Slot=="3")|(newDF_Bach$StudyPeriod_Dates=="2021-05-14"&newDF_Bach$Slot=="2")|(newDF_Bach$StudyPeriod_Dates=="2021-05-15"&newDF_Bach$Slot=="4")|(newDF_Bach$StudyPeriod_Dates=="2021-05-16"&newDF_Bach$Slot=="1")|(newDF_Bach$StudyPeriod_Dates=="2021-05-19"&newDF_Bach$Slot=="4")|(newDF_Bach$StudyPeriod_Dates=="2021-05-20"&newDF_Bach$Slot=="1")|(newDF_Bach$StudyPeriod_Dates=="2021-05-22"&newDF_Bach$Slot=="2")|(newDF_Bach$StudyPeriod_Dates=="2021-05-23"&newDF_Bach$Slot=="3")|(newDF_Bach$StudyPeriod_Dates=="2021-05-27"&newDF_Bach$Slot=="3")|(newDF_Bach$StudyPeriod_Dates=="2021-05-29"&newDF_Bach$Slot=="2")|(newDF_Bach$StudyPeriod_Dates=="2021-05-30"&newDF_Bach$Slot=="4")|(newDF_Bach$StudyPeriod_Dates=="2021-05-31"&newDF_Bach$Slot=="1")|(newDF_Bach$StudyPeriod_Dates=="2021-06-01"&newDF_Bach$Slot=="4")|(newDF_Bach$StudyPeriod_Dates=="2021-06-03"&newDF_Bach$Slot=="2")|(newDF_Bach$StudyPeriod_Dates=="2021-06-05"&newDF_Bach$Slot=="1")),]
newDF_Park<-newDF_Park[((newDF_Park$StudyPeriod_Dates=="2021-05-13"&newDF_Park$Slot=="3")|(newDF_Park$StudyPeriod_Dates=="2021-05-15"&newDF_Park$Slot=="1")|(newDF_Park$StudyPeriod_Dates=="2021-05-17"&newDF_Park$Slot=="4")|(newDF_Park$StudyPeriod_Dates=="2021-05-18"&newDF_Park$Slot=="3")|(newDF_Park$StudyPeriod_Dates=="2021-05-19"&newDF_Park$Slot=="2")|(newDF_Park$StudyPeriod_Dates=="2021-05-21"&newDF_Park$Slot=="4")|(newDF_Park$StudyPeriod_Dates=="2021-05-23"&newDF_Park$Slot=="2")|(newDF_Park$StudyPeriod_Dates=="2021-05-24"&newDF_Park$Slot=="1")|(newDF_Park$StudyPeriod_Dates=="2021-05-28"&newDF_Park$Slot=="1")|(newDF_Park$StudyPeriod_Dates=="2021-05-29"&newDF_Park$Slot=="2")|(newDF_Park$StudyPeriod_Dates=="2021-05-30"&(newDF_Park$Slot=="3"|newDF_Park$Slot=="4"))|(newDF_Park$StudyPeriod_Dates=="2021-06-01"&newDF_Park$Slot=="3")|(newDF_Park$StudyPeriod_Dates=="2021-06-02"&newDF_Park$Slot=="4")|(newDF_Park$StudyPeriod_Dates=="2021-06-03"&newDF_Park$Slot=="2")),]
newDF_Pettstadt<-newDF_Pettstadt[((newDF_Pettstadt$StudyPeriod_Dates=="2021-05-24"&newDF_Pettstadt$Slot=="3")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-05-26"&newDF_Pettstadt$Slot=="4")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-05-28"&newDF_Pettstadt$Slot=="2")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-05-29"&newDF_Pettstadt$Slot=="1")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-05-30"&newDF_Pettstadt$Slot=="2")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-06-03"&(newDF_Pettstadt$Slot=="3"|newDF_Pettstadt$Slot=="1"))|(newDF_Pettstadt$StudyPeriod_Dates=="2021-06-04"&(newDF_Pettstadt$Slot=="2"|newDF_Pettstadt$Slot=="4"))|(newDF_Pettstadt$StudyPeriod_Dates=="2021-06-05"&newDF_Pettstadt$Slot=="1")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-06-06"&newDF_Pettstadt$Slot=="4")|(newDF_Pettstadt$StudyPeriod_Dates=="2021-06-07"&newDF_Pettstadt$Slot=="1")),]

#fill in Standort
newDF_Aue$Standort[is.na(newDF_Aue$Standort)] <- "Aue"
newDF_Bach$Standort[is.na(newDF_Bach$Standort)]<- "Bach"
newDF_Park$Standort[is.na(newDF_Park$Standort)]<- "Park"
newDF_Pettstadt$Standort[is.na(newDF_Pettstadt$Standort)]<- "Pettstadt"

#remove unnecessary columns
#newDF<-subset(newDF, select= -c(time_frame,Wetterzustand,DayTime.y,fZeit,Dates))
newDF_Aue<-subset(newDF_Aue, select= -c(time_frame,Wetterzustand,DayTime.y,fZeit,Dates))
newDF_Bach<-subset(newDF_Bach, select= -c(time_frame,Wetterzustand,DayTime.y,fZeit,Dates))
newDF_Park<-subset(newDF_Park, select= -c(time_frame,Wetterzustand,DayTime.y,fZeit,Dates))
newDF_Pettstadt<-subset(newDF_Pettstadt, select= -c(time_frame,Wetterzustand,DayTime.y,fZeit,Dates))

#replace NA in Regen with 0
newDF_Aue$Regen[is.na(newDF_Aue$Regen)]<-"0"
newDF_Bach$Regen[is.na(newDF_Bach$Regen)]<-"0"
newDF_Pettstadt$Regen[is.na(newDF_Pettstadt$Regen)]<-"0"
newDF_Park$Regen[is.na(newDF_Park$Regen)]<-"0"

#replace NA in disturbance and birdactivity with 0
newDF_Park[ , 8:25][is.na(newDF_Park[ ,8:25] ) ] <- 0
newDF_Pettstadt[ , 8:25][is.na(newDF_Pettstadt[ ,8:25] ) ] <- 0
newDF_Aue[ , 8:25][is.na(newDF_Aue[ ,8:25] ) ] <- 0
newDF_Bach[ , 8:25][is.na(newDF_Bach[ ,8:25] ) ] <- 0

#add column Störung 
#newDF_Park['Störung'] <- NA
#newDF_Aue['Störung'] <- NA
#newDF_Pettstadt['Störung'] <- NA
#newDF_Bach['Störung'] <- NA

#character in numeric umwandeln 4:27
sapply(newDF_Aue, is.character)       # um herauszufinden welche Spalte character oder numeric ist
coltonum<-c(4:26)                     #vector der die Columns umfasst

newDF_Aue[ , coltonum] <- apply(newDF_Aue[ , coltonum], 2,           
                                function(x) as.numeric(as.character(x)))#umwandeln der columns, x wird in Na umgewandelt
newDF_Park[ , coltonum] <- apply(newDF_Park[ , coltonum], 2,           
                                 function(x) as.numeric(as.character(x)))
newDF_Bach[ , coltonum] <- apply(newDF_Bach[ , coltonum], 2,           
                                function(x) as.numeric(as.character(x)))
newDF_Pettstadt[ , coltonum] <- apply(newDF_Pettstadt[ , coltonum], 2,           
                                function(x) as.numeric(as.character(x)))

sapply(newDF_Aue, is.numeric)#nochmal checken ob die entsprechenden columns numeric sind

#rowsum ausrechen von Störungen & in Störung eintragen
                   
#newDF_Aue$Störung<-ifelse((rowSums((newDF_Aue[ , c(8:19)]), na.rm=TRUE)=="0"),0,1)                         
#newDF_Park$Störung<-ifelse((rowSums((newDF_Park[ , c(8:19)]), na.rm=TRUE)=="0"),0,1)
#newDF_Bach$Störung<-ifelse((rowSums((newDF_Bach[ , c(8:19)]), na.rm=TRUE)=="0"),0,1)
#newDF_Pettstadt$Störung<-ifelse((rowSums((newDF_Pettstadt[ , c(8:19)]), na.rm=TRUE)=="0"),0,1)


#merge all locations into one dataframe
DF_all<-rbind(newDF_Aue,newDF_Bach,newDF_Park,newDF_Pettstadt)
DF_all$Störung_n<-rowSums(DF_all[ , c(8:19)])

#für jede Störung eine binary column erstellen
#DF_all['F_b'] <-ifelse(DF_all$F=="0",0,1) 
#DF_all['P_b'] <- ifelse(DF_all$P=="0",0,1) 
#DF_all['Kd_b'] <- ifelse(DF_all$Kd=="0",0,1) 
#DF_all['Kj_b'] <- ifelse(DF_all$Kj=="0",0,1)
#DF_all['SUP_b'] <- ifelse(DF_all$SUP=="0",0,1)
#DF_all['A_b'] <- ifelse(DF_all$A=="0",0,1) 
#DF_all['M_b'] <- ifelse(DF_all$M=="0",0,1) 
#DF_all['J_b'] <- ifelse(DF_all$J=="0",0,1)
#DF_all['Fo_b'] <- ifelse(DF_all$Fo=="0",0,1)
#DF_all['Car_b'] <- ifelse(DF_all$Car=="0",0,1) 
#DF_all['H_b'] <- ifelse(DF_all$H=="0",0,1) 
#DF_all['S_b'] <- ifelse(DF_all$S=="0",0,1)

# fill Temp_MW 

for (i in 1:length(unique(DF_all[,"StudyPeriod_Dates"]))){
  
  studyDate <- unique(DF_all[,"StudyPeriod_Dates"])[i]#einzelne Daten werden rausgefiltert und in vector gespeichert
  
  DF_study <- DF_all[DF_all[,"StudyPeriod_Dates"]==studyDate,] #subset vom einzelnen Datum wird erstellt
  
  temp_studyDate <- unique(DF_study[,"Temp_C_MW"][!is.na(DF_study[,"Temp_C_MW"])])#filtert temp heraus
  
  DF_all[DF_all[,"StudyPeriod_Dates"]==studyDate,"Temp_C_MW"] <- temp_studyDate #überträgt Temp auf den ganzen Tageszeitraum (überschreibt alles)
}

#create column with distance. Less= 30 m 1- other 0

DF_all['Distance_b']<-ifelse(DF_all$Entfernung.Störung..m.=="7",                  
       yes = 1,
       no = ifelse(DF_all$Entfernung.Störung..m.<="30",1,0))
#DF_all['Distance_b']<-is.na(0)
DF_all$Distance_b[is.na(DF_all$Distance_b)]<-0
table(DF_all$Distance_b)

#day of the year
DF_all$Day<-yday(DF_all$StudyPeriod_Dates)

#subset Dataset for individual analysis
DF_Pettstadt<-subset(DF_all,Standort=="Pettstadt")
DF_Park<-subset(DF_all,Standort=="Park")
DF_Aue<-subset(DF_all,Standort=="Aue")
DF_Bach<-subset(DF_all,Standort=="Bach")

#add column day untill fledging (Day of fledgig-Day of the year)

DF_Aue$until_fledging<-(yday(as.Date("2021-05-29"))-DF_Aue$Day)
DF_Park$until_fledging<-(yday("2021-06-04")-DF_Park$Day)
DF_Bach$until_fledging<-(yday("2021-06-05")-DF_Bach$Day)
DF_Pettstadt$until_fledging<-(yday("2021-06-08")-DF_Pettstadt$Day)

##############################################################################################################
# Test for duplicates #
# install.packages("gdata")
library(gdata)
duplies<-DF_all[duplicated2(paste(DF_all$StudyPeriod_Dates, DF_all$DayTime,DF_all$Standort)),]
duplies

#transform character Standort & DayTime into factor
DF_all$location<-as.factor(as.character(DF_all$Standort))
DF_all$minutes<-as.numeric(lapply(strsplit(DF_all$DayTime,split=":"),function(x){x[[1]]}))*60+as.numeric(lapply(strsplit(DF_all$DayTime,split=":"),function(x){x[[2]]})) # calculates "minutes of the day"
DF_all$Stunde<-as.numeric(lapply(strsplit(DF_all$DayTime,split=":"),function(x){x[[1]]}))
DF_all$rain<-DF_all$Regen
DF_all$temperature<-DF_all$Temp_C_MW
DF_all$until_fledging<-c(DF_Aue$until_fledging, DF_Bach$until_fledging, DF_Park$until_fledging, DF_Pettstadt $until_fledging)
rm(DF_Aue, DF_Bach, DF_Park, DF_Pettstadt)
DF_all$biker<-DF_all$F
DF_all$boats<-as.numeric(rowSums(DF_all[,c("Kd","Kj","SUP")]))
DF_all$pedestrian<-as.numeric(rowSums(DF_all[,c("P","J","Fo")]))
DF_all$fisher<-DF_all$A
DF_all$music<-DF_all$M
DF_all[DF_all$Car==1 & !is.na(DF_all$Car),]
DF_all$Car[DF_all$Car==1 & !is.na(DF_all$Car)]<-0 # raus weil zu selten und von Hecke abgeschottet/bzw. durch andere Störungen (Parken) erfasst
DF_all$dog<-DF_all$H
DF_all$swimmer<-DF_all$S
DF_all$entry<-as.numeric(DF_all$in.>0)
DF_all$entry<-as.numeric(DF_all$in.>0) # bird enters the nest
DF_all$visit<-as.numeric(rowSums(DF_all[,c("v","in.","ex")])>0) # bird is in front of or enters/leaves the nest
DF_all$n.people<-DF_all$Anz.Personen 
DF_all$distance<-DF_all$Entfernung.Stoerungsdist

##################
# calculate time since last enry #
DF_all$time.since.last.entry<-NA
DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry)]<-0
for(i in 2:nrow(DF_all)){
if(DF_all$minutes[i]==DF_all$minutes[i-1]+1 & DF_all$location[i]==DF_all$location[i-1] & !is.na(DF_all$time.since.last.entry[i-1]) & !is.na(DF_all$entry[i]) & DF_all$Day[i]==DF_all$Day[i-1]){DF_all$time.since.last.entry[i]<-	DF_all$time.since.last.entry[i-1]+1}
if(DF_all$entry[i-1]==1 & !is.na(DF_all$entry[i-1])){DF_all$time.since.last.entry[i]<-1}
#DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry)]<-0
}
DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry) & DF_all$time.since.last.entry==0]<-NA



##################
##################
# calculate time since last enry,
# not counting from beginning but from first observation time
DF_all$time.since.last.entry2<-1
for(i in 2:nrow(DF_all)){
if(DF_all$minutes[i]==DF_all$minutes[i-1]+1 & DF_all$location[i]==DF_all$location[i-1] & DF_all$StudyPeriod_Dates[i]==DF_all$StudyPeriod_Dates[i-1]){DF_all$time.since.last.entry2[i]<-	DF_all$time.since.last.entry2[i-1]+1}
if(DF_all$entry[i-1]==1 & !is.na(DF_all$entry[i-1])){DF_all$time.since.last.entry2[i]<-1}
}

DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry)]<-0
for(i in 2:nrow(DF_all)){
if(DF_all$minutes[i]==DF_all$minutes[i-1]+1 & DF_all$location[i]==DF_all$location[i-1] & !is.na(DF_all$time.since.last.entry[i-1]) & !is.na(DF_all$entry[i]) & DF_all$StudyPeriod_Dates[i]==DF_all$StudyPeriod_Dates[i-1]){DF_all$time.since.last.entry[i]<-	DF_all$time.since.last.entry[i-1]+1}
if(DF_all$entry[i-1]==1 & !is.na(DF_all$entry[i-1])){DF_all$time.since.last.entry[i]<-1}
#DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry)]<-0
}
DF_all$time.since.last.entry[DF_all$entry==1 & !is.na(DF_all$entry) & DF_all$time.since.last.entry==0]<-NA

##################
DF_all$hour<-as.numeric(unlist(lapply(strsplit(as.character(DF_all$DayTime),":"),function(x){x[[1]]})))
DF_all$Stunde_id <-paste(DF_all$StudyPeriod_Dates,DF_all$hour,DF_all$location,sep = "_")
#für jeweilige Stunde die Anzahl an in
DF_hour<-data.frame(entries=tapply(DF_all$in., DF_all$Stunde_id, sum, na.rm=TRUE),
visits=tapply(DF_all$visit, DF_all$Stunde_id, sum, na.rm=TRUE),
                    min=tapply(DF_all$entry, DF_all$Stunde_id, function(x){sum(!is.na(x))}),#alle Beobachtungen in einer h
                    day=tapply(DF_all$Day,DF_all$Stunde_id, mean),
                    until_fledging=tapply(DF_all$until_fledging, DF_all$Stunde_id, mean,na.rm=TRUE),
                    temperature =tapply(DF_all$Temp_C_MW, DF_all$Stunde_id, mean,na.rm=TRUE),
                    n.people=tapply(DF_all$Anz.Personen, DF_all$Stunde_id, sum,na.rm=TRUE),
               
                    biker =tapply(as.numeric(DF_all$biker>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    boats=tapply(as.numeric(DF_all$boats>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    pedestrian =tapply(as.numeric(DF_all$pedestrian>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    fisher=tapply(as.numeric(DF_all$A>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    music=tapply(as.numeric(DF_all$M>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    dog=tapply(as.numeric(DF_all$H>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    swimmer=tapply(as.numeric(DF_all$S>0), DF_all$Stunde_id, sum,na.rm=TRUE),
                    rain=tapply(DF_all$Regen, DF_all$Stunde_id, sum),
                    location=tapply(DF_all$Standort, DF_all$Stunde_id, function(x){as.character(x[1])}),
                    hour=tapply(DF_all$Stunde, DF_all$Stunde_id, mean))
                    
head(DF_all)
DF_all<-DF_all[,c("StudyPeriod_Dates","DayTime","Day","location","rain","temperature","minutes","until_fledging","time.since.last.entry","time.since.last.entry2","entry","visit","n.people","distance","pedestrian","biker","boats","fisher","swimmer","dog","music")]
rm(list=ls()[!ls() %in% c("DF_all","DF_hour")])

save.image("Eisvogel20220111.RData")

# DF_all[,c("StudyPeriod_Dates","DayTime","Day","location","entry","time.since.last.entry")]