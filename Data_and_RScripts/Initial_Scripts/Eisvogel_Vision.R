setwd("C:/Users/Cara/Documents/Uni/Masterarbeit_07.08.2021/Daten")
source("Eisvogel_goes_R.R")
source("Eisvogel_Stat.R")

###########PLot of main disturbances#######################
#in hour
png("Disturbance_Park_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=DF_hour_Park)+
  geom_point(mapping = aes(x = Stunde, y = Person),colour="red",size=4)+
  #geom_smooth(se=FALSE,method=loess,mapping = aes(x = Stunde, y = Person),colour="red")+
  geom_point(mapping = aes(x = Stunde, y = Fahrrad),colour="blue",size=4)+
  #geom_smooth(se=FALSE,method=loess,mapping = aes(x = Stunde, y = Fahrrad),colour="blue")+
  geom_point(mapping = aes(x = Stunde, y = Jogger),colour="green",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Musik),colour="yellow",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Hund),colour="orange",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Fotograf),colour="darkgreen",size=4)+
  labs(x = "Time",y="Count of disturbances (min)", title="")+
  theme(text = element_text(size = 18))
dev.off()


#   theme_bw()

png("Disturbance_Pettstadt_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=DF_hour_Pettstadt)+
  geom_point(mapping = aes(x = Stunde, y = SUP),colour="pink",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Fahrrad),colour="blue",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Person),colour="red",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Kanu),colour="yellow3",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Musik),colour="yellow",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Hund),colour="orange",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Kajak),colour="black",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Schwimmer),colour="brown",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Angler),colour="purple",size=4)+
  labs(x = "Time",y="Count of disturbances (min)", title="")+
  theme(text = element_text(size = 18))
dev.off()

png("Disturbance_Aue_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=DF_hour_Aue)+
  geom_point(mapping = aes(x = Stunde, y = Fahrrad),colour="blue",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Person),colour="red",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Auto),colour="grey",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Musik),colour="yellow",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Hund),colour="orange",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Angler),colour="purple",size=4)+
  labs(x = "Time",y=" Count of disturbances (min)", title="")+
  theme(text = element_text(size = 18))
dev.off()


png("Disturbance_Bach_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=DF_hour_Bach)+
  geom_point(mapping = aes(x = Stunde, y = Person),colour="red",size=4)+
  geom_point(mapping = aes(x = Stunde, y = Auto),colour="grey",size=4)+
 # geom_point(mapping = aes(x = Stunde, y = Hund),colour="orange")+
  labs(x = "Time",y="Count of disturbances (min)", title="")+
  theme(text = element_text(size = 18))
dev.off()


########################################################################################
#facet_wrap(~Standort, nrow = 2)

######plot glmer
#visreg(test, xvar= "Entfernung.Störung..m.", type="contrast", gg=TRUE,
#      scale="response",xlab="Distance of the Disturbance", ylab="Propability of 'in'",
#     line=list(col="blue"),fill=list(col="orange"))
#by="Regen"


png("Distance_Disturbance_all_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 3.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(test1, type = "pred", terms = "Entfernung.Störung..m.",axis_title.x=20,
           axis.title = c("Distance of the disturbance (m)","Chance of entering"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Disturbance_all_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(test2, type = "pred", terms = "Störung_n[all]",
           axis.title = c("Disturbances","Chance of entering"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20,)
dev.off()

png("Bicycles+Music_all_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(test3, type = "pred", terms = c("F","M"),title = "",
           axis.title = c("Bicycles","Chance of entering"),legend.title = "Music",line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20,)
dev.off()

png("Nr.People_all_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(test4, type = "pred", terms = "Anz.Personen[all]",
           axis.title = c("Count of People","Chance of entering"),line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

######plot glm's der einzelnen Standtorte
png("Disturbance_Pett_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet1,type="pred",terms="Störung_n[all]",line.size = 1.1,title = "",axis.title = c("Disturbance","Chance of entering"))+
             font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("U.Fledging+Temp_Pett_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet2, type = "pred", terms = c("until_fledging","Temp_C_MW"),title = "",
           line.size = 1.1,legend.title = c("°C"),axis.title = c("Days until fledging","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("U.Fledging+Disturbance_Pett_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet2, type = "pred", terms = c("Störung_n[all]","until_fledging"),title = "",
           line.size = 1.1,legend.title = c("Days until fledging"),axis.title = c("Disturbance","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Person_Pett_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet3, type = "pred", terms = c("P"),title = "",
           line.size = 1.1,axis.title = c(" Person/Walker ","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Nr.P_Pett_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet4, type = "pred", terms = c("Anz.Personen[all]"),title = "",
           line.size = 1.1,axis.title = c("Count of People","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()


png("Disturbance_Park_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Par2,type="pred",terms="Störung_n[all]",line.size = 1.1,title = "",axis.title = c("Disturbance","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Bicycle_Park_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Par3,type="pred",terms="F",line.size = 1.1,title = "",axis.title = c("Bicycles","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Nr.P_Park_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Par4,type="pred",terms="Anz.Personen[all]",line.size = 1.1,title = "",axis.title = c("Count of People","Chance of entering"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("U.Fledging+Regen_Aue_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Aue2, type = "pred", terms = c("until_fledging","Regen"),title = "",
           line.size = 1.1,legend.title = "Rain",axis.title = c("Days until fledging","Chance of entering"))+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

png("Temp_Bach_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Bach2, type="pred",terms="Temp_C_MW",title="",line.size = 1.1,axis.title = c("Temperature","Chance of entering"))+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

############################## plot glmer innerhalb einer Stunde
#Fütterung über Tagesverlauf- alle Standorte


#summary(model1<-glmer(in.~Störungen+Regen+Tag+I(Tag^2)+(1|Standort),data=DF_hour_h,family="poisson"))
#summary(model2<-glmer(in.~Person+Kanu+Angler+Regen+Tag+I(Tag^2)+Stunde+I(Stunde^2)+(1|Standort),data=DF_hour_h,family="poisson"))
#summary(model3<-glmer(in.~Anzahl_P+Regen+Tag+I(Tag^2)+(1|Standort),data=DF_hour_h,family="poisson"))
png("Disturbance_all_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(model1, type = "pred", terms = c("Störungen[all]"),title = "",
           axis.title = c("Disturbances per hour","Count of entries"),
           line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

plot_model(model2, type = "pred", terms = c("Person[all]"),title = "",
           axis.title = c("Person/Walker","Probability of a bird entering the chamber"),line.size = 1.1)
plot_model(model2, type = "pred", terms = c("Angler[all]"),title = "",
           axis.title = c("Fisher","Probability of a bird entering the chamber"),line.size = 1.1)
plot_model(model2, type = "pred", terms = c("Kanu[all]"),title = "",
           axis.title = c("Canoe","Probability of a bird entering the chamber"),line.size = 1.1)
plot_model(model3, type = "pred", terms = c("Anzahl_P[all]","Tag"),title = "",
           axis.title = c("Count of People","Probability of a bird entering the chamber"),legend.title = "Day of the year",line.size = 1.1)

png("Hour_all_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(model2,type="pred",terms="Stunde[all]",title = "",
           axis.title = c("Time","Chance of entering (%)"),line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Day_all_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(model2,type="pred",terms="Tag[all]",title = "",
           axis.title = c("Day of the year","Count of entries"),line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()



#Fütterung über den Tagesverlauf einzelne Standorte

visreg(modelAue1,xvar="until_fledging", by="Stunde", scale="response",ylab="Probability of a bird entering the breeding chamber",xlab="Day until fledging")
visreg(modelAue1,xvar="Stunde", scale="response",ylab="Probability of a bird entering the breeding chamber",xlab="Hour")

plot_model(modelAue1, type = "pred", terms = c("Stunde"),title = "",
    axis.title = c("Hour","Probability of a bird entering the chamber"),line.size = 1.1)

#plot_model(modelAue1, type = "pred", terms = c("until_fledging","Stunde"),title = "",
#           axis.title = c("Day until fledging","Probability of a bird entering the chamber"),legend.title = "Hour",line.size = 1.1)

#visreg(modelBach1,xvar="until_fledging", by="Stunde", scale="response")
plot_model(modelBach1, type = "pred", terms = c("until_fledging","Stunde"),title = "",
           axis.title = c("Day until fledging","Probability of a bird entering the chamber"),legend.title = "Hour",line.size = 1.1)
png("Hour_Bach_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(modelBach1, type = "pred", terms = c("Stunde"),title = "",
           axis.title = c("Time","Count of entries"),line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Hour_Park_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(modelPark1, type = "pred", terms = c("Stunde"),title = "",
           axis.title = c("Time","Count of entries"),line.size = 1.1)+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Dist._Pett_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(modelPettstadt1, type = "pred", terms = c("Störungen[all]"),title = "",
           axis.title = c("Disturbances per hour","Count of entries"),line.size = 1.1)+
font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()


plot_model(modelPettstadt3, type = "pred", terms = c("Anzahl_P"),title = "",
           axis.title = c("Count of People","Probability of a bird entering the chamber"),line.size = 1.1)
#sum in.


png("IN's.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=in.all)+
  geom_point(mapping = aes(x = Tag, y = in_Aue),colour="deeppink3",size=4)+
  geom_point(mapping = aes(x = Tag, y = in_Bach),colour="yellow",size=4)+
  geom_point(mapping = aes(x = Tag, y = in_Pettstadt),colour="darkorchid2",size=4)+
  geom_point(mapping = aes(x = Tag, y = in_Park),colour="blue1",size=4)+
  labs(x = "Day of the year",y="Times of entries")+
  theme(text = element_text(size = 18))
 # labs(title = "Count of a bird entering the breeding chamber per day")
dev.off()



png("Temperature during Season.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=DF_all)+
   geom_line(mapping = aes(x = Day, y = Temp_C_MW),colour="black", size=1.2)+
  geom_point(mapping = aes(x = Day, y = Temp_C_MW),colour="black",size = 2)+
  labs(x = "Day of the year",y="°C")+
  theme(text = element_text(size = 16))
dev.off()


###Störungen by Stunde

visreg(model1,xvar="Störungen", by="Stunde", scale="response")



ggplot(data = DF_hour, aes(x = Stunde, y = Fahrrad)) +
  geom_point(aes(color = Standort))+
  facet_wrap(~ Tag, nrow = 4)

# geom_abline(aes(slope = mean(P), intercept = mean(P, na.rm=T) ))

