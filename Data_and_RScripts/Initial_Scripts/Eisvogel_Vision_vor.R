setwd("C:/Users/Cara/Documents/Uni/Masterarbeit_07.08.2021/Daten")
source("Eisvogel_goes_R.R")
source("Eisvogel_Stat_vor.R")

####glmer- all locations
png("Disturbance_all_vor_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep1, type = "pred", terms = c("Störung_n[all]"),
           axis.title = c("Disturbances","Chance of perching"),line.size = 1.1,title="")+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

png("Temp+Rain_all_vor_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep2, type = "pred", terms = c("Temp_C_MW[all]","Regen"),
           axis.title = c("Temperature (°C)","Chance of perching"),legend="Rain",line.size = 1.1,title="")+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

plot_model(dep3, type = "pred", terms = c("M"),
           axis.title = c("Music","Probability of a bird sitting infront of the chamber"),legend="Rain",line.size = 1.1,title="")
png("Nr.P_all_vor_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep4, type = "pred", terms = c("Anz.Personen[all]"),
           axis.title = c("Count of People","Chance of perching"),legend="Rain",line.size = 1.1,title="")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()



plot_model(Pet_vor1,type="pred",terms=c("Störung_n[all]","Temp_C_MW"),
           axis.title = c("Disturbance","Pobability of a bird sitting infront of the chamber"),legend="Temperature",line.size = 1.1,title="")
plot_model(Pet_vor2,type="pred",terms=c("Störung_n[all]","until_fledging"),
           axis.title = c("Disturbance","Pobability of a bird sitting infront of the chamber"),legend="Days until fledging",line.size = 1.1,title="")

png("Person_Pettstadt_vor_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Pet_vor3,type="pred",terms=c("P"),
           axis.title = c("Persons/Walker","Chance of perching"),line.size = 1.1,title="")+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

plot_model(Pet_vor4,type="pred",terms=c("Anz.Personen[all]","until_fledging"),
           axis.title = c("Count of People","Pobability of a bird sitting infront of the chamber"),legend="Days until fledging",line.size = 1.1,title="")

png("Day until fledging_Park_vor_min.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(Par_vor1,type="pred",terms=c("until_fledging"),
           axis.title = c("Days until fledging","Chance of perching"),legend="Temperature",line.size = 1.1,title="")+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()

plot_model(Par_vor1,type="pred",terms=c("Temp_C_MW","Regen"),
           axis.title = c("Temperature","Pobability of a bird sitting infront of the chamber"),legend="Rain",line.size = 1.1,title="")

plot_model(Bach_vor1,type="pred",terms=c("until_fledging","Temp_C_MW"),
           axis.title = c("Days until fledging","Pobability of a bird sitting infront of the chamber"),legend.title = "Temperature",line.size = 1.1,title="")


plot_model(Aue_vor2,type="pred",terms=c("until_fledging"),
           axis.title = c("Days until fledging","Pobability of a bird sitting infront of the chamber"),legend="Temperature",line.size = 1.1,title="")
#plot_model(Aue_vor3,type="pred",terms=c("H","until_fledging"),
 #          axis.title = c("Dog","Pobability of a bird sitting infront of the chamber"),legend="Days until fedging",line.size = 1.1,title="")

###glm_hour_all locations

visreg(dep_hour1,xvar="Tag", by="Stunde", scale="response")

png("Day+Rain_all_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep_hour1,type="pred",terms=c("Tag[all]","Regen"),line.size = 1.1,title = "",
           axis.title = c("Day of the Year","Count of minutes perching"),legend.title = "Rain")+
  font_size( axis_title.x=15, axis_title.y=15, labels.x=15, labels.y=15)
dev.off()



plot_model(dep_hour2,type="pred",terms=c("Person[all]"),line.size = 1.1,title = "",
           axis.title = c("Person/Walker","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Fahrrad[all]"),line.size = 1.1,title = "",
           axis.title = c("Bicycle","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("SUP[all]"),line.size = 1.1,title = "",
           axis.title = c("SUP","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Schwimmer[all]"),line.size = 1.1,title = "",
           axis.title = c("Swimmer","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Fotograf[all]"),line.size = 1.1,title = "",
           axis.title = c("Photographer","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Kanu[all]"),line.size = 1.1,title = "",
           axis.title = c("Canoe","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Kajak[all]"),line.size = 1.1,title = "",
           axis.title = c("Kajak","Pobability of a bird sitting infront of the chamber"))
plot_model(dep_hour2,type="pred",terms=c("Angler[all]"),line.size = 1.1,title = "",
           axis.title = c("Fishermen","Pobability of a bird sitting infront of the chamber"))

plot_model(dep_hour3,type="pred",terms=c("Regen[all]"),line.size = 1.1,title = "",
           axis.title = c("Rain","Pobability of a bird sitting infront of the chamber"))


png("Day until fledging_Aue_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hourAue1,xvar="until_fledging", scale="response",
       line=list(col="black", cex=25.25),gg=TRUE, rug=FALSE,
       xlab="Day until fledging", ylab="Chance of perching (%)")+
  theme(text = element_text(size = 18))
dev.off()

png("Person_Aue_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hourAue2,xvar="Person", scale="response",
       line=list(col="black", cex=5.25),gg=TRUE, rug=FALSE, xlab="Person/Walker", ylab="Chance of perching (%)")+
  theme(text = element_text(size = 18))
dev.off()

png("Auto_Aue_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hourAue2,xvar="Auto", scale="response",
       line=list(col="black", cex=5.25),gg=TRUE, rug=FALSE, xlab="Car", ylab="Chance of perching (%)")+
  theme(text = element_text(size = 18))
dev.off()

png("Nr.P_Aue_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hourAue3,xvar="Anzahl_P", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="Count of People", ylab="Chance of perching (%)")+
  theme(text = element_text(size = 18))
dev.off()

#visreg(dep_hourAue2,xvar="Fahrrad", scale="response",
   #    line=list(col="black", cex=5.25),gg=TRUE, rug=FALSE, xlab="Bicycle", ylab="Probability of a bird sitting outside the breeding chamber")
#visreg(dep_hourAue2,xvar="Hund", scale="response",
    #   line=list(col="black", cex=5.25),gg=TRUE, rug=FALSE, xlab="Dog", ylab="Probability of a bird sitting outside the breeding chamber")


png("UntilFledging_Pettstadt_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hour_Pettstadt2,xvar="until_fledging", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="Days until fledging", ylab="Chance of perching (%)")+
  theme(text = element_text(size = 16))
dev.off()

png("SUP_Pettstadt_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hour_Pettstadt2,xvar="SUP", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="SUP's per Hour", ylab="Count of minutes perching")+
  theme(text = element_text(size = 18))
  dev.off()


png("Canoe_Pettstadt_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hour_Pettstadt2,xvar="Kanu", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="Canoes per Hour", ylab="Count of minutes perching")+
  theme(text = element_text(size = 18))
dev.off()

png("Fisher_Pettstadt_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hour_Pettstadt2,xvar="Angler", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="Fisher per Hour", ylab="Count of minutes perching")+
  theme(text = element_text(size = 18))
dev.off()

png("Anzahl_P_Pettstadt_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
visreg(dep_hour_Pettstadt3,xvar="Anzahl_P", scale="response",
       line=list(col="black"),gg=TRUE, rug=FALSE, xlab="Count of People per Hour", ylab="Count of minutes perching")+
  theme(text = element_text(size = 18))
dev.off()



png("Stunde+Day until fledging_Bach_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep_hour_Bach1,type="pred",terms=c("Stunde","until_fledging"),line.size = 1.1,title = "",
           axis.title = c("Time","Count of minutes perching"),legend.title = "Days until fledging")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

plot_model(dep_hour_Bach1,type="pred",terms=c("Störungen[all]"),line.size = 1.1,title = "",
           axis.title = c("Disturbance","Pobability of a bird sitting infront of the chamber"),legend.title = "Days until fledging")
visreg(dep_hour_Bach1,xvar = "Stunde", scale="response",ylab="Probability of a bird entering the breeding chamber",xlab="Hour")


#?
summary(dep_hour_Park1<-glm(vor~Störungen+Regen+Stunde+I(Stunde^2)+until_fledging+I(until_fledging^2),data=DF_hour_Park_vor,family=poisson))
plot_model(dep_hour_Park1,)

png("Stunde+Day until fledging_Park_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep_hour_Park1,type="pred", terms=c("Stunde","until_fledging"),line.size = 1.1, title="",
           axis.title = c("Time","Count of minutes perching"),legend.title = "Days until fledging")+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()



png("Hund_Park_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep_hour_Park2,type="pred",terms="Hund",line.size = 1.1,title = "",
           axis.title = c("Dogs per Hour","Count of minutes perching"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

png("Walker_Park_vor_hour.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
plot_model(dep_hour_Park2,type="pred",terms="Person",line.size = 1.1,title = "",
           axis.title = c("Person/Walker per Hour","Count of minutes perching"))+
  font_size( axis_title.x=20, axis_title.y=20, labels.x=20, labels.y=20)
dev.off()

visreg(dep_hour_Park1,xvar = "until_fledging", scale="response",ylab="Probability of a bird entering the breeding chamber",xlab="Day until fledging")


#sum vor

png("VOR's.png", height = 12, width = 16, units = "cm", res = 300)
windowsFonts(FontSourceSans = windowsFont("Source Sans Pro"))
par(mar = c(2.8, 4, 2, 1), lend = 2, cex = 1.25, lwd = 2.75, bty = "l", family = "FontSourceSans")
ggplot(data=vor.all)+
  geom_point(mapping = aes(x = Tag, y = vor_Aue),colour="deeppink3",size=4)+
  geom_point(mapping = aes(x = Tag, y = vor_Bach),colour="yellow",size=4)+
  geom_point(mapping = aes(x = Tag, y = vor_Pettstadt),colour="darkorchid2",size=4)+
  geom_point(mapping = aes(x = Tag, y = vor_Park),colour="blue1",size=4)+
  labs(x = "Day of the year",y="Times of perching")+
  theme(text = element_text(size = 18))
dev.off()

