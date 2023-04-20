setwd("~/Desktop/Research- PhD to Present/Side Projects/Ornate Light/Ornate-Geos")

## Packages & Functions ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(emmeans)
library(suncalc)
library(lubridate)
library(cowplot)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(MuMIn)
library(devtools)
library(ggbiplot)
library(patchwork)
st.err = function(x) {sd(x)/sqrt(length(x))}
path<-"/Users/ianclifton/Desktop/Research- PhD to Present/Side Projects//Figures"

## Data management ----
data.1<-read.csv("ornateboxdataloggers.p1.csv")
data.2<-read.csv("ornateboxdataloggers.p2.csv")
data1<-rbind(data.1,data.2)
# Temp is logged every 30 minutes
# Light level is logged every 5 minutes
# Females- Uno, Cuatro, Cinco, Seis, Seite, Ocho
# Males- Dos, Tres, Nueve, Diez
data1$kLight<-data1$Light/1000 # klx conversion
data1$datetime<-as.POSIXct(strptime(data1$Adjusted.Local.Time, format("%m/%d/%y %H:%M")))

# Daily Light summary dataset
Light.Summary<-aggregate(kLight~Sex+Name+format(datetime, "%Y-%m-%d"), mean, data=data1)
colnames(Light.Summary)<-c("Sex","Name","datetime","Light")
Light.Summary$datetime<-as.POSIXct(strptime(Light.Summary$datetime, format("%Y-%m-%d")))

# Daily temperature summary dataset
# This could be problematic since there will always be 6 identical measurements
Temp.Summary<-aggregate(Temp~Sex+Name+format(datetime, "%Y-%m-%d"), mean, data=data1)
colnames(Temp.Summary)<-c("Sex","Name","datetime","Temp")
Temp.Summary$datetime<-as.POSIXct(strptime(Temp.Summary$datetime, format("%Y-%m-%d")))

EnvData<-read.csv("FruitlandWeather.csv")

TelemData<-read.csv("OrnateBoxTelemetryLocations.csv")

## Exploratory Figures ----
lims<-as.POSIXct(strptime(c("2021-04-18","2022-10-05"), format = "%Y-%m-%d"))

## Individual daily light summary
p.uno.light<-ggplot(data=subset(Light.Summary, Name=="Uno"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  ggtitle("Uno")+
  xlab("Date")
p.dos.light<-ggplot(data=subset(Light.Summary, Name=="Dos"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Dos")+
  xlab("Date")
p.tres.light<-ggplot(data=subset(Light.Summary, Name=="Tres"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  ggtitle("Tres")+
  xlab("Date")
p.cuatro.light<-ggplot(data=subset(Light.Summary, Name=="Cuatro"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Cuatro")+
  xlab("Date")
p.cinco.light<-ggplot(data=subset(Light.Summary, Name=="Cinco"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  ggtitle("Cinco")+
  xlab("Date")
p.seis.light<-ggplot(data=subset(Light.Summary, Name=="Seis"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Seis")+
  xlab("Date")
p.seite.light<-ggplot(data=subset(Light.Summary, Name=="Seite"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  ggtitle("Siete")+
  xlab("Date")
p.ocho.light<-ggplot(data=subset(Light.Summary, Name=="Ocho"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Ocho")+
  xlab("Date")
p.nueve.light<-ggplot(data=subset(Light.Summary, Name=="Nueve"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  ggtitle("Nueve")+
  xlab("Date")
p.diez.light<-ggplot(data=subset(Light.Summary, Name=="Diez"), aes(x=datetime, y=Light))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=28, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Diez")+
  xlab("Date")

# Panel Plot- Individual Light
p.ind.light<-(p.uno.light|p.dos.light)/
  (p.tres.light|p.cuatro.light)/
  (p.cinco.light|p.seis.light)/
  (p.seite.light|p.ocho.light)/
  (p.nueve.light|p.diez.light)

# Individual Light- all 10
p.light<-ggplot(data=Light.Summary, aes(x=datetime, y=Light, group=Name))+
  theme_classic()+
  geom_point(aes(colour=Name), size=3)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=28, face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0,20,2))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Light (klux)")+
  xlab("Date")

## Individual daily temp summary
p.uno.temp<-ggplot(data=subset(Temp.Summary, Name=="Uno"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  ggtitle("Uno")+
  xlab("Date")
p.dos.temp<-ggplot(data=subset(Temp.Summary, Name=="Dos"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Dos")+
  xlab("Date")
p.tres.temp<-ggplot(data=subset(Temp.Summary, Name=="Tres"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  ggtitle("Tres")+
  xlab("Date")
p.cuatro.temp<-ggplot(data=subset(Temp.Summary, Name=="Cuatro"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Cuatro")+
  xlab("Date")
p.cinco.temp<-ggplot(data=subset(Temp.Summary, Name=="Cinco"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  ggtitle("Cinco")+
  xlab("Date")
p.seis.temp<-ggplot(data=subset(Temp.Summary, Name=="Seis"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Seis")+
  xlab("Date")
p.seite.temp<-ggplot(data=subset(Temp.Summary, Name=="Seite"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  ggtitle("Siete")+
  xlab("Date")
p.ocho.temp<-ggplot(data=subset(Temp.Summary, Name=="Ocho"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#FF6FD3")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Ocho")+
  xlab("Date")
p.nueve.temp<-ggplot(data=subset(Temp.Summary, Name=="Nueve"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  ggtitle("Nueve")+
  xlab("Date")
p.diez.temp<-ggplot(data=subset(Temp.Summary, Name=="Diez"), aes(x=datetime, y=Temp))+
  theme_classic()+
  geom_point(size=3, color="#6FDEFF")+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=28, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("")+
  ggtitle("Diez")+
  xlab("Date")

# Panel Plot- Individual Temperature
p.ind.temp<-(p.uno.temp|p.dos.temp)/
  (p.tres.temp|p.cuatro.temp)/
  (p.cinco.temp|p.seis.temp)/
  (p.seite.temp|p.ocho.temp)/
  (p.nueve.temp|p.diez.temp)
 
# Individual Light- all 10
p.temp<-ggplot(data=Temp.Summary, aes(x=datetime, y=Temp, group=Name))+
  theme_classic()+
  geom_point(aes(colour=Name), size=3)+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=26,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = "bottom", plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"),
        plot.title = element_text(hjust = 0.5, size=28, face="bold"),
        legend.text=element_text(size=26, face="bold"), legend.title=element_text(size=30, face="bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,5))+
  scale_x_datetime(date_breaks="2 month", date_labels="%b", limits=lims)+
  ylab("Daily Mean Temperature (°C)")+
  xlab("Date")
