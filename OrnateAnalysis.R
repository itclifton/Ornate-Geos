setwd("~/Desktop/Research- PhD to Present/Side Projects/Ornate Light/Ornate-Geos")

## Packages ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
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
st.err = function(x) {sd(x)/sqrt(length(x))}
path<-"/Users/ianclifton/Desktop/Research- PhD to Present/Side Projects//Figures"

# Data management ----
data.1<-read.csv("ornateboxdataloggers.p1.csv")
data.2<-read.csv("ornateboxdataloggers.p2.csv")
data1<-rbind(data.1,data.2)

EnvData<-read.csv("FruitlandWeather.csv")

TelemData<-read.csv("OrnateBoxTelemetryLocations.csv")

# Exploratory analyses ----

