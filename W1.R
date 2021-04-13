### Western Meadowlark Colorado R Script ###

# Author: Nicholas Faraco-Hadlock

# Date: 11/13/2020

# This is a simple R Script designed to determine population trend of 
# Western Meadowlark at one BBS line transect location in Colorado using
# simple linear regression and considering the population trend to be
# the slope.

# I will then generalize this code and apply it ot multiple line 
# transects

# Once I have done that I will determine a correlation between 
# population trend and latititude and longitude to bring in basic 
# spatial data

remove(list=ls()) 

#Set Working Directory
setwd("C:/Users/Nicholas/Desktop/Senior Thesis Work/")

## Load in Necessary Packages ##

library(sp)

library(raster)

# 5. Install the rgdal package
#install.packages(c('rgdal'),repos = "http://cran.case.edu", configure.args=c("--with-proj-include=/packages/PROJ/6.1.0/include","--with-proj-lib=/packages/PROJ/6.1.0/lib"))

library(rgdal)

## Reading in BBS Data ##

#routes
routes=read.csv('routes.csv',header=T)
head(routes)

#first data set from 50 states data
fifty2=read.csv('fifty2.csv',header=T)
head(fifty2)

#Colorado Data
colorado=read.csv('Colorad.csv',header=T)
head(colorado)

#merge Colorado and fifty 2 data
colorado2=merge(colorado,fifty2)
head(colorado2)

#merge colorado2 and route data
colorado3=merge(colorado2,routes)
head(colorado3)

#Extract Western Meadowlark Data
wm=colorado2[colorado2$AOU==05011,]
head(wm)

## Extract Geospatial Data ##

#Read in Zonal Histogram from QGIS Buffer
histogram=read.csv('CO_HISTO_2004_NLCD2.csv',header=T)
head(histogram)
names(histogram)=c("Latitude","Longitude","PropAg")



#plot population agianst year
plot(wm$StopTotal ~ wm$Year)


#Plot for a single route
wmRoute8=wm[wm$Route==8,]
plot(wmRoute8$StopTotal~wmRoute8$Year)

#Plot for a single route and Stop1
wmRoute8=wm[wm$Route==8,]
plot(wmRoute8$Stop1~wmRoute8$Year)

#linear regression of single route data
x=lm(formula = wmRoute8$StopTotal~wmRoute8$Year)
abline(x)
matrix_coef <- summary(x)$coefficients
matrix_coef
summary(lm(formula = wmRoute8$StopTotal~wmRoute8$Year))$coefficients[2]

wmRoute=wm[wm$Route==wm$Route[1],]
wmRoute

#linear regression for all point count data

WMallroutes=matrix(ncol=2,byrow=TRUE)

for (i in wm$Route){
  
  wmRoute=wm[wm$Route==wm$Route[i],]
  A=wm$Route[i]
  B=summary(lm(formula = wmRoute$StopTotal~wmRoute$Year))$coefficients[2]
  WMallroutes=rbind(WMallroutes, c(A,B))
  
}
head(WMallroutes)


#Plotting population trends
plot(WMallroutes)
summary(WMallroutes)

#Plot Population Trend as a function of Latitude, Probably loosing Data
WMAFLat=matrix(ncol=3,byrow=TRUE)
wm2=colorado3[colorado3$AOU==05011,]
for (i in wm$Route){
  
  wm2Route=wm2[wm2$Route==wm2$Route[i],]
  A=wm2$Route[i]
  B=summary(lm(formula = wm2Route$StopTotal~wm2Route$Year))$coefficients[2]
  C=wm2$Latitude[i]
  WMAFLat=rbind(WMAFLat, c(A,B,C))
  
}
head(WMAFLat)
plot(WMAFLat[,2]~WMAFLat[,3])
x=lm(formula = WMAFLat[,2]~WMAFLat[,3])
abline(x)

#Plot Population Trend as a function of Longitude, Probably loosing data
WMAFLong=matrix(ncol=3,byrow=TRUE)
wm2=colorado3[colorado3$AOU==05011,]
for (i in wm2$Route){
  
  wm2Route=wm2[wm2$Route==wm2$Route[i],]
  A=wm2$Route[i]
  B=summary(lm(formula = wm2Route$StopTotal~wm2Route$Year))$coefficients[2]
  C=wm2$Longitude[i]
  WMAFLong=rbind(WMAFLong, c(A,B,C))
  
}

head(WMAFLong)
plot(WMAFLong[,2]~WMAFLong[,3])
x=lm(formula = WMAFLong[,2]~WMAFLong[,3])
abline(x)

#linear regression for all point count data for Stop1 with Long and Lat

WMS1=matrix(ncol=6,byrow=TRUE)
wm2=colorado3[colorado3$AOU==05011,]
head(wm2)
unique(wm2$Longitude)
summary(wm2)


for (i in 1:nrow(wm2)){
  
  A=wm2$Route[i]
  wm2Route=wm2[wm2$Route==A,]
  L=wm2$RouteDataID[i]
  Y=wm2$Stratum[i]
  B=summary(lm(formula = wm2Route$Stop1~wm2Route$Year))$coefficients[2]
  C=wm2$Longitude[i]
  D=wm2$Latitude[i]
  WMS1=rbind(WMS1, c(A,L,Y,B,C,D))
  
}

head(WMS1)
dfWMS1=as.data.frame(WMS1)
names(dfWMS1) = c("Route","RouteDataID","Strata","PopTrend","Longitude","Latitude")
unique(dfWMS1$PopTrend)
dfWMS1=na.omit(dfWMS1)
nrow(dfWMS1)
unique(dfWMS1$Longitude)
head(dfWMS1)

#Merge histogram with population trend data
PTLC = merge(histogram,dfWMS1)
PTLC=na.omit(PTLC)
head(PTLC)
range(PTLC$PropAg)
plot(PTLC$PopTrend~PTLC$PropAg)
x=lm(formula = PTLC$PopTrend~PTLC$PropAg)
abline(x)
summary(x)


unique(histogram$Longitude)
unique(dfWMS1$Longitude)
unique(PTLC$Longitude)

#Save data as CSVs

dfWMallroutes=as.data.frame(WMallroutes)
dfWMAFLat=as.data.frame(WMAFLat)
dfWMAFLong=as.data.frame(WMAFLong)

write.csv(dfWMAFLong,"WMAFLong.csv",FALSE)
write.csv(dfWMAFLat,"WMAFLat.csv",FALSE)
write.csv(dfWMallroutes,"WMallroutes.csv",FALSE)
write.csv(dfWMS1, "WMS1.csv", FALSE)
write.csv(wm2, "wm2.csv", FALSE)
write.csv(PTLC, "PTLC.csv", FALSE)

# Run Mixed Models Controlling for Observer and Stratum

StrataRandom = PTLC$PopTrend~PTLC$PropAg+(1+PTLC$Strata)
plot(StrataRandom)

# Colleen Code

x=lm(formula = PTLC$PopTrend~PTLC$PropAg)
x.1 <- lmer(PopTrend~PropAg + (1|Route), data=PTLC)
firstmodel <- lmer(PopTrend~ 1 + Randoms)
second <- lmer(PopTrend ~ PropAg + Randoms)
atsomepoint <- lmer(PopTrend ~ PropAg*Latitude/Longitude + (1|Route) + (1|Obs), data=PTLC)

now <- lm(PopTrend ~ PropAg*Longitude, data=PTLC)
summary(now)
plot(PopTrend ~ PropAg, col=alpha('cornflowerblue', 0.2), data=PTLC, pch=19, cex=4)

library(ggplot2)
library(sjPlot)

plot_model(now, type='int')

















#Discarded Code

#wmRoute=wm[wm$Route==wm$Route[i],]
#plot(wmRoute$StopTotal~wramRoute$Year)
#WMallroutes[wmRoute]=summary(lm(formula = wmRoute$StopTotal~wmRoute$Year))$coefficients[2]
#WMallroutes[i]=wm$Route[i]
#WMallroutes[i]=lm(formula = wmRoute$StopTotal~wmRoute$Year)
#abline(lm(formula = wmRoute$StopTotal~wmRoute$Year))

# for (i in wm$Route) {
#   RC=rbind(c(NA,NA), RC)
#   RC[length(RC)]=FALSE
#   RC[length(RC)-1]=wm$Route[i]
#}
#wm$Route[1]
#wmRoute=wm[wm$Route==wm$Route[1],]




#Read in LCMAP data as csv

#LCMAP=read.csv('LCMAP_CU_20200414_V01_REF.csv',header=TRUE)
#head(LCMAP)

# #Read in LCMAP data as TIFF Files
# 
# imported_raster_LCMAP=raster('LandCoverData1.tiff')
# rasterpoints=rasterToPoints(imported_raster_LCMAP)
# head(rasterpoints)
# 
# #Read in NLCD data as TIFF Files
# 
# imported_raster_NLCD=raster('NLCD_2001_Land_Cover_L48_20190424_cCMgnVZQ9PVThNu5c8JC.tiff')
# rasterpoints=rasterToPoints(imported_raster_NLCD)
# head(rasterpoints)
# 
# #Convert UTM to Longitude and Latitude
# #Code From https://www.ecoblender.org/utm-to-longitude-latitude-code/
# 
# utms <- SpatialPoints(data[, c("x", "y")], proj4string=CRS("+proj=utm +zone=10")) #create UTM matrix
# 
# longlats <- spTransform(utms, CRS("+proj=longlat")) #transform

# unique(wm2$Longitude)
# nrow(wm2)
# wm2$Route[1]
# class(as.numeric(wm2[33,3]))

# A=wm2$Route[30]
# A
# wm2Route=wm2[wm2$Route==A,]
# wm2Route
# B=summary(lm(formula = wm2Route$Stop1~wm2Route$Year))$coefficients[2]
# B
# C=wm2$Longitude[30]
# C
# D=wm2$Latitude[30]
# D
# WMS1=rbind(WMS1, c(A,B,C,D))
# WMS1

# for (i in wm2$Route){
# 
#   wm2Route=wm2[wm2$Route==wm2$Route[i],]
#   A=wm2$Route[i]
#   B=summary(lm(formula = wm2Route$Stop1~wm2Route$Year))$coefficients[2]
#   C=wm2$Longitude[i]
#   D=wm2$Latitude[i]
#   WMS1=rbind(WMS1, c(A,B,C,D))
# 
# }