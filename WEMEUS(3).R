### Western Meadowlark United States R Script ###

# Author: Nicholas Faraco-Hadlock

# Date: 02/22/2021

# This is a simple R Script designed to determine population trend of 
# Western Meadowlark at one BBS line transect location in the United States using
# simple linear regression and considering the population trend to be
# the slope.

remove(list=ls()) 

#Set Working Directory
setwd("~/Senior Thesis Work")

## Load in Necessary Packages ##

library(sp)

library(raster)

# Install the rgdal package
#install.packages(c('rgdal'),repos = "http://cran.case.edu", configure.args=c("--with-proj-include=/packages/PROJ/6.1.0/include","--with-proj-lib=/packages/PROJ/6.1.0/lib"))
library(rgdal)

#Instal dplyr
library(dplyr)

## Reading in BBS Data ##

#routes
routes=read.csv('routes.csv',header=T)
#head(routes)

#Read in Data from all 50 States
fifty1=read.csv('fifty1.csv',header=T)
#head(fifty1)

fifty2=read.csv('fifty2.csv',header=T)
#head(fifty2)

fifty3=read.csv('fifty3.csv',header=T)
#head(fifty3)

fifty4=read.csv('fifty4.csv',header=T)
#head(fifty4)

fifty5=read.csv('fifty5.csv',header=T)
#head(fifty5)

fifty6=read.csv('fifty6.csv',header=T)
#head(fifty6)

fifty7=read.csv('fifty7.csv',header=T)
#head(fifty7)

fifty8=read.csv('fifty8.csv',header=T)
#head(fifty8)

fifty9=read.csv('fifty9.csv',header=T)
#head(fifty9)

fifty10=read.csv('fifty10.csv',header=T)
#head(fifty10)

fifty=rbind(fifty1,fifty2,fifty3,fifty4,fifty5,fifty6,fifty7,fifty8,fifty9,fifty10)
#head(fifty)

#merge fifty with routes

USData=merge(routes,fifty)
head(USData)

#Test: Determine the Years for Which a route was surveyed
RouteTest=USData[USData$RouteName=='MAYBELL',]
unique(RouteTest$Year)

#Extract Western Meadowlark Data

# WM=fifty[fifty$AOU==05011,]
# WEMEUS=merge(WM,routes)
WEMEUS=USData[USData$AOU==05011,]
head(WEMEUS)

#Test: Determine the years when WEME were not seen
WEMERouteTest=WEMEUS[WEMEUS$RouteName=='MAYBELL',]
unique(WEMERouteTest$Year)

#Read in Zonal Histogram from QGIS Buffer
histogram=read.csv('US_NLCD_2016_HISTO.csv',header=T)
head(histogram)

#plot population agianst year
plot(WEMEUS$Stop1 ~ WEMEUS$Year)

#generate random point count within each route

#Plot for a single route
WEMEUSRoute8=WEMEUS[WEMEUS$Route==8,]
plot(WEMEUSRoute8$Stop1~WEMEUSRoute8$Year)

#linear regression of single route data
x=lm(formula = WEMEUSRoute8$Stop1~WEMEUSRoute8$Year)
abline(x)
matrix_coef <- summary(x)$coefficients
matrix_coef
summary(lm(formula = WEMEUSRoute8$Stop1~WEMEUSRoute8$Year))$coefficients[2]

WEMEUSRoute=WEMEUS[WEMEUS$Route==WEMEUS$Route[1],]
WEMEUSRoute

#linear regression for all point count data

WEMEallroutes=matrix(ncol=2,byrow=TRUE)

for (i in WEMEUS$Route){
  
  WEMERoute=WEMEUS[WEMEUS$Route==WEMEUS$Route[i],]
  A=WEMEUS$Route[i]
  B=summary(lm(formula = WEMERoute$Stop1~WEMERoute$Year))$coefficients[2]
  WEMEallroutes=rbind(WEMEallroutes, c(A,B))
  
}
head(WEMEallroutes)

#Plotting population trends
plot(WEMEallroutes)
summary(WEMEallroutes)

#linear regression for all point count data for Stop1 with Long and Lat
WMS1=matrix(ncol=7,byrow=TRUE)
dfWMS1=as.data.frame(WMS1)
TrashMatrix=matrix()

for (i in 1:nrow(WEMEUS)){
  if (identical(which(TrashMatrix==WEMEUS$RouteName[i]),integer(0))){
    if (setequal(unique(USData[USData$RouteName==WEMEUS$RouteName[i],]$Year),unique(WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[i],]$Year))){
      dfWMS1=rbind(dfWMS1, matrix(ncol=7,byrow=TRUE))
      #wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
      A=WEMEUS$RouteName[i]
      S=WEMEUS$StateNum[i]
      wmiRoute=WEMEUS[WEMEUS$RouteName==A,]
      L=WEMEUS$RouteDataID[i]
      Y=WEMEUS$Stratum[i]
      B=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
      C=WEMEUS$Longitude[i]
      D=WEMEUS$Latitude[i]
      dfWMS1[i,1]=A
      dfWMS1[i,2]=L
      dfWMS1[i,3]=S
      dfWMS1[i,4]=Y
      dfWMS1[i,5]=B
      dfWMS1[i,6]=C
      dfWMS1[i,7]=D
      TrashMatrix=rbind(TrashMatrix,matrix(A))
      
      #WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))      
    }
    else{
      dfWMS1=rbind(dfWMS1, matrix(ncol=7,byrow=TRUE))
      #wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
      A=WEMEUS$RouteName[i]
      S=WEMEUS$StateNum[i]
      ZeroYears=unique(USData[USData$RouteName==WEMEUS$RouteName[1],]$Year)[!(unique(USData[USData$RouteName==WEMEUS$RouteName[1],]$Year) %in% unique(WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[1],]$Year))]
      wmiRoute=WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[1],]
      wmiR2=cbind(matrix(nrow=nrow(wmiRoute)),wmiRoute$Year,wmiRoute$Stop1)
      for (i in 1:length(ZeroYears)){
        wmiR2=rbind(wmiR2,c(NA,ZeroYears[i],0))
      }
      L=WEMEUS$RouteDataID[i]
      Y=WEMEUS$Stratum[i]
      B=summary(lm(formula = wmiR2[,3]~wmiR2[,2]))$coefficients[2]
      C=WEMEUS$Longitude[i]
      D=WEMEUS$Latitude[i]
      dfWMS1[i,1]=A
      dfWMS1[i,2]=L
      dfWMS1[i,3]=S
      dfWMS1[i,4]=Y
      dfWMS1[i,5]=B
      dfWMS1[i,6]=C
      dfWMS1[i,7]=D
      TrashMatrix=rbind(TrashMatrix,matrix(A))
      
      #WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))        
    }
  }
}

head(dfWMS1)
#dfWMS1=as.data.frame(WMS1)
names(dfWMS1) = c("Route","RouteDataID","StateNum","Strata","PopTrend","Longitude","Latitude")
unique(dfWMS1$PopTrend)
dfWMS1=na.omit(dfWMS1)
nrow(dfWMS1)
head(dfWMS1)

#Merge histogram with population trend data
newhistogram=matrix(ncol=3,byrow=TRUE)
dfnewhistogram=as.data.frame(newhistogram)

for (i in 1:nrow(histogram)){
  dfnewhistogram=rbind(dfnewhistogram, matrix(ncol=3,byrow=TRUE))
  b=histogram$Longitude[i]
  c=histogram$Latitude[i]
  propag=(histogram$WEME_HISTO_2016_US82[i])/(histogram$WEME_HISTO_2016_US0[i]+histogram$WEME_HISTO_2016_US11[i]+histogram$WEME_HISTO_2016_US21[i]+histogram$WEME_HISTO_2016_US22[i]+histogram$WEME_HISTO_2016_US23[i]+histogram$WEME_HISTO_2016_US24[i]+histogram$WEME_HISTO_2016_US31[i]+histogram$WEME_HISTO_2016_US41[i]+histogram$WEME_HISTO_2016_US42[i]+histogram$WEME_HISTO_2016_US43[i]+histogram$WEME_HISTO_2016_US52[i]+histogram$WEME_HISTO_2016_US71[i]+histogram$WEME_HISTO_2016_US81[i]+histogram$WEME_HISTO_2016_US82[i]+histogram$WEME_HISTO_2016_US90[i]+histogram$WEME_HISTO_2016_US95[i])
  dfnewhistogram[i,1]=propag
  dfnewhistogram[i,2]=b
  dfnewhistogram[i,3]=c 
}
head(dfnewhistogram)
unique(dfnewhistogram$V1)
names(dfnewhistogram)=c("PropAg","Longitude","Latitude")
dfnewhistogram=na.omit(dfnewhistogram)
head(dfnewhistogram)
dfnewhistogram=distinct(dfnewhistogram)
PTLC = merge(dfnewhistogram,dfWMS1)
PTLC=na.omit(PTLC)
head(PTLC)
unique(PTLC$PropAg)
plot(PTLC$PopTrend~PTLC$PropAg)
x=lm(formula = PTLC$PopTrend~PTLC$PropAg)
abline(x)
abline(a=0,b=0)
summary(x)










#Plot Population Trend as a function of Latitude
WEMEAFLat=matrix(ncol=3,byrow=TRUE)
for (i in WEMEUS$Route){
  
  WEME2Route=WEMEUS[WEMEUS$Route==WEMEUS$Route[i],]
  A=WEMEUS$Route[i]
  B=summary(lm(formula = WEME2Route$Stop1~WEME2Route$Year))$coefficients[2]
  C=WEMEUS$Latitude[i]
  WEMEAFLat=rbind(WEMEAFLat, c(A,B,C))
  
}
head(WEMEAFLat)
plot(WEMEAFLat[,2]~WEMEAFLat[,3])
x=lm(formula = WEMEAFLat[,2]~WEMEAFLat[,3])
abline(x)

#Plot Population Trend as a function of Longitude
WEMEAFLong=matrix(ncol=3,byrow=TRUE)
for (i in WEMEUS$Route){
  
  WEME2Route=WEMEUS[WEMEUS$Route==WEMEUS$Route[i],]
  A=WEMEUS$Route[i]
  B=summary(lm(formula = WEME2Route$Stop1~WEME2Route$Year))$coefficients[2]
  C=WEMEUS$Longitude[i]
  WEMEAFLong=rbind(WEMEAFLong, c(A,B,C))
  
}
head(WEMEAFLong)
plot(WEMEAFLong[,2]~WEMEAFLong[,3])
x=lm(formula = WEMEAFLong[,2]~WEMEAFLong[,3])
abline(x)

#Check Ranges
range(WEMEAFLong[,3],na.rm=TRUE)
range(WEMEUS$Longitude,na.rm=TRUE)

range(WEMEAFLat[,3],na.rm=TRUE)
range(WEMEUS$Latitude,na.rm=TRUE)

range(WEMEallroutes[,2], na.rm=TRUE)
range(WEMEAFLat[,2], na.rm=TRUE)







#Discarded Code


# wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[100],]
# A=wmi$Route[100]
# S=wmi$StateNum[100]
# wmiRoute=wmi[wmi$Route==A,]
# L=wmi$RouteDataID[100]
# Y=wmi$Stratum[100]
# B=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
# C=wmi$Longitude[100]
# D=wmi$Latitude[100]
# WMS1=rbind(WMS1, c(A,L,Y,B,C,D))

# for (i in 1:nrow(WEMEUS)){
#   A=wmi$RouteName[i]
#   S=wmi$StateNum[i]
#   wmiRoute=wmi[wmi$RouteName==A,]
#   L=wmi$RouteDataID[i]
#   Y=wmi$Stratum[i]
#   B=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
#   C=wmi$Longitude[i]
#   D=wmi$Latitude[i]
#   WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))
# }
# 
# for (i in 1:nrow(WEMEUS)){
#   wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
#   A=wmi$Route[i]
#   S=wmi$StateNum[i]
#   wmiRoute=wmi[wmi$Route==A,]
#   L=wmi$RouteDataID[i]
#   Y=wmi$Stratum[i]
#   B=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
#   C=wmi$Longitude[i]
#   D=wmi$Latitude[i]
#   WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))
# }

#wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
# A=WEMEUS$RouteName[4]
# S=WEMEUS$StateNum[4]
# wmiRoute=WEMEUS[WEMEUS$RouteName==A,]
# L=WEMEUS$RouteDataID[4]
# Y=WEMEUS$Stratum[4]
# B=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
# C=WEMEUS$Longitude[4]
# D=WEMEUS$Latitude[4]
# 
# class(D)
# class(WMS1[2,7])

#setequal(unique(USData[USData$RouteName==WEMEUS$RouteName[1],]$Year),unique(WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[1],]$Year))
