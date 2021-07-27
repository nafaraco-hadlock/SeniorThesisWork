### Western Meadowlark United States R Script ###

# Author: Nicholas Faraco-Hadlock

# Date: 02/22/2021

# This is a simple R Script designed to determine population trend of 
# Western Meadowlark at one BBS line transect location in the United States using
# simple linear regression and considering the population trend to be
# the slope.

remove(list=ls()) 

#Set Working Directory
setwd("//rschfs1x/userrs/K-Q/naf42_RS/Desktop/Senior Thesis Work")

## Load in Necessary Packages ##

library(sp)

library(raster)

# Install the rgdal package
#install.packages(c('rgdal'),repos = "http://cran.case.edu", configure.args=c("--with-proj-include=/packages/PROJ/6.1.0/include","--with-proj-lib=/packages/PROJ/6.1.0/lib"))
library(rgdal)

#Install dplyr
library(dplyr)

#Install lme
library(lme4)
library(nlme)

#Instat MuMIn
library(MuMIn)

#Install tidyverse
library(tidyverse)

## Reading in BBS Data ##

#Read in weather.csv to get observer #
weather=read.csv('weather.csv',header=T)
head(weather)

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

#merge fifty with routes and weather

USData=merge(routes,fifty)
USData=merge(USData,weather)
head(USData)

#Test: Determine the Years for Which a route was surveyed
RouteTest=USData[USData$RouteName=='MAYBELL',]
unique(RouteTest$Year)

#Extract Western Meadowlark Data
WEMEUS=USData[USData$AOU==05011,]
head(WEMEUS)
# WM=fifty[fifty$AOU==05011,]
# WEMEUS=merge(WM,routes)

#Remove Years Before 2001 and after 2016 (Those Included in NLCD Land Change Index)
WEMEUS=WEMEUS[WEMEUS$Year > 2000,]
WEMEUS=WEMEUS[WEMEUS$Year < 2017,]

#Test: Determine the years when WEME were not seen
WEMERouteTest=WEMEUS[WEMEUS$RouteName=='MAYBELL',]
unique(WEMERouteTest$Year)

#Read in Zonal Histogram from QGIS Buffer: Radius 263.3 feet (5 acres)
histogram=read.csv('US_NLCD_2016_HISTO.csv',header=T)
head(histogram)

#Read in Plant Hardiness Zone Data
PHZroutes=read.csv('PHZroutes.csv',header=T)
head(PHZroutes)

#Read in Land Cover Change Data
LandChangeHisto=read.csv('Land_Change_Histo.csv',header=T)
head(LandChangeHisto)

##End of Startup Code##

#plot population agianst year
plot(WEMEUS$Stop1 ~ WEMEUS$Year)

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
WMS1=matrix(ncol=8,byrow=TRUE)
dfWMS1=as.data.frame(WMS1)
TrashMatrix=matrix()
j=1

for (i in 1:nrow(WEMEUS)){
  if (identical(which(TrashMatrix==WEMEUS$RouteName[i]),integer(0))){
    if (setequal(unique(USData[USData$RouteName==WEMEUS$RouteName[i],]$Year),unique(WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[i],]$Year))){
      #wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
      RouteName=WEMEUS$RouteName[i]
      State=WEMEUS$StateNum[i]
      wmiRoute=WEMEUS[WEMEUS$RouteName==RouteName,]
      if (nrow(wmiRoute)>9 & colSums(wmiRoute != 0)[16]>3){
        dfWMS1=rbind(dfWMS1, matrix(ncol=8,byrow=TRUE))
        RouteDataID=WEMEUS$RouteDataID[i]
        Stratum=WEMEUS$Stratum[i]
        PopTrend=summary(lm(formula = wmiRoute$Stop1~wmiRoute$Year))$coefficients[2]
        Long=WEMEUS$Longitude[i]
        Lat=WEMEUS$Latitude[i]
        ObsN=WEMEUS$ObsN[i]
        dfWMS1[j,1]=RouteName
        dfWMS1[j,2]=RouteDataID
        dfWMS1[j,3]=State
        dfWMS1[j,4]=Stratum
        dfWMS1[j,5]=PopTrend
        dfWMS1[j,6]=Long
        dfWMS1[j,7]=Lat
        dfWMS1[j,8]=ObsN
        j=j+1
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }else{
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      #WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))      
    }else{
      #wmi=WEMEUS[WEMEUS$StateNum==WEMEUS$StateNum[i],]
      RouteName=WEMEUS$RouteName[i]
      State=WEMEUS$StateNum[i]
      ZeroYears=unique(USData[USData$RouteName==WEMEUS$RouteName[i],]$Year)[!(unique(USData[USData$RouteName==WEMEUS$RouteName[i],]$Year) %in% unique(WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[i],]$Year))]
      wmiRoute=WEMEUS[WEMEUS$RouteName==WEMEUS$RouteName[i],]
      wmiR2=cbind(matrix(nrow=nrow(wmiRoute)),wmiRoute$Year,wmiRoute$Stop1)
      for (n in 1:length(ZeroYears)){
        wmiR2=rbind(wmiR2,c(NA,ZeroYears[n],0))
      }
      if (nrow(wmiR2)>9 & colSums(wmiR2 != 0)[3]>3){
        dfWMS1=rbind(dfWMS1, matrix(ncol=8,byrow=TRUE))
        RouteDataID=WEMEUS$RouteDataID[i]
        Stratum=WEMEUS$Stratum[i]
        PopTrend=summary(lm(formula = wmiR2[,3]~wmiR2[,2]))$coefficients[2]
        Long=WEMEUS$Longitude[i]
        Lat=WEMEUS$Latitude[i]
        ObsN=WEMEUS$ObsN[i]
        dfWMS1[j,1]=RouteName
        dfWMS1[j,2]=RouteDataID
        dfWMS1[j,3]=State
        dfWMS1[j,4]=Stratum
        dfWMS1[j,5]=PopTrend
        dfWMS1[j,6]=Long
        dfWMS1[j,7]=Lat
        dfWMS1[j,8]=ObsN
        j=j+1
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }else{
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      
      
      #WMS1=rbind(WMS1, c(A,L,S,Y,B,C,D))        
    }
  }
}

head(dfWMS1)
#dfWMS1=as.data.frame(WMS1)
names(dfWMS1) = c("Route","RouteDataID","StateNum","Strata","PopTrend","Longitude","Latitude","Observer_Number")
unique(dfWMS1$PopTrend)
dfWMS1=na.omit(dfWMS1)
nrow(dfWMS1)
head(dfWMS1)
write.csv(dfWMS1,"WMS1.csv",FALSE)

### Merge histogram with population trend data ###

#Add Land cover, Land change and Plant Hardiness Zones data
newhistogram=matrix(ncol=3,byrow=TRUE)
dfnewhistogram=as.data.frame(newhistogram)
newPHZroutes=matrix(ncol=4,byrow=TRUE)
dfnewPHZroutes=as.data.frame(newPHZroutes)
newLandChangeHisto=matrix(ncol=3,byrow=TRUE)
dfnewLandChangeHisto=as.data.frame(newLandChangeHisto)
for (i in 1:nrow(histogram)){
  dfnewhistogram=rbind(dfnewhistogram, matrix(ncol=3,byrow=TRUE))
  b=histogram$Longitude[i]
  c=histogram$Latitude[i]
  propag=(histogram$WEME_HISTO_2016_US82[i])/(histogram$WEME_HISTO_2016_US0[i]+histogram$WEME_HISTO_2016_US11[i]+histogram$WEME_HISTO_2016_US21[i]+histogram$WEME_HISTO_2016_US22[i]+histogram$WEME_HISTO_2016_US23[i]+histogram$WEME_HISTO_2016_US24[i]+histogram$WEME_HISTO_2016_US31[i]+histogram$WEME_HISTO_2016_US41[i]+histogram$WEME_HISTO_2016_US42[i]+histogram$WEME_HISTO_2016_US43[i]+histogram$WEME_HISTO_2016_US52[i]+histogram$WEME_HISTO_2016_US71[i]+histogram$WEME_HISTO_2016_US81[i]+histogram$WEME_HISTO_2016_US82[i]+histogram$WEME_HISTO_2016_US90[i]+histogram$WEME_HISTO_2016_US95[i])
  dfnewhistogram[i,1]=propag
  dfnewhistogram[i,2]=b
  dfnewhistogram[i,3]=c 
}
for (i in 1:nrow(PHZroutes)){
  dfnewPHZroutes=rbind(dfnewPHZroutes, matrix(ncol=4,byrow=TRUE))
  b=PHZroutes$Longitude[i]
  c=PHZroutes$Latitude[i]
  PHZ=PHZroutes$rvalue_1[i]
  PHZc=PHZroutes$temp_c[i]
  dfnewPHZroutes[i,1]=PHZ
  dfnewPHZroutes[i,2]=PHZc
  dfnewPHZroutes[i,3]=b
  dfnewPHZroutes[i,4]=c 
}
for (i in 1:nrow(LandChangeHisto)){
  dfnewLandChangeHisto=rbind(dfnewLandChangeHisto, matrix(ncol=3,byrow=TRUE))
  b=LandChangeHisto$Longitude[i]
  c=LandChangeHisto$Latitude[i]
  propstatic=(LandChangeHisto$HISTO_Land_Change1[i]/sum(LandChangeHisto$HISTO_Land_Change0[i],LandChangeHisto$HISTO_Land_Change1[i],LandChangeHisto$HISTO_Land_Change2[i],LandChangeHisto$HISTO_Land_Change3[i],LandChangeHisto$HISTO_Land_Change4[i],LandChangeHisto$HISTO_Land_Change5[i],LandChangeHisto$HISTO_Land_Change6[i],LandChangeHisto$HISTO_Land_Change7[i],LandChangeHisto$HISTO_Land_Change8[i],LandChangeHisto$HISTO_Land_Change9[i],LandChangeHisto$HISTO_Land_Change10[i],LandChangeHisto$HISTO_Land_Change11[i]))
  dfnewLandChangeHisto[i,1]=propstatic
  dfnewLandChangeHisto[i,2]=b
  dfnewLandChangeHisto[i,3]=c
}
head(dfnewhistogram)
unique(dfnewhistogram$V1)
names(dfnewhistogram)=c("PropAg","Longitude","Latitude")
names(dfnewPHZroutes)=c("PHZ","PHZc","Longitude","Latitude")
names(dfnewLandChangeHisto)=c("PropStatic","Longitude","Latitude")
dfnewhistogram=na.omit(dfnewhistogram)
dfnewLandChangeHisto=na.omit(dfnewLandChangeHisto)
dfnewPHZroutes=na.omit(dfnewPHZroutes)
head(dfnewhistogram)
dfnewhistogram=distinct(dfnewhistogram)
dfnewLandChangeHisto=distinct(dfnewLandChangeHisto)
dfnewPHZroutes=distinct(dfnewPHZroutes)
head(dfnewhistogram)

PTLCWM = merge(dfnewhistogram,dfWMS1)
PTLCWM = PTLCWM[PTLCWM$PropAg!=0,]
PTLCWM = merge(PTLCWM,dfnewPHZroutes)
PTLCWM = merge(PTLCWM,dfnewLandChangeHisto)
PTLCWM = na.omit(PTLCWM)
PTLCWM = distinct(PTLCWM)

#Remove Point Counts with over 10% Land Cover Change Since 2001 
PTLCWM = PTLCWM[PTLCWM$PropStatic>0.9,]
head(PTLCWM)
unique(PTLCWM$PropAg)
plot(PTLCWM$PopTrend~PTLCWM$PropAg)
ggplot(data=PTLCWM,mapping=aes(PropAg,PopTrend))
plot(PTLCWM$PopTrend~PTLCWM$Longitude)
plot(PTLCWM$PopTrend~PTLCWM$Latitude)
x=lm(formula = PTLCWM$PopTrend~PTLCWM$PropAg)
y=lm(formula = PTLCWM$PopTrend~PTLCWM$Longitude)
z=lm(formula = PTLCWM$PopTrend~PTLCWM$Latitude)
abline(x)
abline(y)
abline(z)
abline(a=0,b=0)
summary(x)
dfPTLCWM=as.data.frame(PTLCWM)
write.csv(dfPTLCWM,"PTLCWM.csv",FALSE)
dfPTLCWM=read.csv('PTLCWM.csv',header=T)

##Model Suite##

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
# lme1 <- lme(PopTrend ~ 1, random=list(~1|Observer_Number), data=dfPTLCWM)
lm1 <- lm(PopTrend ~ 1, data=dfPTLCWM)
# #Population trend varies with relation to proportion agriculture
# lme2 <- lme(PopTrend ~ PropAg, random=list(~1|Observer_Number), data=dfPTLCWM)
lm2 <- lm(PopTrend ~ PropAg, data=dfPTLCWM)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
# lme3 <- lme(PopTrend ~ PropAg + Longitude, random=list(~1|Observer_Number), data=dfPTLCWM)
lm3 <- lm(PopTrend ~ PropAg + Longitude, data=dfPTLCWM)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
# lme4 <- lme(PopTrend ~ PropAg + Latitude, random=list(~1|Observer_Number), data = dfPTLCWM)
lm4 <- lm(PopTrend ~ PropAg + Latitude, data = dfPTLCWM)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
# lme5 <- lme(PopTrend ~ PropAg + Longitude + Latitude, random=list(~1|Observer_Number), data=dfPTLCWM)
lm5 <- lm(PopTrend ~ PropAg + Longitude + Latitude, data=dfPTLCWM)
# #Population trend varies with relation to Plant Hardiness Zone
# lme6 <- lme(PopTrend ~ PHZ, random=list(~1|Observer_Number), data=dfPTLCWM)
lm6 <- lm(PopTrend ~ PHZ, data=dfPTLCWM)

##AIC##
model.sel (lm1, lm2, lm3, lm4, lm5, lm6)
summary(lme1)
summary(lme2)


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


# #Rasterize Plant Hardiness Zone
# 
# PHZ= readOGR(dsn=path.expand('\\rschfs1x\ userrs\ K-Q\ naf42_RS\ Desktop\ Senior Thesis Work'),layer='phm_us_shp.shp')
# s <- shapefile("\\rschfs1x\ userrs\ K-Q\ naf42_RS\ Desktop\ Senior Thesis Work\ phm_us_shp.shp")
# shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
#                        proj.to = NA, map = TRUE) {
#   require(raster, rgdal)
#   
#   # use transform==TRUE if the polygon is not in the same coordinate system as
#   # the output raster, setting proj.from & proj.to to the appropriate
#   # projections
#   if (transform == TRUE) {
#     proj4string(shp) <- proj.from
#     shp <- spTransform(shp, proj.to)
#   }
#   
#   # convert the shapefile to a raster based on a standardised background
#   # raster
#   r <- rasterize(shp, mask.raster)
#   # set the cells associated with the shapfile to the specified value
#   r[!is.na(r)] <- value
#   # merge the new raster with the mask raster and export to the working
#   # directory as a tif file
#   r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
#             overwrite = T)
#   
#   # plot map of new raster
#   if (map == TRUE) {
#     plot(r, main = label, axes = F, box = F)
#   }
#   
#   names(r) <- label
#   return(r)
# }
# RasterPHZ = shp2raster('phm_us_shp.shp',raster(xmn=-125,xmx=-66,ymn=23,ymx=50),transform=TRUE, map=TRUE, proj.from="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs ",proj.to="+proj=longlat +datum=WGS84 +no_defs ")
# 
# ?readOGR