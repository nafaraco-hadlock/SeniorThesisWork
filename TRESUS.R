### Western Meadowlark United States R Script ###

# Author: Nicholas Faraco-Hadlock

# Date: 05/04/2021

# This is a simple R Script designed to determine population trend of 
# Tree Swallows at one BBS line transect location in the United States using
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

#merge fifty with routes

USData=merge(routes,fifty)
USData=merge(USData,weather)
head(USData)

#Test: Determine the Years for Which a route was surveyed
RouteTest=USData[USData$RouteName=='MAYBELL',]
unique(RouteTest$Year)

#Extract Western Meadowlark Data
TRESUS=USData[USData$AOU==06140,]
head(TRESUS)
# TS=fifty[fifty$AOU==05011,]
# TRESUS=merge(TS,routes)


#Test: Determine the years when TRES were not seen
TRESRouteTest=TRESUS[TRESUS$RouteName=='MAYBELL',]
unique(TRESRouteTest$Year)

#Read in Zonal Histogram from QGIS Buffer
histogram=read.csv('US_NLCD_2016_HISTO.csv',header=T)
head(histogram)

##End of Startup Code##

#plot population agianst year
plot(TRESUS$Stop1 ~ TRESUS$Year)


#generate random point count within each route

#Plot for a single route
TRESUSRoute8=TRESUS[TRESUS$Route==8,]
plot(TRESUSRoute8$Stop1~TRESUSRoute8$Year)

#linear regression of single route data
x=lm(formula = TRESUSRoute8$Stop1~TRESUSRoute8$Year)
abline(x)
matrix_coef <- summary(x)$coefficients
matrix_coef
summary(lm(formula = TRESUSRoute8$Stop1~TRESUSRoute8$Year))$coefficients[2]

TRESUSRoute=TRESUS[TRESUS$Route==TRESUS$Route[1],]
TRESUSRoute

#linear regression for all point count data

TRESallroutes=matrix(ncol=2,byrow=TRUE)

for (i in TRESUS$Route){
  
  TRESRoute=TRESUS[TRESUS$Route==TRESUS$Route[i],]
  A=TRESUS$Route[i]
  B=summary(lm(formula = TRESRoute$Stop1~TRESRoute$Year))$coefficients[2]
  TRESallroutes=rbind(TRESallroutes, c(A,B))
  
}
head(TRESallroutes)

#Plotting population trends
plot(TRESallroutes)
summary(TRESallroutes)

#linear regression for all point count data for Stop1 with Long and Lat
TSS1=matrix(ncol=8,byrow=TRUE)
dfTSS1=as.data.frame(TSS1)
TrashMatrix=matrix()


for (i in 1:nrow(TRESUS)){
  if (identical(which(TrashMatrix==TRESUS$RouteName[i]),integer(0))){
    if (setequal(unique(USData[USData$RouteName==TRESUS$RouteName[i],]$Year),unique(TRESUS[TRESUS$RouteName==TRESUS$RouteName[i],]$Year))){
      dfTSS1=rbind(dfTSS1, matrix(ncol=8,byrow=TRUE))
      #TSi=TRESUS[TRESUS$StateNum==TRESUS$StateNum[i],]
      RouteName=TRESUS$RouteName[i]
      State=TRESUS$StateNum[i]
      TSiRoute=TRESUS[TRESUS$RouteName==RouteName,]
      if (nrow(TSiRoute)>9){
        RouteDataID=TRESUS$RouteDataID[i]
        Stratum=TRESUS$Stratum[i]
        PopTrend=summary(lm(formula = TSiRoute$Stop1~TSiRoute$Year))$coefficients[2]
        Long=TRESUS$Longitude[i]
        Lat=TRESUS$Latitude[i]
        ObsN=TRESUS$ObsN[i]
        dfTSS1[i,1]=RouteName
        dfTSS1[i,2]=RouteDataID
        dfTSS1[i,3]=State
        dfTSS1[i,4]=Stratum
        dfTSS1[i,5]=PopTrend
        dfTSS1[i,6]=Long
        dfTSS1[i,7]=Lat
        dfTSS1[i,8]=ObsN
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      else{
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      
      #TSS1=rbind(TSS1, c(A,L,S,Y,B,C,D))      
    }
    else{
      dfTSS1=rbind(dfTSS1, matrix(ncol=8,byrow=TRUE))
      #TSi=TRESUS[TRESUS$StateNum==TRESUS$StateNum[i],]
      RouteName=TRESUS$RouteName[i]
      State=TRESUS$StateNum[i]
      ZeroYears=unique(USData[USData$RouteName==TRESUS$RouteName[i],]$Year)[!(unique(USData[USData$RouteName==TRESUS$RouteName[i],]$Year) %in% unique(TRESUS[TRESUS$RouteName==TRESUS$RouteName[i],]$Year))]
      TSiRoute=TRESUS[TRESUS$RouteName==TRESUS$RouteName[i],]
      TSiR2=cbind(matrix(nrow=nrow(TSiRoute)),TSiRoute$Year,TSiRoute$Stop1)
      for (i in 1:length(ZeroYears)){
        TSiR2=rbind(TSiR2,c(NA,ZeroYears[i],0))
      }
      if (nrow(TSiR2>9)){
        RouteDataID=TRESUS$RouteDataID[i]
        Stratum=TRESUS$Stratum[i]
        PopTrend=summary(lm(formula = TSiR2[,3]~TSiR2[,2]))$coefficients[2]
        Long=TRESUS$Longitude[i]
        Lat=TRESUS$Latitude[i]
        ObsN=TRESUS$ObsN[i]
        dfTSS1[i,1]=RouteName
        dfTSS1[i,2]=RouteDataID
        dfTSS1[i,3]=State
        dfTSS1[i,4]=Stratum
        dfTSS1[i,5]=PopTrend
        dfTSS1[i,6]=Long
        dfTSS1[i,7]=Lat
        dfTSS1[i,8]=ObsN
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      else{
        TrashMatrix=rbind(TrashMatrix,matrix(RouteName))
      }
      
      
      #TSS1=rbind(TSS1, c(A,L,S,Y,B,C,D))        
    }
  }
}

head(dfTSS1)
#dfTSS1=as.data.frame(TSS1)
names(dfTSS1) = c("Route","RouteDataID","StateNum","Strata","PopTrend","Longitude","Latitude","Observer_Number")
unique(dfTSS1$PopTrend)
dfTSS1=na.omit(dfTSS1)
nrow(dfTSS1)
head(dfTSS1)

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
PTLC = merge(dfnewhistogram,dfTSS1)
PTLC=na.omit(PTLC)
head(PTLC)
unique(PTLC$PropAg)
plot(PTLC$PopTrend~PTLC$PropAg)
x=lm(formula = PTLC$PopTrend~PTLC$PropAg)
abline(x)
abline(a=0,b=0)
summary(x)
dfPTLC=as.data.frame(PTLC)
write.csv(dfPTLC,"PTLC.csv",FALSE)
dfPTLC=read.csv('PTLC.csv',header=T)

##Model Suite##

#Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
lme1 <- lme(PopTrend ~ 1, random=list(~1|Observer_Number), data=dfPTLC)
#Population trend varies with relation to proportion agriculture
lme2 <- lme(PopTrend ~ PropAg, random=list(~1|Observer_Number), data=dfPTLC)
#Population trend varies with relation to proportion agriculture and longitude (With relation to TRES historic range, land use pattern and precipitation)
lme3 <- lme(PopTrend ~ PropAg + Longitude, random=list(~1|Observer_Number), data=dfPTLC)
#Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
lme4 <- lme(PopTrend ~ PropAg + Latitude, random=list(~1|Observer_Number), data = dfPTLC)
#Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
lme5 <- lme(PopTrend ~ PropAg + Longitude + Latitude, random=list(~1|Observer_Number), data=dfPTLC)

##AIC##
model.sel (lme1, lme2, lme3, lme4, lme5)
summary(lme1)
summary(lme2)