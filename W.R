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
setwd("C:/Users/Nicholas/Desktop/Senior Thesis Work")

## Load in Necessary Packages ##

library(sp)

library(raster)

# 5. Install the rgdal package
install.packages(c('rgdal'),repos = "http://cran.case.edu", configure.args=c("--with-proj-include=/packages/PROJ/6.1.0/include","--with-proj-lib=/packages/PROJ/6.1.0/lib"))

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

#plot population agianst year
plot(wm$StopTotal ~ wm$Year)

#Plot for a single route
wmRoute8=wm[wm$Route==8,]
plot(wmRoute8$StopTotal~wmRoute8$Year)

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

#Plot Population Trend as a function of Latitude
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

#Plot Population Trend as a function of Longitude
WMAFLong=matrix(ncol=3,byrow=TRUE)
wm3=colorado3[colorado3$AOU==05011,]
for (i in wm$Route){
  
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

#Save data as CSVs
dfWMallroutes=as.data.frame(WMallroutes)
dfWMAFLat=as.data.frame(WMAFLat)
dfWMAFLong=as.data.frame(WMAFLong)
write.csv(dfWMAFLong,"WMAFLong",FALSE)
write.csv(dfWMAFLat,"WMAFLat",FALSE)
write.csv(dfWMallroutes,"WMallroutes",FALSE)

#Read in LCMAP data as TIFF Files

imported_raster_LCMAP=raster('LandCoverData1.tiff')
rasterpoints=rasterToPoints(imported_raster_LCMAP)
head(rasterpoints)

#Read in NLCD data as TIFF Files

imported_raster_NLCD=raster('nlcdmdutm17.tif')
rasterpoints=rasterToPoints(imported_raster_NLCD)
head(rasterpoints)


#Read in LCMAP data as csv

LCMAP=read.csv('LCMAP_CU_20200414_V01_REF.csv',header=TRUE)
head(LCMAP)













#Discarded Code

#wmRoute=wm[wm$Route==wm$Route[i],]
#plot(wmRoute$StopTotal~wmRoute$Year)
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

