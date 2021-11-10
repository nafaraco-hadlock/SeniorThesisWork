### Initial Final Analysis R Script Fall 21 Analyses ###

# Author: Nicholas Faraco-Hadlock

# Date: 10/06/2021

# This R script is intended to contain the final analyses for WEME, TRES and NOHA using
# hierarchical beyesian analysis for population trend and determining effects 
# of various explanatory variables, including proportion agriculture using linear
# models assesed by AIC

remove(list=ls()) 

#Set Working Directory
setwd("//rschfs1x/userrs/K-Q/naf42_RS/Desktop")

## Load in Necessary Packages ##

library(sp)

library(raster)

# Install the rgdal package
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

#Install bbsBayes
library(bbsBayes)

#Install other necessary bbsBayes packages
library(sf)
library(ggplot2)

#Read in Adam Analyses
Route_Level_Trend=read.csv('combined_2004_2019_Route_level_BBS_RangeWide_trends_and_intercepts.csv',header=T)

#Read in 30 mile buffer information
Buffers30mile = read.csv('NLCD2016_30MB.csv',header = T)

#Read in Land Cover Change Data
LCC = read.csv('LCC_30MB.csv',header = T)

#Read in Plant Hardiness Zone Data
PHZc = read.csv('PHZroutes.csv',header = T)
PHZ_30MB = read.csv('PHZ_30MB.csv',header = T)

#Create Data Frame for Prop Ag
pa=matrix(ncol=3,byrow=TRUE)
dfpropag=as.data.frame(pa)
for (i in 1:nrow(Buffers30mile)){
  dfpropag=rbind(dfpropag, matrix(ncol=3,byrow=TRUE))
  b=Buffers30mile$Longitude[i]
  c=Buffers30mile$Latitude[i]
  propag=(Buffers30mile$HISTO_30_Miles_2016_NLCD82[i])/(Buffers30mile$HISTO_30_Miles_2016_NLCD0[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD11[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD21[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD22[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD23[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD24[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD31[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD41[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD42[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD43[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD52[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD71[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD81[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD82[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD90[i]+Buffers30mile$HISTO_30_Miles_2016_NLCD95[i])
  dfpropag[i,1]=propag
  dfpropag[i,2]=b
  dfpropag[i,3]=c 
}
names(dfpropag)=c("PropAg","Longitude","Latitude")
head(dfpropag)

#Add Prop Ag to Trend Data Frame
MainData = merge(dfpropag,Route_Level_Trend,by="Latitude")
write.csv(MainData,"RLTrendsPropAg.csv",FALSE)
plot(MainData$Trend ~ MainData$PropAg)
WEME = MainData[MainData$english_name=="Western Meadowlark",]
TRES = MainData[MainData$english_name=="Tree Swallow",]
NOHA = MainData[MainData$english_name=="Northern Harrier",]
FEHA = MainData[MainData$english_name=="Ferruginous Hawk",]
AMKE = MainData[MainData$english_name=="American Kestrel",]
BASW = MainData[MainData$english_name=="Barn Swallow",]
EAME = MainData[MainData$english_name=="Eastern Meadowlark",]
PRFA = MainData[MainData$english_name=="Prairie Falcon",]
RTHA = MainData[MainData$english_name=="Red-tailed Hawk (all forms)",]
head(WEME)

#Land Cover Change Data
LandChange=matrix(ncol=3,byrow=TRUE)
dfLandChange=as.data.frame(LandChange)
for (i in 1:nrow(LCC)){
  dfLandChange=rbind(dfLandChange, matrix(ncol=3,byrow=TRUE))
  b=LCC$Longitude[i]
  c=LCC$Latitude[i]
  propstatic=(LCC$HISTO_LCC_30MB1[i]/sum(LCC$HISTO_LCC_30MB0[i],LCC$HISTO_LCC_30MB1[i],LCC$HISTO_LCC_30MB2[i],LCC$HISTO_LCC_30MB3[i],LCC$HISTO_LCC_30MB4[i],LCC$HISTO_LCC_30MB5[i],LCC$HISTO_LCC_30MB6[i],LCC$HISTO_LCC_30MB7[i],LCC$HISTO_LCC_30MB8[i],LCC$HISTO_LCC_30MB9[i],LCC$HISTO_LCC_30MB10[i],LCC$HISTO_LCC_30MB11[i]))
  dfLandChange[i,1]=propstatic
  dfLandChange[i,2]=b
  dfLandChange[i,3]=c
}
names(dfLandChange)=c("PropStatic","Longitude","Latitude")
head(dfLandChange)
WEME = merge(WEME,dfLandChange,by="Latitude")
TRES = merge(TRES,dfLandChange,by="Latitude")
NOHA = merge(NOHA,dfLandChange,by="Latitude")
FEHA = merge(FEHA,dfLandChange,by="Latitude")
AMKE = merge(AMKE,dfLandChange,by="Latitude")
BASW = merge(BASW,dfLandChange,by="Latitude")
EAME = merge(EAME,dfLandChange,by="Latitude")
PRFA = merge(PRFA,dfLandChange,by="Latitude")
RTHA = merge(RTHA,dfLandChange,by="Latitude")

#PHZ Data
newPHZc=matrix(ncol=4,byrow=TRUE)
dfPHZc=as.data.frame(newPHZc)
for (i in 1:nrow(PHZc)){
  dfPHZc=rbind(dfPHZc, matrix(ncol=4,byrow=TRUE))
  b=PHZc$Longitude[i]
  c=PHZc$Latitude[i]
  PHZ=PHZc$rvalue_1[i]
  PHZCel=PHZc$temp_c[i]
  dfPHZc[i,1]=PHZ
  dfPHZc[i,2]=PHZCel
  dfPHZc[i,3]=b
  dfPHZc[i,4]=c 
}
names(dfPHZc)=c("PHZ","PHZC","Longitude","Latitude")
WEME = merge(WEME,dfPHZc,by="Latitude")
TRES = merge(TRES,dfPHZc,by="Latitude")
NOHA = merge(NOHA,dfPHZc,by="Latitude")
FEHA = merge(FEHA,dfPHZc,by="Latitude")
AMKE = merge(AMKE,dfPHZc,by="Latitude")
BASW = merge(BASW,dfPHZc,by="Latitude")
EAME = merge(EAME,dfPHZc,by="Latitude")
PRFA = merge(PRFA,dfPHZc,by="Latitude")
RTHA = merge(RTHA,dfPHZc,by="Latitude")

#Clean
WEME = WEME[WEME$PropAg!=0,]
WEME = na.omit(WEME)
WEME = WEME[WEME$PropStatic>0.9,]
TRES = TRES[TRES$PropAg!=0,]
TRES = na.omit(TRES)
TRES = TRES[TRES$PropStatic>0.9,]
NOHA = NOHA[NOHA$PropAg!=0,]
NOHA = na.omit(NOHA)
NOHA = NOHA[NOHA$PropStatic>0.9,]
FEHA = FEHA[FEHA$PropAg!=0,]
FEHA = na.omit(FEHA)
FEHA = FEHA[FEHA$PropStatic>0.9,]
AMKE = AMKE[AMKE$PropAg!=0,]
AMKE = na.omit(AMKE)
AMKE = AMKE[AMKE$PropStatic>0.9,]
BASW = BASW[BASW$PropAg!=0,]
BASW = na.omit(BASW)
BASW = BASW[BASW$PropStatic>0.9,]
EAME = EAME[EAME$PropAg!=0,]
EAME = na.omit(EAME)
EAME = EAME[EAME$PropStatic>0.9,]
PRFA = PRFA[PRFA$PropAg!=0,]
PRFA = na.omit(PRFA)
PRFA = PRFA[PRFA$PropStatic>0.9,]
RTHA = RTHA[RTHA$PropAg!=0,]
RTHA = na.omit(RTHA)
RTHA = RTHA[RTHA$PropStatic>0.9,]
#Save as Data Frame for Model Suite
dfWEME=as.data.frame(WEME)
dfTRES=as.data.frame(TRES)
dfNOHA=as.data.frame(NOHA)
dfFEHA=as.data.frame(FEHA)
dfAMKE=as.data.frame(AMKE)
dfBASW=as.data.frame(BASW)
dfEAME=as.data.frame(EAME)
dfPRFA=as.data.frame(PRFA)
dfRTHA=as.data.frame(RTHA)

#DATA READY FOR MODEL SUITE!

#Save Data
write.csv(dfWEME,"WEMEIFA.csv",FALSE)
write.csv(dfTRES,"TRESIFA.csv",FALSE)
write.csv(dfNOHA,"NOHAIFA.csv",FALSE)
write.csv(dfFEHA,"FEHAIFA.csv",FALSE)
write.csv(dfAMKE,"AMKEIFA.csv",FALSE)
write.csv(dfBASW,"BASWIFA.csv",FALSE)
write.csv(dfEAME,"EAMEIFA.csv",FALSE)
write.csv(dfPRFA,"PRFAIFA.csv",FALSE)
write.csv(dfRTHA,"RTHAIFA.csv",FALSE)

#Plot Prop Ag vs Trend
plot(WEME$Trend ~ WEME$PropAg)
plot(WEME$Trend ~ WEME$Longitude.x)
plot(WEME$Trend ~ WEME$Latitude)
x=lm(WEME$Trend ~ WEME$PropAg)
abline(x)
summary(x)

##Model Suite##
# Western Meadowlark #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
WEMElm1 <- lm(Trend ~ 1, data=dfWEME)
# #Population trend varies with relation to proportion agriculture
WEMElm2 <- lm(Trend ~ PropAg, data=dfWEME)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
WEMElm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfWEME)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
WEMElm4 <- lm(Trend ~ PropAg + Latitude, data = dfWEME)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
WEMElm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfWEME)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
WEMElm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfWEME)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
WEMElm7 <- lm(Trend ~ PropAg*Latitude, data=dfWEME)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
WEMElm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfWEME)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
WEMElm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfWEME)
# #Population trend varies with plant hardiness zone (biome level effects)
WEMElm10 <- lm(Trend ~ PHZ, data=dfWEME)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
WEMElm11 <- lm(Trend ~ PHZ + PropAg, data=dfWEME)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
WEMElm12 <- lm(Trend ~ PHZ*PropAg, data=dfWEME)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
WEMElm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfWEME)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
WEMElm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfWEME)

##AIC##
model.sel (WEMElm1, WEMElm2, WEMElm3, WEMElm4, WEMElm5, WEMElm6, WEMElm7, WEMElm8, WEMElm9, WEMElm10, WEMElm11, WEMElm12, WEMElm13, WEMElm14)

##Model Suite##
# Tree Swallow #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
TRESlm1 <- lm(Trend ~ 1, data=dfTRES)
# #Population trend varies with relation to proportion agriculture
TRESlm2 <- lm(Trend ~ PropAg, data=dfTRES)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
TRESlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfTRES)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
TRESlm4 <- lm(Trend ~ PropAg + Latitude, data = dfTRES)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
TRESlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfTRES)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
TRESlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfTRES)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
TRESlm7 <- lm(Trend ~ PropAg*Latitude, data=dfTRES)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
TRESlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfTRES)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
TRESlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfTRES)
# #Population trend varies with plant hardiness zone (biome level effects)
TRESlm10 <- lm(Trend ~ PHZ, data=dfTRES)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
TRESlm11 <- lm(Trend ~ PHZ + PropAg, data=dfTRES)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
TRESlm12 <- lm(Trend ~ PHZ*PropAg, data=dfTRES)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
TRESlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfTRES)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
TRESlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfTRES)

##AIC##
model.sel (TRESlm1, TRESlm2, TRESlm3, TRESlm4, TRESlm5, TRESlm6, TRESlm7, TRESlm8, TRESlm9, TRESlm10, TRESlm11, TRESlm12, TRESlm13, TRESlm14)

##Model Suite##
# Northern Harrier #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
NOHAlm1 <- lm(Trend ~ 1, data=dfNOHA)
# #Population trend varies with relation to proportion agriculture
NOHAlm2 <- lm(Trend ~ PropAg, data=dfNOHA)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
NOHAlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfNOHA)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
NOHAlm4 <- lm(Trend ~ PropAg + Latitude, data = dfNOHA)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
NOHAlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfNOHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
NOHAlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfNOHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
NOHAlm7 <- lm(Trend ~ PropAg*Latitude, data=dfNOHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
NOHAlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfNOHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
NOHAlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfNOHA)
# #Population trend varies with plant hardiness zone (biome level effects)
NOHAlm10 <- lm(Trend ~ PHZ, data=dfNOHA)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
NOHAlm11 <- lm(Trend ~ PHZ + PropAg, data=dfNOHA)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
NOHAlm12 <- lm(Trend ~ PHZ*PropAg, data=dfNOHA)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
NOHAlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfNOHA)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
NOHAlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfNOHA)

##AIC##
model.sel (NOHAlm1, NOHAlm2, NOHAlm3, NOHAlm4, NOHAlm5, NOHAlm6, NOHAlm7, NOHAlm8, NOHAlm9, NOHAlm10, NOHAlm11, NOHAlm12, NOHAlm13, NOHAlm14)

##Model Suite##
# Ameriacn Kestrel #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
AMKElm1 <- lm(Trend ~ 1, data=dfAMKE)
# #Population trend varies with relation to proportion agriculture
AMKElm2 <- lm(Trend ~ PropAg, data=dfAMKE)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
AMKElm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfAMKE)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
AMKElm4 <- lm(Trend ~ PropAg + Latitude, data = dfAMKE)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
AMKElm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfAMKE)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
AMKElm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfAMKE)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
AMKElm7 <- lm(Trend ~ PropAg*Latitude, data=dfAMKE)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
AMKElm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfAMKE)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
AMKElm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfAMKE)
# #Population trend varies with plant hardiness zone (biome level effects)
AMKElm10 <- lm(Trend ~ PHZ, data=dfAMKE)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
AMKElm11 <- lm(Trend ~ PHZ + PropAg, data=dfAMKE)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
AMKElm12 <- lm(Trend ~ PHZ*PropAg, data=dfAMKE)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
AMKElm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfAMKE)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
AMKElm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfAMKE)

##AIC##
model.sel (AMKElm1, AMKElm2, AMKElm3, AMKElm4, AMKElm5, AMKElm6, AMKElm7, AMKElm8, AMKElm9, AMKElm10, AMKElm11, AMKElm12, AMKElm13, AMKElm14)

##Model Suite##
# Ferruginous Hawk #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
FEHAlm1 <- lm(Trend ~ 1, data=dfFEHA)
# #Population trend varies with relation to proportion agriculture
FEHAlm2 <- lm(Trend ~ PropAg, data=dfFEHA)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
FEHAlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfFEHA)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
FEHAlm4 <- lm(Trend ~ PropAg + Latitude, data = dfFEHA)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
FEHAlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfFEHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
FEHAlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfFEHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
FEHAlm7 <- lm(Trend ~ PropAg*Latitude, data=dfFEHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
FEHAlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfFEHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
FEHAlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfFEHA)
# #Population trend varies with plant hardiness zone (biome level effects)
FEHAlm10 <- lm(Trend ~ PHZ, data=dfFEHA)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
FEHAlm11 <- lm(Trend ~ PHZ + PropAg, data=dfFEHA)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
FEHAlm12 <- lm(Trend ~ PHZ*PropAg, data=dfFEHA)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
FEHAlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfFEHA)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
FEHAlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfFEHA)

##AIC##
model.sel (FEHAlm1, FEHAlm2, FEHAlm3, FEHAlm4, FEHAlm5, FEHAlm6, FEHAlm7, FEHAlm8, FEHAlm9, FEHAlm10, FEHAlm11, FEHAlm12, FEHAlm13, FEHAlm14)

##Model Suite##
# Barn Swallow #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
BASWlm1 <- lm(Trend ~ 1, data=dfBASW)
# #Population trend varies with relation to proportion agriculture
BASWlm2 <- lm(Trend ~ PropAg, data=dfBASW)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
BASWlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfBASW)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
BASWlm4 <- lm(Trend ~ PropAg + Latitude, data = dfBASW)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
BASWlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfBASW)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
BASWlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfBASW)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
BASWlm7 <- lm(Trend ~ PropAg*Latitude, data=dfBASW)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
BASWlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfBASW)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
BASWlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfBASW)
# #Population trend varies with plant hardiness zone (biome level effects)
BASWlm10 <- lm(Trend ~ PHZ, data=dfBASW)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
BASWlm11 <- lm(Trend ~ PHZ + PropAg, data=dfBASW)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
BASWlm12 <- lm(Trend ~ PHZ*PropAg, data=dfBASW)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
BASWlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfBASW)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
BASWlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfBASW)

##AIC##
model.sel (BASWlm1, BASWlm2, BASWlm3, BASWlm4, BASWlm5, BASWlm6, BASWlm7, BASWlm8, BASWlm9, BASWlm10, BASWlm11, BASWlm12, BASWlm13, BASWlm14)

##Model Suite##
# Eastern Meadlowlark #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
EAMElm1 <- lm(Trend ~ 1, data=dfEAME)
# #Population trend varies with relation to proportion agriculture
EAMElm2 <- lm(Trend ~ PropAg, data=dfEAME)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
EAMElm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfEAME)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
EAMElm4 <- lm(Trend ~ PropAg + Latitude, data = dfEAME)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
EAMElm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfEAME)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
EAMElm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfEAME)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
EAMElm7 <- lm(Trend ~ PropAg*Latitude, data=dfEAME)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
EAMElm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfEAME)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
EAMElm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfEAME)
# #Population trend varies with plant hardiness zone (biome level effects)
EAMElm10 <- lm(Trend ~ PHZ, data=dfEAME)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
EAMElm11 <- lm(Trend ~ PHZ + PropAg, data=dfEAME)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
EAMElm12 <- lm(Trend ~ PHZ*PropAg, data=dfEAME)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
EAMElm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfEAME)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
EAMElm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfEAME)

##AIC##
model.sel (EAMElm1, EAMElm2, EAMElm3, EAMElm4, EAMElm5, EAMElm6, EAMElm7, EAMElm8, EAMElm9, EAMElm10, EAMElm11, EAMElm12, EAMElm13, EAMElm14)

##Model Suite##
# Prairie Falcon #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
PRFAlm1 <- lm(Trend ~ 1, data=dfPRFA)
# #Population trend varies with relation to proportion agriculture
PRFAlm2 <- lm(Trend ~ PropAg, data=dfPRFA)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
PRFAlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfPRFA)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
PRFAlm4 <- lm(Trend ~ PropAg + Latitude, data = dfPRFA)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
PRFAlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfPRFA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
PRFAlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfPRFA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
PRFAlm7 <- lm(Trend ~ PropAg*Latitude, data=dfPRFA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
PRFAlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfPRFA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
PRFAlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfPRFA)
# #Population trend varies with plant hardiness zone (biome level effects)
PRFAlm10 <- lm(Trend ~ PHZ, data=dfPRFA)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
PRFAlm11 <- lm(Trend ~ PHZ + PropAg, data=dfPRFA)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
PRFAlm12 <- lm(Trend ~ PHZ*PropAg, data=dfPRFA)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
PRFAlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfPRFA)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
PRFAlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfPRFA)

##AIC##
model.sel (PRFAlm1, PRFAlm2, PRFAlm3, PRFAlm4, PRFAlm5, PRFAlm6, PRFAlm7, PRFAlm8, PRFAlm9, PRFAlm10, PRFAlm11, PRFAlm12, PRFAlm13, PRFAlm14)

##Model Suite##
# Red-tailed Hawk #

# #Null Model: Population Trend Varies Randomly (not varying with propag or coordinate)
RTHAlm1 <- lm(Trend ~ 1, data=dfRTHA)
# #Population trend varies with relation to proportion agriculture
RTHAlm2 <- lm(Trend ~ PropAg, data=dfRTHA)
# #Population trend varies with relation to proportion agriculture and longitude (With relation to WEME historic range, land use pattern and precipitation)
RTHAlm3 <- lm(Trend ~ PropAg + Longitude.x, data=dfRTHA)
# #Population trend varies with relation to proportion agriculture and latitude (migration distance, preferred breeding habitat, mean annual temperature)
RTHAlm4 <- lm(Trend ~ PropAg + Latitude, data = dfRTHA)
# #Population trend varies with relation to proportion agriculture, latitude and longitude (reasons above)
RTHAlm5 <- lm(Trend ~ PropAg + Longitude.x + Latitude, data=dfRTHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture (differenet effects of agriculture at different longitudes)
RTHAlm6 <- lm(Trend ~ PropAg*Longitude.x, data=dfRTHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture (differenet effects of agriculture at different latitidues)
RTHAlm7 <- lm(Trend ~ PropAg*Latitude, data=dfRTHA)
# #Population trend varies with an interaction between Latitude and Proportion agriculture and Longitude (differenet effects of agriculture at different latitidues combined with longitude)
RTHAlm8 <- lm(Trend ~ PropAg*Latitude + Longitude.x, data=dfRTHA)
# #Population trend varies with an interaction between Longitude and Proportion agriculture and Longitude (differenet effects of agriculture at different longitudes combined with latitude)
RTHAlm9 <- lm(Trend ~ PropAg*Longitude.x + Latitude, data=dfRTHA)
# #Population trend varies with plant hardiness zone (biome level effects)
RTHAlm10 <- lm(Trend ~ PHZ, data=dfRTHA)
# #Population trend varies with plant hardiness zone and prop ag (biome level effects and prop ag)
RTHAlm11 <- lm(Trend ~ PHZ + PropAg, data=dfRTHA)
# #Population trend varies with interaction between plant hardiness zone and propag (different propag effects in different PHZ's)
RTHAlm12 <- lm(Trend ~ PHZ*PropAg, data=dfRTHA)
# #Population trend varies with plant hardiness zone and prop ag and longtitude (biome level effects, prop ag and longitude)
RTHAlm13 <- lm(Trend ~ PHZ + PropAg + Longitude.x, data=dfRTHA)
# #Population trend varies with interaction between plant hardiness zone and propag and longtidue (different propag effects in different PHZ's and longitude)
RTHAlm14 <- lm(Trend ~ PHZ*PropAg + Longitude.x, data=dfRTHA)

##AIC##
model.sel (RTHAlm1, RTHAlm2, RTHAlm3, RTHAlm4, RTHAlm5, RTHAlm6, RTHAlm7, RTHAlm8, RTHAlm9, RTHAlm10, RTHAlm11, RTHAlm12, RTHAlm13, RTHAlm14)