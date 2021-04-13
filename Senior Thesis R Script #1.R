# Nicholas Faraco-Hadlock Senior Thesis Work
#

remove(list=ls()) 
library(raster)
library(tidyverse)
setwd("C:/Users/Nicholas/Desktop/Senior Thesis Work")

##TIF File Stuff##
#library(tiff)
#library(raster)
#str_name<-'MOD16A2_ET_0.05deg_GEO_2008M01.tif' 
#read_file<-readTIFF(str_name) 

##Reading in BBS Data##
#routes
routes=read.csv('routes.csv',header=T)
head(routes)
#first data set from 50 states data
fifty1=read.csv('fifty1.csv',header=T)
head(fifty1)

plot(fifty1$AOU ~ fifty1$RouteDataID)
plot(routes$Route ~ routes$Latitude)


# Read tif file with values that are long integer type
tif_arc <- raster("tif_arc.tif")

#Read csv file that has an attribute 'ID' the value of which = the raster value on the tif file 
csv_whole <- read_delim("csv_qgis.csv",delim = ",")
