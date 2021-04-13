### Western Meadowlark United States With Spatial Data R Script ###

# Author: Nicholas Faraco-Hadlock

# Date: 02/22/2021

# This is R script is for the purposes of taking poplation trends as determined
# in WEMEUS and to correlate them with spatial land cover data.

#Set Working Directory
setwd("C:/Users/Nicholas/Desktop/Senior Thesis Work")

# Connect LCMAP / NLCD Data to Population Trends

#Set reference value for nested for loop
#a=1000
#Create temporary data frame to be combined with initial data frame
#RouteIDLandCover=matrix(ncol=2,byrow=TRUE)

#for (i in dfWMallroutesUS):
#  for (j in landcover):
#    if abs(Long(i)-Long(j)):
#       a=abs(Long(i)-Long(j))
#for (i in dfWMallroutesUS):
#  for (j in landcover):
#    if abs(Long(i)-Long(j)):
#       RouteID(i) = A
#       LandCoverType(j) = B
#       RouteIDLandCover=rbind(RouteIDLandCover, c(A,B))
#WEMEUSwLC=merge(dfWMallroutesUS,RouteIDLandCover)