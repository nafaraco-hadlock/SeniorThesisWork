# To install from CRAN:
install.packages("bbsBayes")
library(bbsBayes)

#Get Route Level Data
fetch_bbs_data()

# set up for the annual CWS status and trend analysis
strat_data <- stratify(by = "bbs_cws")

library(sf)
library(ggplot2)
laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area coordinate reference system for pretty mapping


stratifications <- c("bbs_cws","bbs_usgs","bcr","latlong","state")

strata_map = load_map(stratifications[2]) #selecting the map for "bbs_usgs" stratification
strata_map = st_transform(strata_map,crs = laea) #transforming the coordinate reference system
# mapping using geom_sf()
st_gg = ggplot(data = strata_map)+
  geom_sf()+
  labs(title = paste("stratify(by =",stratifications[2],")"))

plot(st_gg)