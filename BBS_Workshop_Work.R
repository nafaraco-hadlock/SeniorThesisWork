# To install from CRAN:
install.packages("bbsBayes")
library(bbsBayes)

#Get Route Level Data
fetch_bbs_data()

# set up for the annual CWS status and trend analysis
strat_data <- stratify(by = "latlong")

library(sf)
library(ggplot2)
laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area coordinate reference system for pretty mapping


stratifications <- c("bbs_cws","bbs_usgs","bcr","latlong","state")

strata_map = load_map(stratifications[4]) #selecting the map for "bbs_usgs" stratification
strata_map = st_transform(strata_map,crs = laea) #transforming the coordinate reference system
# mapping using geom_sf()
st_gg = ggplot(data = strata_map)+
  geom_sf()+
  labs(title = paste("stratify(by =",stratifications[2],")"))

plot(st_gg)

write.csv(strat_data$species_strat,"Full_BBS_speciesList.csv")

jags_data <- prepare_jags_data(strat_data = stratify(by = "latlong"),
                               species_to_run = "Western Meadowlark",
                               model = "slope",
                               min_n_routes = 1)

# Not Run - Note: for Barn Swallow, this may require ~ 6-days to run!
jags_mod <- run_model(jags_data = jags_data,
                      n_iter = 24000, #higher than the default 10,000
                      n_burnin = 20000,
                      n_chains = 3,
                      n_thin = 20, #saves memory by retaining only 1/20 posterior samples
                      parallel = TRUE,
                      inits = NULL,
                      parameters_to_save = c("n","n3"))
