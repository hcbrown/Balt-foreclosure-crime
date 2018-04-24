################# ********Foreclosures and Crime in Baltimore City******** ##################
################## ***by Hayley Brown*** #######

########### ****Initial steps**** ###########
# Load packages
library(foreign)
library(maptools)
library(network)
library(rgdal)
library(splm)
library(sp)
library(spatstat)
library(ggmap)
library(rgeos)

# Make it able to find the folder
setwd("~/Documents/school 2/Fall 2014/Applied Spatial Econometrics/Applied Spatial Econometrics paper/Baltimoredatashapefiles")

########### ****Geocode foreclosure addresses**** ###########
# Load data file
baltf <- read.csv("baltforeclosures.csv",sep=",",header=T)

# Append city and state to address line
faddresses <- apply(as.matrix(baltf$Address), 2, function(x) paste0(x, ', Baltimore, MD'))

# Define a function that will process Google’s server responses
getGeoDetails <- function(address){   

# Use the gecode function to query Google servers
geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)

# Extract desired bits from the returned list
answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status  
  
  # If over query limit - pause for ten minutes, switch IP address, restart
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 10 minutes:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(10*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
# Return NA's for addresses that don’t yield a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   

  # Extract results from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

# Initialize a dataframe to hold the results
fgeocoded <- data.frame()

# Determine where to start in address list (if script was previously interrupted):
startindex <- 1

# Load temp file
ftempfile <- paste0('_temp_geocoded.rds')
if (file.exists(ftempfile)){
  print("Found temp file - resuming from index:")
  fgeocoded <- readRDS(ftempfile)
  startindex <- nrow(fgeocoded)
  print(startindex)
}

# Begin geocoding process - address by address
for (ii in seq(startindex, length(faddresses))){
  print(paste("Working on index", ii, "of", length(faddresses)))
# Query the Google geocoder - will pause here if over the limit
  result = getGeoDetails(faddresses[ii]) 
  print(result$status)     
  result$index <- ii
# Append the answer to the results file
  fgeocoded <- rbind(fgeocoded, result)
  # Save temporary results as I go
  saveRDS(fgeocoded, ftempfile)
}

#  Add the latitude and longitude to the main data
baltforeclosure$lat <- fgeocoded$lat
baltforeclosure$long <- fgeocoded$lat
baltforeclosure$accuracy <- fgeocoded$accuracy

# Output files
saveRDS(baltimoreforeclosure, paste0("../data/", fgeocoded.rds"))
                                     write.table(baltimoreforeclosure, file=paste0("../data/", infile ,"fgeocoded.csv"), sep=",", row.names=FALSE)
                                     

########### ****Geocode remaining crime addresses**** ###########
# Load data file
baltc <- read.csv("baltcrime08-10.csv",sep=",",header=T)

# Append city and state to address line
crimeaddresses <- apply(as.matrix(baltc$Address), 2, function(x) paste0(x, ', Baltimore, MD'))

# Initialize a dataframe to hold the results
crimegeocoded <- data.frame()

# Determine where to start in address list (if script was previously interrupted):
        startindex <- 1
    # Load temp file #
                                    crimetempfile <- paste0('_temp_geocoded.rds')
                                    if (file.exists(crimetempfile)){
                                     print("Found temp file - resuming from index:")
                                     crimegeocoded <- readRDS(crimetempfile)
                                     startindex <- nrow(crimegeocoded)
                                     print(startindex)
                                     }
   # Begin geocoding process - address by address #
                                     for (ii in seq(startindex, length(crimeaddresses))){
                                     print(paste("Working on index", ii, "of", length(crimeaddresses)))
   # Query the Google geocoder - will pause here if over the limit #
                                     result = getGeoDetails(crimeaddresses[ii]) 
                                     print(result$status)     
                                     result$index <- ii
   # Append the answer to the results file #
                                     crimegeocoded <- rbind(crimegeocoded, result)
   # Save temporary results as I go #
                                     saveRDS(crimegeocoded, crimetempfile)
                                     }

#  Add the latitude and longitude to the main data
baltcrime2008to2010$lat <- crimegeocoded$lat
baltcrime2008to2010$long <- crimegeocoded$lat
baltcrime2008to2010$accuracy <- crimegeocoded$accuracy

# Output files
saveRDS(baltcrime2008to2010, paste0("../data/", ,"_crimegeocoded.rds"))
                                     write.table(baltcrime2008to2010, file=paste0("../data/","crimegeocoded.csv"), sep=",", row.names=FALSE)
                                     
########### ****Convert coordinate systems**** ###########
# Load data files
baltcrime <- read.csv("baltcrime.csv",sep=",",header=T)
baltforeclosure <- read.csv("baltforeclosure.csv",sep=",",header=T)

# Load shapefiles
city_shp <- readOGR(dsn='Baltcity_20Line', layer='baltcity_line')
baltgrid <- readOGR(dsn='baltgrid', layer='baltgrid')
                                     
# Store original projection
origProj <- city_shp@proj4string

# Convert lat/long to Maryland grid for crime data
latlng_df2 <- baltcrime[,c('long','lat')] 
latlng_spdf <- SpatialPoints(latlng_df2,  proj4string=CRS("+proj=longlat +datum=WGS84")) 
latlng_spdf <-  spTransform(latlng_spdf,origProj) 
latlng_spdf_coords <- coordinates(latlng_spdf) 
baltcrime$long <-  latlng_spdf_coords[,1] 
baltcrime$lat <-  latlng_spdf_coords[,2]
                                     
# Convert lat/long to Maryland grid for foreclosure data
latlng_df2 <- baltforeclosure[,c('long','lat')] 
latlng_spdf <- SpatialPoints(latlng_df2,  proj4string=CRS("+proj=longlat +datum=WGS84")) 
latlng_spdf <-  spTransform(latlng_spdf,origProj) 
latlng_spdf_coords <- coordinates(latlng_spdf) 
baltforeclosure$long <-  latlng_spdf_coords[,1] 
baltforeclosure$lat <-  latlng_spdf_coords[,2]

########### ****Spatial weights matrix**** ###########
# Create a queen’s continuity spatial weight matrix
library(McSpatial)
baltqww <- makew(shpfile=baltgrid, method="queen")
baltqwmat <- baltqww$wmat

# mat2listw
library(spdep)
baltqlw <- mat2listw(baltqwmat)
 
# Create nearest neighbors spatial weight matrices for a variety of distances
dnn200m <- dnearneigh(coordinates(baltgrid), 0, 2000)
summary(dnn200m)
dnn500m <- dnearneigh(coordinates(baltgrid), 0, 5000)
summary(dnn500m)
dnn1km <- dnearneigh(coordinates(baltgrid), 0, 10000)
summary(dnn1km)

# mat2listw
dnn200mlw <- mat2listw(dnn200m)
dnn500mlw <- mat2listw(dnn500m)
dnn1kmlw <- mat2listw(dnn1km)


########### ****Define formulae for a series of regressions**** ###########
# Rename things to make them more accessible
allcrime <- baltcrime$overallcrime_totalcount
propcrime <- baltcrime$propertycrime_totalcount
violcrime <- baltcrime$violentcrime_totalcount
foreclosure <- baltforeclosure$foreclosure_totalcount
                                     
# Define regression formulae
fmAC <- allcrime ~ foreclosure
fmPC <- propcrime ~ foreclosure
fmVC <- violcrime ~ foreclosure

# Hausman test for property crime
mod1PC<- spgm(formula = fmPC, data = baltfc, index = c("CSA2010", "Year"), listw = baltqwmat, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
mod2PC<- spgm(formula = fmPC, data = baltfc, index = c("CSA2010", "Year"), listw = baltqwmat, model = "within", spatial.error = TRUE, lag = TRUE)
test2PC<-sphtest(x = mod1PC, x2 = mod2PC)
test2PC

# Hausman test for violent crime
mod1VC<- spgm(formula = fmVC, data = baltfc, index = c("CSA2010", "Year"), listw = baltqwmat, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
mod2VC<- spgm(formula = fmVC, data = baltfc, index = c("CSA2010", "Year"), listw = baltqwmat, model = "within", spatial.error = TRUE, lag = TRUE)
test2VC<-sphtest(x = mod1VC, x2 = mod2VC)
test2VC

# Hausman test for all crime
mod1AC<- spgm(formula = fmAC, data = baltfc, index = c("CSA2010", "Year"), listw = baltqwmat, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
mod2AC<- spgm(formula = fmAC, data = baltfc, index = c("Cell", "Year"), listw = baltqwmat, model = "within", spatial.error = TRUE, lag = TRUE)
test2AC<-sphtest(x = mod1AC, x2 = mod2AC)
test2AC

########### ****Fixed effects spatial lag models**** ###########
# Fixed effects spatial lag models for all crime – queen continuity
sarfemodAC <- spml(formula = fmAC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)

# Fixed effects spatial lag models for all crime – more distant neighbors
sarfemodAC <- spml(formula = fmAC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)

# Fixed effects spatial lag models for property crime – queen continuity
sarfemodPC <- spml(formula = fmPC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)
                                     
# Fixed effects spatial lag models for property crime – more distant neighbors
sarfemodPC <- spml(formula = fmPC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)

# Fixed effects spatial lag models for violent crime – queen continuity
sarfemodVC <- spml(formula = fmVC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)
                                     
# Fixed effects spatial lag models for violent crime – more distant neighbors
sarfemodVC <- spml(formula = fmVC, data = baltfc, index = c("Cell", "Year"), listw = baltqlw, model="within", effect = "individual", method = "eigen", na.action = na.fail, quiet = TRUE, zero.policy = NULL, tol.solve = 1e-10 )
summary(sarfemodAC)




