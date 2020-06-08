# PROJECT: Nature in Lockdown
# PURPOSE: Process modis temperature data
# Data source: MODIS MOD11 
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
#READ.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/modis_temp/'
#SAVE.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/'
#SHP <- '/home/rmadhok/projects/def-sgulati/rmadhok/IndiaPowerPlant/data/shapefiles/district2011/'
READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/modis_temp/'
SAVE.DIR <- '/Users/rmadhok/Dropbox (Personal)/ebird_lockdown/data/'
SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/maps/india-district/'

# Packages
require(raster)
require(rgdal) 
require(sp)
require(tidyverse) 

# --------------------------------------------------------------
# Load Data
# --------------------------------------------------------------

# District shapefiles
india_dist <- readOGR(SHP, 'SDE_DATA_IN_F7DSTRBND_2011', stringsAsFactors = F)

# Raster stack
setwd(READ.DIR)
tifs <- list.files()

# initiate master
master <- data.frame()

# Iterate
for(tif in tifs){
  
  year <- substr(tif, 28, 31)
  day <- as.numeric(substr(tif, 32, 34))
  date <- as.Date(day-1, origin = paste(year, '-01-01', sep=''))
  
  print(paste('Processing: ', date, sep=''))
  
  # Read
  r <- raster(tif)
  r <- crop(r, extent(india_dist)) # crop
  
  # Convert to degrees C
  r <- r * 0.02 - 273.15
  
  # Extract mean
  df <- raster::extract(r, india_dist,
                        fun = mean, na.rm = T, sp=T)@data

  # Clean
  df <- df %>% 
    mutate(date = date) %>%
    rename(temperature = starts_with('MOD')) %>%
    dplyr::select('STATE_UT', 'NAME', 'c_code_11', 'date', 'temperature')
  
  # append
  master <- rbind(master, df)
  
}

write.csv(master, 
          paste(SAVE.DIR, 'india_temp_modis.csv', sep=""),
          row.names = F)

