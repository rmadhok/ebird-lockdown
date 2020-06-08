# PROJECT: Nature in Lockdown
# PURPOSE: Process GPM rainfall data
# Data source: GPM 
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
#READ.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/gpm_tiff/'
#SAVE.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/'
#SHP <- '/home/rmadhok/projects/def-sgulati/rmadhok/IndiaPowerPlant/data/shapefiles/district2011/'
READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/gpm_tiff/'
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

for(tif in tifs){
  
  date <- substr(tif, 5, 14)
  print(paste('Processing: ', date, sep=''))
  
  # Read
  r <- raster(tif)

  # Extract mean
  df <- raster::extract(r, india_dist,
                        fun = mean, na.rm = T, sp=T)@data
  
  # Clean
  df <- df %>% 
    mutate(date = date) %>%
    rename(rain = starts_with('GPM')) %>%
    dplyr::select('STATE_UT', 'NAME', 'c_code_11', 'date', 'rain')
  
  # append
  master <- rbind(master, df)
  
}

# Save
write.csv(master, 
          paste(SAVE.DIR, 'india_rain_gpm.csv', sep=""),
          row.names = F)
