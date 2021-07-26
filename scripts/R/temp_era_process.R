# PROJECT: Nature in Lockdown
# PURPOSE: Construct weather data
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
#READ.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/era_temp/'
#SAVE.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/'
#SHP <- '/home/rmadhok/projects/def-sgulati/rmadhok/IndiaPowerPlant/data/shapefiles/district2011/'
READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/era_temp/'
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

# --------------------------------------------------------------
# Process
# --------------------------------------------------------------
  
# Read NetCDF Var (hourly data)
rstack <- stack(paste(READ.DIR, 'weather_era.nc', sep=''))

# Crop
india_dist <- spTransform(india_dist, rstack@crs) # sync projections
rstack <- crop(rstack, extent(india_dist))

# Extract mean over shapefile in each layer and convert to dataframe
print('Extracting hourly mean over district boundaries...')
df <- raster::extract(rstack, india_dist,
                      fun = mean, na.rm = T, sp=T)@data

# Reshape to long panel
print('Aggregating to district-daily mean...')
df_long <- df %>% gather(time, temperature, starts_with('X20')) %>%
  select('c_code_11', 'time', 'temperature') %>%
  mutate(date = substr(time, 2, 11),
         date = stringr::str_replace_all(date, '\\.', '-'),
         temperature = temperature - 273.15)
  
# Aggregate to district-daily mean
df_long <- df_long %>%
  group_by(c_code_11, date) %>%
  summarize(temperature = mean(temperature, na.rm=T))
    
# Write
write.csv(df_long, 
          paste(SAVE.DIR, 'india_temperature_era.csv', sep=""),
          row.names = F)
