# PROJECT: Nature in Lockdown
# PURPOSE: Construct weather data
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/'
SAVE.DIR <- '/Users/rmadhok/Dropbox (Personal)/covid/ebird_lockdown/data/'
SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'

# Packages
require(raster)
require(rgdal) 
require(sp)
require(tidyverse) 

# --------------------------------------------------------------
# Load Data
# --------------------------------------------------------------

# District shapefiles
india_dist <- readOGR(paste(SHP, "maps/india-district", sep=""), 
                                         'SDE_DATA_IN_F7DSTRBND_2011', 
                                         stringsAsFactors = F)

# --------------------------------------------------------------
# Process
# --------------------------------------------------------------

# Iterate through variables
for(weather in c('t2m', 'tp')) {
  
  print(paste('Processing: ', weather, sep=''))
  
  # Read NetCDF Var (hourly data)
  rstack <- stack(paste(READ.DIR, 'india-weather-raw.nc', sep=''), varname=weather)

  # Crop
  india_dist <- spTransform(india_dist, rstack@crs) # sync projections
  rstack <- crop(rstack, extent(india_dist))

  # Extrack mean over shapefile in each layer and convert to dataframe
  df <- raster::extract(rstack, india_dist,
                        fun = mean, na.rm = T, sp=T)@data

  # Reshape to long panel
  df_long <- df %>% gather(time, value, starts_with('X20')) %>%
    select('STATE_UT', 'NAME', 'c_code_11', 'time', 'value') %>%
    mutate(date = substr(time, 2, 11),
           date = stringr::str_replace_all(date, '\\.', '-'))
  
  # Aggregate to district-daily mean
  df_long <- df_long %>%
    group_by(c_code_11, date) %>%
    summarize(STATE = first(STATE_UT),
              NAME = first(NAME),
              value = mean(value, na.rm=T))
  
  # Save
  if(weather == 't2m') {
    
    df_long <- rename(df_long, temp = value)
    df_long$temp <- df_long$temp - 273.15
    write.csv(df_long, 
              paste(SAVE.DIR, 'dist_temp.csv', sep=""),
              row.names = F)
  }
  if(weather == 'tp') {
    
    df_long <- rename(df_long, rain = value)
    df_long$rain <- df_long$rain * 1000
    write.csv(df_long, 
              paste(SAVE.DIR, 'dist_rain.csv', sep=""),
              row.names = F)
  }
}