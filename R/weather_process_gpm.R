# PROJECT: Nature in Lockdown
# PURPOSE: Process GPM rainfall data
# Data source: GPM 
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
READ.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/gpm_tiff/'
SAVE.DIR <- '/home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/data/'
SHP <- '/home/rmadhok/projects/def-sgulati/rmadhok/IndiaPowerPlant/data/shapefiles/district2011/'
#READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/gpm_tiff/'
#SAVE.DIR <- '/Users/rmadhok/Dropbox (Personal)/covid/ebird_lockdown/data/'
#SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/maps/india-district/'

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
rstack <- stack(list.files())

# Extract mean over district boundaries
print('Extracting mean over district boundaries...')
df <- raster::extract(rstack, india_dist,
                      fun = mean, na.rm = T, sp=T)@data

# Reshape to long panel
print('Reshaping to panel...')
df_long <- df %>% 
  gather(date, rain, starts_with('GPM')) %>%
  select('STATE_UT', 'NAME', 'c_code_11', 'date', 'rain') %>%
  mutate(date = stringr::str_replace(date, 'GPM.', ''),
         date = stringr::str_replace_all(date, '\\.', '-'))

# Save
write.csv(df_long, 
          paste(SAVE.DIR, 'india_rain_gpm.csv', sep=""),
          row.names = F)
