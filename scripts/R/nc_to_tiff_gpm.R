# PROJECT: Nature in Lockdown
# PURPOSE: Convert GPM netcdf to tiff files
# Data source: GPM 
# AUTHOR: Raahil Madhok
# DATE: Jun 2 2020 [created]

# Directories
rm(list=ls())
DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/weather/'
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

# List NetCDFs
setwd(DIR)
files <- list.files('./gpm_nc', full.names=T)

# Iterate
for(file in files){
  
  # read netcdf as raster
  r <- raster(file, varname='precipitationCal')
  date <- getZ(r)
  
  print(paste('Processing: ', date, sep=''))
  
  # Transpose (raw raster is rotated)
  r <- t(r)
  
  # save as geotiff
  names(r) <- 'rain'
  writeRaster(r, 
              paste('./gpm_tif/GPM-', date, sep=''),
              format='GTiff',
              overwrite=T)
}