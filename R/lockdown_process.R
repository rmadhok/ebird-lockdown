# PROJECT: Nature in Lockdown
# PURPOSE: Construct final data
# AUTHOR: Raahil Madhok
# DATE: May 21 2020 [created]

# Directories
rm(list=ls())
DIR <- '/Users/rmadhok/Dropbox (Personal)/covid/ebird_lockdown/data'
SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'
setwd(DIR)

# Load Packages
require(data.table)
require(tidyverse)
require(sf)

# 1. Read data -------------------------------

# Load (n=1,586,974)
ebird <- readRDS('ebd_lockdown_raw.rds')

# district shapefile
india_districts <- st_read(paste(SHP, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', 
                           stringsAsFactors = F) %>%
  select('c_code_11', 'TOT_POP', 'TOT_AREA', 'NAME', 'geometry') %>%
  mutate(POP_DENSITY = TOT_POP / TOT_AREA) %>%
  rename(DIST_NAME = NAME)

# 2. Filter eBird -------------------------------

# a. Stationary trips 
ebd_use <- filter(ebird, PROTOCOL.TYPE == 'Stationary') # n=572,605

# b. Complete checklists (# n = 564,547)
ebd_use <- filter(ebd_use, ALL.SPECIES.REPORTED == 1)

# c. Drop Duplicates from group trips (n=499,915)
ebd_use$GROUP.IDENTIFIER[ebd_use$GROUP.IDENTIFIER == ''] <- NA
ebd_m <- ebd_use %>% filter(is.na(GROUP.IDENTIFIER))
ebd_nm <- ebd_use %>% filter(!is.na(GROUP.IDENTIFIER))
ebd_dd <- distinct(ebd_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebd_use <- rbind(ebd_m, ebd_dd)
rm(list=c('ebd_dd', 'ebd_m', 'ebd_nm'))

# d. Duration b/w 5-240 mins (see Callaghan et al. 2019) n=483,426
ebd_use <- filter(ebd_use, DURATION.MINUTES >= 5 & DURATION.MINUTES <= 240)

# 3. Species Diversity -------------------------------

#ebd_use$OBSERVATION.COUNT[ebd_use$OBSERVATION.COUNT == 'X'] <- NA
#ebd_use$OBSERVATION.COUNT <- as.numeric(ebd_use$OBSERVATION.COUNT)

# Species diveristy per trip (can add shannon and simpson)
# e. Drop if checklist has 1 bird (see Walker & Tayler, 2017) n=482,047
ebd_use <- ebd_use %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(s_richness = n()) %>%
  filter(s_richness > 1)

# Keep selected columns
ebd_use <- select(ebd_use, 'TAXONOMIC.ORDER', 'COMMON.NAME', 'SCIENTIFIC.NAME', 
                  'OBSERVATION.COUNT', 'STATE', 'COUNTY', 'LOCALITY', 'LOCALITY.TYPE', 
                  'LATITUDE', 'LONGITUDE', 'OBSERVATION.DATE', 'TIME.OBSERVATIONS.STARTED', 
                  'OBSERVER.ID', 'SAMPLING.EVENT.IDENTIFIER', 'DURATION.MINUTES', 
                  'NUMBER.OBSERVERS', 'GROUP.IDENTIFIER', 'YEARMONTH', 'YEAR', 'HOUR',
                  'n_trips_pld', 's_richness')

# 2. Overlay District -------------------------

# Spatial merge census code and population
ebd_use <- as.data.frame(st_join(st_as_sf(ebd_use, 
                                        coords = c('LONGITUDE', 'LATITUDE'), 
                                        crs = 4326), 
                               india_districts, 
                               join = st_intersects))

# drop sightings outside map (n=481,772)
ebd_use <- filter(ebd_use, !is.na(c_code_11))

# Missing District names
ebd_use$COUNTY[ebd_use$COUNTY == ""] <- ebd_use$DIST_NAME[ebd_use$COUNTY == ""]

# Save
saveRDS(ebd_use, 'ebd_full.rds')
