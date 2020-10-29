# PROJECT: Nature in Lockdown
# PURPOSE: Construct final data
# AUTHOR: Raahil Madhok
# DATE: May 21 2020 [created]

# Directories
rm(list=ls())
DIR <- '/Users/rmadhok/Dropbox (Personal)/ebird_lockdown'
SHP <- '/Users/rmadhok/Dropbox (Personal)/IndiaPowerPlant/data/'
setwd(DIR)

# Load Packages
require(tidyverse)
require(sf)
require(lubridate)
require(stargazer)

# 1. Read data -------------------------------

# Load (n=1,586,974)
ebird <- as.data.frame(readRDS('./data/ebd_lockdown_raw.rds'))

# district shapefile
india_districts <- st_read(paste(SHP, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', 
                           stringsAsFactors = F) %>%
  select('c_code_11', 'TOT_POP', 'TOT_AREA', 'NAME', 'geometry') %>%
  mutate(POP_DENSITY = TOT_POP / TOT_AREA)

# 2. Filter eBird -------------------------------

# Tabulate protocols for Supp. Materials
protocol <- ebird %>%
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  group_by(PROTOCOL.TYPE) %>%
  summarize(`Num. Trips` = n()) %>%
  mutate(Pct. = round((`Num. Trips`/sum(`Num. Trips`)*100),2)) %>%
  rename(Protocol = PROTOCOL.TYPE) %>%
  arrange(desc(Pct.))
pcol_stat <- stargazer(protocol, summary=F, rownames=F, out= './tables/protocol.tex')

#long <- ebird %>%
#  mutate(long = if_else(DURATION.MINUTES>=240,1, 0)) %>%
#  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all=T) %>%
#  group_by(long) %>%
#  summarize(n=n()) %>%
#  mutate(freq = n/sum(n))

all_rep <- ebird %>%
  distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
  group_by(ALL.SPECIES.REPORTED) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

# a. Stationary trips 
ebd_use <- filter(ebird, PROTOCOL.TYPE == 'Stationary' | PROTOCOL.TYPE == 'Traveling') # n=1,542,603

# b. Complete checklists (# n = 1,508,524)
ebd_use <- filter(ebd_use, ALL.SPECIES.REPORTED == 1)

# c. Drop Duplicates from group trips (n=1,167,214)
ebd_use$GROUP.IDENTIFIER[ebd_use$GROUP.IDENTIFIER == ''] <- NA
ebd_m <- ebd_use %>% filter(is.na(GROUP.IDENTIFIER))
ebd_nm <- ebd_use %>% filter(!is.na(GROUP.IDENTIFIER))
ebd_dd <- distinct(ebd_nm, GROUP.IDENTIFIER, TAXONOMIC.ORDER, .keep_all = T)
ebd_use <- rbind(ebd_m, ebd_dd)
rm(list=c('ebd_dd', 'ebd_m', 'ebd_nm'))

# d. Duration b/w 5-240 mins (see Callaghan et al. 2019) n=1,083,950
ebd_use <- filter(ebd_use, DURATION.MINUTES >= 5 & DURATION.MINUTES <= 240)

# 3. Species Diversity -------------------------------

#ebd_use$OBSERVATION.COUNT[ebd_use$OBSERVATION.COUNT == 'X'] <- NA
#ebd_use$OBSERVATION.COUNT <- as.numeric(ebd_use$OBSERVATION.COUNT)

# Species diveristy per trip
ebd_use <- ebd_use %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  mutate(s_richness = n())

# e. Drop if checklist has 1 bird (see Walker & Tayler, 2017) n=1,081,728
ebd_use <- as.data.frame(filter(ebd_use, s_richness > 1))

# Keep selected columns
ebd_use <- select(ebd_use, 'TAXONOMIC.ORDER', 'COMMON.NAME', 'SCIENTIFIC.NAME', 
                  'OBSERVATION.COUNT', 'STATE', 'COUNTY', 'LOCALITY', 'LOCALITY.TYPE', 
                  'LATITUDE', 'LONGITUDE', 'OBSERVATION.DATE', 'TIME.OBSERVATIONS.STARTED', 
                  'OBSERVER.ID', 'SAMPLING.EVENT.IDENTIFIER', 'DURATION.MINUTES', 
                  'NUMBER.OBSERVERS', 'GROUP.IDENTIFIER', 'YEARMONTH', 'YEAR', 'HOUR',
                  'PROTOCOL.TYPE','n_trips_pld', 's_richness')

# Week Dummy
ebd_use <- ebd_use %>%
  mutate(month = format(OBSERVATION.DATE, '%B'),
         dow = weekdays(OBSERVATION.DATE),
         weekend = as.numeric(dow %in% c('Saturday', 'Sunday')),
         week = isoweek(OBSERVATION.DATE),
         day_week = paste(month, ', ', dow, ', week ', week, sep='')) 

# 2. Overlay District -------------------------

# Spatial merge census code and population
ebd_use <- st_join(st_as_sf(ebd_use, 
                            coords = c('LONGITUDE', 'LATITUDE'), 
                            crs = 4326), 
                   india_districts, join = st_intersects) %>% as.data.frame()

# drop sightings outside map (n=1,080,034)
ebd_use <- filter(ebd_use, !is.na(c_code_11))

# Missing District names
ebd_use$COUNTY[ebd_use$COUNTY == ""] <- ebd_use$NAME[ebd_use$COUNTY == ""]

# 3. Distance to nearest hotspot -------------------------

# Convert to trip-level sf
ebd_trip <- distinct(ebd_use, SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    select(SAMPLING.EVENT.IDENTIFIER, geometry)
ebd_trip <- st_as_sf(ebd_trip, crs = 4326)

# Read hotspot data
hotspots <- read.csv('./data/hotspots.csv', header = F)
hotspots <- st_as_sf(hotspots, coords = c('V6', 'V5'), crs = 4326)

#3. Distance to Nearest Hotspot
idx <- st_nearest_feature(ebd_trip, hotspots) # index of nearest hotspot
hotspots <- hotspots %>% slice(idx) # line up indices
ebd_trip$hotspot_km <- st_distance(ebd_trip, hotspots, by_element = T) / 1000 #20-30 mins
ebd_trip <- ebd_trip %>%
  as.data.frame() %>%
  select(SAMPLING.EVENT.IDENTIFIER, hotspot_km) %>%
  mutate(hotspot_km = as.numeric(hotspot_km))
ebd_use <- merge(ebd_use, ebd_trip, by = 'SAMPLING.EVENT.IDENTIFIER')

# Add rain
rain <- read.csv('./data/india_rain_gpm.csv') %>%
  dplyr::select('c_code_11', 'date', 'rain') %>%
  rename(OBSERVATION.DATE = date)
ebd_use <- merge(ebd_use, rain, 
               by = c('c_code_11', 'OBSERVATION.DATE'), 
               all.x = T)

# Add temperature
temperature <- read.csv('./data/india_temperature_era.csv') %>%
  rename(OBSERVATION.DATE = date)
ebd_use <- merge(ebd_use, temperature, 
                 by = c('c_code_11', 'OBSERVATION.DATE'), 
                 all.x = T)

# Save
saveRDS(ebd_use, './data/ebd_full.rds')
