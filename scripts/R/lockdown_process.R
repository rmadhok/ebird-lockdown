# PROJECT: Nature in Lockdown
# PURPOSE: Construct final data
# AUTHOR: Raahil Madhok
# DATE: May 21 2020 [created]

# Directories
rm(list=ls())
DIR1 <- '/Users/rmadhok/Dropbox/ebird_lockdown/data/'
DIR2 <- '/Volumes/Backup Plus/research/ebird_lockdown/ebird/'
SHP <- '/Users/rmadhok/Dropbox/IndiaPowerPlant/data/'

# Load Packages
require(tidyverse)
require(sf)
require(lubridate)
require(stargazer)

#-----------------------------------------------------
# LOAD DATA
#-----------------------------------------------------

# Read
setwd(DIR2)
ebird <- as.data.frame(readRDS('ebd_lockdown_raw.rds'))
ebird <- filter(ebird, year==2020 | year==2019) # only for summary stats

# India Map
india_dist <- st_read(paste(SHP, "maps/india-district", sep=""), 
                           'SDE_DATA_IN_F7DSTRBND_2011', 
                           stringsAsFactors = F) %>%
  mutate(pop_density = TOT_POP/TOT_AREA) %>%
  select('c_code_11', 'pop_density', 'NAME')

# Tabulate protocols for Supp. Materials
protocol <- ebird %>%
  distinct(sampling_event_identifier, .keep_all = T) %>%
  group_by(protocol_type) %>%
  summarize(`Num. Trips` = n()) %>%
  mutate(Pct. = round((`Num. Trips`/sum(`Num. Trips`)*100),2)) %>%
  rename(Protocol = protocol_type) %>%
  arrange(desc(Pct.))
stargazer(protocol, summary=F, rownames=F, out= '/Users/rmadhok/Dropbox/ebird_lockdown/tables/protocol.tex')

all_rep <- ebird %>%
  distinct(sampling_event_identifier, .keep_all = T) %>%
  group_by(all_species_reported) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))
rm(list=c('protocol', 'all_rep'))

#-----------------------------------------------------
# FILTER
#-----------------------------------------------------

# Stationary trips 
ebd_use <- filter(ebird, protocol_type %in% c('Stationary','Traveling')) # n=2,246,935

# Complete checklists (# n = 2,192,647)
ebd_use <- filter(ebd_use, all_species_reported == 1)

# Drop Duplicates from group trips (n=1,658,606)
ebd_use$group_identifier[ebd_use$group_identifier == ''] <- NA
ebd_m <- ebd_use %>% filter(is.na(group_identifier))
ebd_nm <- ebd_use %>% filter(!is.na(group_identifier))
ebd_dd <- distinct(ebd_nm, group_identifier, taxonomic_order, .keep_all = T)
ebd_use <- rbind(ebd_m, ebd_dd)
rm(list=c('ebd_dd', 'ebd_m', 'ebd_nm'))

# Duration b/w 5-240 mins (see Callaghan et al. 2019) n=1,523,793
ebd_use <- filter(ebd_use, duration_minutes >= 5 & duration_minutes <= 240)

# Species diveristy per trip
ebd_use <- ebd_use %>% 
  group_by(sampling_event_identifier) %>% 
  mutate(s_richness = n())

# Drop if checklist has 1 bird (see Walker & Tayler, 2017) n=1,517,631
ebd_use <- as.data.frame(filter(ebd_use, s_richness > 1))

#-----------------------------------------------------
# CLEAN UP
#-----------------------------------------------------

# Keep selected columns
ebd_use <- select(ebd_use, 'taxonomic_order', 'common_name', 'scientific_name', 
                  'observation_count', 'state', 'county', 'locality', 'locality_type', 
                  'latitude', 'longitude', 'time_observations_started', 'observer_id', 
                  'sampling_event_identifier', 'duration_minutes', 'number_observers', 
                  'group_identifier', 'yearmonth', 'year', 'hour', 'month', 'date', 
                  'protocol_type', 's_richness')

# Rename
ebd_use <- ebd_use %>%
  rename(count = observation_count,
         time = time_observations_started,
         trip_id = sampling_event_identifier,
         duration = duration_minutes,
         group_id = group_identifier,
         protocol = protocol_type,
         num_observers = number_observers,
         lat = latitude,
         lon = longitude)

# Weekdays/Weekends
ebd_use <- ebd_use %>%
  mutate(dow = weekdays(date),
         weekend = as.numeric(dow %in% c('Saturday', 'Sunday')))
#week = isoweek(date),
#day_week = paste(month, ', ', dow, ', week ', week, sep='')) 

# Overlay Census Codes
ebd_use <- ebd_use %>% mutate(lon2=lon, lat2=lat)
ebd_use <- st_join(st_as_sf(ebd_use, 
                            coords = c('lon2', 'lat2'), 
                            crs = 4326), 
                   india_dist, join = st_intersects)
ebd_use <- st_drop_geometry(ebd_use)

# Dropp trips off coast (n=1,514,964)
ebd_use <- filter(ebd_use, !is.na(c_code_11))

# Missing District names
ebd_use$county[ebd_use$county == ""] <- ebd_use$NAME[ebd_use$county == ""]

#---------------------------------------------------------
# ADD COVARIATES
#---------------------------------------------------------

#---- Distance to Nearest Hotspot ------

# Trip Level (n=113,881)
ebd_trip <- distinct(ebd_use, trip_id, .keep_all = T) %>% 
  select(trip_id, lat, lon) %>% mutate(lon2=lon, lat2=lat)
ebd_trip <- st_as_sf(ebd_trip, coords =c('lon2', 'lat2'), crs = 4326)

# Read hotspot data
setwd(DIR1)
hotspots <- read.csv('hotspots.csv', header = F) %>%
  select(V5, V6, V7) %>% 
  rename(lon_hs = V6, lat_hs=V5, hs_name=V7) %>%
  mutate(lon2_hs=lon_hs, lat2_hs=lat_hs)
hotspots <- st_as_sf(hotspots, coords = c('lon2_hs', 'lat2_hs'), crs = 4326)

# Distance to Nearest Hotspot (20-30 mins)
ebd_trip <- st_join(ebd_trip, hotspots, join = st_nearest_feature)
ebd_trip <- st_drop_geometry(ebd_trip)
ebd_trip$hotspot_km <- st_distance(st_as_sf(ebd_trip, 
                                      coords = c('lon', 'lat'),
                                      crs=4326), 
                                   st_as_sf(ebd_trip, 
                                      coords = c('lon_hs', 'lat_hs'),
                                      crs=4326), 
                                   by_element = T)
ebd_trip$hotspot_km <- as.numeric(ebd_trip$hotspot_km)/1000

# Merge back to species-level
ebd_use <- left_join(ebd_use, ebd_trip[,c('trip_id','hotspot_km')], by='trip_id')

# --------- Weather (2019/2020 only) ---------------------------------
ebd_use <- ebd_use %>%
  left_join(read_csv('india_rain_gpm.csv') %>% # rain
              select('c_code_11', 'date', 'rain'),
            by=c('c_code_11', 'date')) %>%
  left_join(read_csv('india_temperature_era.csv'), # temperature
            by=c('c_code_11', 'date'))

# Save
setwd(DIR1)
saveRDS(ebd_use, 'ebd_full.rds')
