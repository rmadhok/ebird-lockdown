#-------------------------------------------------------------------
# PROJECT: Nature in Lockdown
# PURPOSE: Reduce dataset size for easier reading
# AUTHOR: Raahil Madhok
# DATE: May 21 2020 [created]

# Directories
rm(list=ls())
DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/ebird/'
setwd(DIR)

# Load Packages
require(data.table)
require(tidyverse)
require(lubridate)
#-------------------------------------------------------------------

# Read 2019-2020 (n=6,645,928) ~2 mins to read 
ebird <- fread('ebd_IN_201901_202004_relApr-2020.txt')

# Append 2018
ebird_18 <- fread('/Volumes/Backup Plus/research/def_biodiv/ebird/ebd_IN_201401_201904_relApr-2019.txt')
ebird_18 <- ebird_18 %>% filter(str_detect(`OBSERVATION DATE`, '2018'))  
ebird <- rbind(ebird, ebird_18) # n=10,236,240
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))
rm(list='ebird_18')

# Format Time
ebird$date <- ymd(ebird$observation_date)
ebird$yearmonth <- format(ebird$date, "%Y-%m")
ebird$year <- year(ebird$date)
ebird$month <- month(ebird$date)
ebird$hour <- as.numeric(substr(ebird$time_observations_started, 1, 2))

# Number of Trips before March 2020 (n=9,229 users)
#ebird_pre <- ebird %>%
#  filter(date < as.Date('2020-03-01')) %>%
#  group_by(OBSERVER.ID) %>%
#  summarize(n_trips_pld = n_distinct(SAMPLING.EVENT.IDENTIFIER))
#ebird <- merge(ebird, ebird_pre, by='OBSERVER.ID', all.x = T)

# March and April (n=2,246,677)
ebird <- ebird %>% filter(month == 3 | month == 4)

# Save (rds smallest filesize)
saveRDS(ebird, paste(DIR, 'ebd_lockdown_raw.rds', sep=''))
