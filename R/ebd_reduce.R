# PROJECT: Nature in Lockdown
# PURPOSE: Reduce dataset size for easier reading
# AUTHOR: Raahil Madhok
# DATE: May 21 2020 [created]

# Directories
rm(list=ls())
READ.DIR <- '/Volumes/Backup Plus/research/ebird_lockdown/'
SAVE.DIR <- '/Users/rmadhok/Dropbox (Personal)/covid/ebird_lockdown/data/'
setwd(READ.DIR)

# Load Packages
require(data.table)
require(tidyverse)
require(lubridate)

# Read (n=6,645,928) ~2 mins to read 
ebird <- fread('ebd_IN_201901_202004_relApr-2020.txt')
colnames(ebird) <- make.names(colnames(ebird))

# Format Time
ebird$OBSERVATION.DATE <- ymd(ebird$OBSERVATION.DATE)
ebird$YEARMONTH <- format(ebird$OBSERVATION.DATE, "%Y-%m")
ebird$YEAR <- year(ebird$OBSERVATION.DATE)
ebird$MONTH <- month(ebird$OBSERVATION.DATE)
ebird$HOUR <- as.numeric(substr(ebird$TIME.OBSERVATIONS.STARTED, 1, 2))

# Number of Trips before March 2020 (n=9,229 users)
ebird_pre <- ebird %>%
  filter(OBSERVATION.DATE < as.Date('2020-03-01')) %>%
  group_by(OBSERVER.ID) %>%
  summarize(n_trips_pld = n_distinct(SAMPLING.EVENT.IDENTIFIER))
ebird <- merge(ebird, ebird_pre, by='OBSERVER.ID', all.x = T)

# Window: 24 days before and after Mar 24 (n=1,586,974)
ebird <- ebird %>%
  filter(YEARMONTH %in% c('2019-03', '2019-04',
                          '2020-03', '2020-04'))

# Save (rds smallest filesize)
saveRDS(ebird, paste(SAVE.DIR, 'ebd_lockdown_raw.rds', sep=''))
