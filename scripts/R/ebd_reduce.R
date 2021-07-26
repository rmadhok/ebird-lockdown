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

#---------------------------------------------------
# PROCESS
#---------------------------------------------------

# Read 2018-2020 (n=13,751,741)
ebird <- fread('ebd_IN_201801_202012_relFeb-2021.txt')
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))

# Format Time
ebird$date <- ymd(ebird$observation_date)
ebird$yearmonth <- format(ebird$date, "%Y-%m")
ebird$year <- year(ebird$date)
ebird$month <- month(ebird$date)
ebird$hour <- as.numeric(substr(ebird$time_observations_started, 1, 2))

# March and April (n=2,323,183)
ebird <- filter(ebird, month == 3 | month == 4)

# Save (rds smallest filesize)
saveRDS(ebird, 'ebd_lockdown_raw.rds')
