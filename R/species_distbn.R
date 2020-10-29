# PROJECT: Wildlife and Lockdon
# PURPOSE: Marginal Species Distributions
# AUTHOR: Raahil Madhok
# DATE: Oct 24 2020

# Settings
rm(list=ls())
require(tidyverse)
require(fastDummies)
require(data.table)
source('/Users/rmadhok/Dropbox (Personal)/ebird_lockdown/scripts/R/select_functions.R')
setwd('/Users/rmadhok/Dropbox (Personal)/ebird_lockdown/')

# Read full 2019/2020 data
ebird_full <- readRDS('./data/ebd_full.rds')

# Select DD Sample, keep stationary trips
sample <- did(ebird_full, num_cities=20) %>%
  filter(PROTOCOL.TYPE == 'Stationary') %>%
  mutate(COUNTY = replace(COUNTY, stringr::str_detect(COUNTY, 'Delhi'), 'Delhi'))

# Cities with data in all four DD groups
cities <- sample %>%
  group_by(COUNTY, YEAR, prepost) %>%
  mutate(n_trips = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  summarize(mean =  mean(s_richness, na.rm=T),
            n_trips = first(n_trips)) %>%
  filter(n_trips >= 2) %>% # Must be more than 2 trips in a pre/post period
  group_by(COUNTY) %>%
  mutate(n = n()) %>%
  filter(n == 4) %>%
  distinct(COUNTY)
sample <- merge(sample, cities, by = 'COUNTY')

# Make data.table for big-data merge
ebird_full <- setDT(ebird_full)
sample <- setDT(sample)
setkey(ebird_full, SAMPLING.EVENT.IDENTIFIER)
setkey(sample, SAMPLING.EVENT.IDENTIFIER)

# Extract checklist for each trip in DD sample
sp_list <- ebird_full[sample[, c('SAMPLING.EVENT.IDENTIFIER', 'prepost', 'dif')], nomatch=0] # Merge
sp_list <- sp_list %>% 
  mutate(COUNTY = replace(COUNTY, stringr::str_detect(COUNTY, 'Delhi'), 'Delhi')) %>%
  select(-c('geometry'))

# Species Distribution by City
citylist <- cities$COUNTY
sp_dist <- list()
for(i in 1:length(citylist)){
  sp <- sp_list %>%
    filter(COUNTY == citylist[i]) %>%
    fastDummies::dummy_cols(select_columns='COMMON.NAME') %>%
    group_by(COUNTY, YEAR, dif) %>%
    summarize_at(vars(starts_with('COMMON.NAME_')), sum, na.rm=T) %>% # daily num. of checklists observing species
    pivot_longer(starts_with('COMMON.NAME_'), 
                 names_to = 'species', 
                 values_to = 'number', 
                 names_prefix='COMMON.NAME_')
  sp_dist[[i]] <- sp
}
sp_dist <- dplyr::bind_rows(sp_dist)

# Merge w num trips/day 
n_trips <- sample %>%
  group_by(COUNTY, YEAR, dif) %>%
  summarize(n_trips = n(), 
            YEAR = first(YEAR),
            prepost = first(prepost))
sp_dist <- merge(sp_dist, n_trips, by=c('COUNTY', 'YEAR', 'dif'))
sp_dist$prop = sp_dist$number / sp_dist$n_trips # proportion of daily checklists reporting species

# Select Marginal Species
m_sp <- sp_dist %>%
  filter(n_trips > 1) %>% # drop if daily frequency is from 1 checklist
  mutate(seen = ifelse(prop > 0, 1, 0)) %>%
  group_by(COUNTY, species, YEAR, prepost) %>%
  mutate(n_days = sum(seen)) %>% # number of days per period (pre/post) species observed
  summarize(freq = mean(prop, na.rm=T),
            n_days = first(n_days)) %>%
  group_by(COUNTY, species) %>%
  mutate(keep = ifelse(YEAR == 2020 & prepost=='POST' & n_days >= 7, 1,0), # dummy if species observed > twice post-lockdown 
         keep = max(keep)) %>%
  filter(keep == 1) %>% select(-c(keep, n_days)) %>% # keep if species observed > twice post-lockdown
  group_by(COUNTY, species, YEAR, prepost) %>%
  pivot_wider(names_from = c('YEAR', 'prepost'), values_from = freq) %>%
  filter((`2020_POST` > `2019_POST`) & (`2020_PRE` <= `2019_PRE`)) # keep if species observed more in 2020 vs. 2019

# Get daily distribution of marginal species
sp_dist <- merge(sp_dist, m_sp, by = c('COUNTY', 'species'))
sp_dist <- arrange(sp_dist, COUNTY, species, YEAR, dif)

# MAIN TEXT ----------------------------------
sp_bangalore <- c('Black-rumped Flameback', 'Black-crowned Night-Heron', 'Black-headed Ibis', 'Indian Scops-Owl')
sp_delhi <- c('Black-rumped Flameback', 'Greater Coucal', 'Large-billed Crow', 'Coppersmith Barbet')
sp_bl <- sp_dist %>%
  filter(COUNTY == 'Bangalore' & species %in% sp_bangalore) %>%
  mutate(species = factor(species, levels=sp_bangalore))
sp_dl <- sp_dist %>%
  filter(COUNTY == 'Delhi' & species %in% sp_delhi) %>%
  mutate(species = factor(species, levels=sp_delhi))
sp_main <- rbind(sp_bl, sp_dl)
rm(list=c('sp_bl', 'sp_dl'))

# PLOT
require('ggsci')
ggplot(sp_main, 
       aes(x=as.numeric(dif), y=prop, group=as.character(YEAR))) +
  geom_line(aes(color=as.character(YEAR)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=17),
        strip.text = element_text(size=15)) +
  facet_wrap(~COUNTY+species, scales='free', ncol=4)
ggsave('./figs/species_dist_main.png', height=5,width=15, unit='in')

# APPENDIX  ----------------------------------
sp_bl <- sp_dist %>%
  filter(COUNTY == 'Bangalore' & !(species %in% sp_bangalore) & species!='bird sp.')
sp_dl <- sp_dist %>%
  filter(COUNTY == 'Delhi' & !(species %in% sp_delhi))
sp_apx <- rbind(sp_bl, sp_dl)
rm(list=c('sp_bl', 'sp_dl'))

ggplot(sp_apx, 
       aes(x=as.numeric(dif), y=prop, group=as.character(YEAR))) +
  geom_line(aes(color=as.character(YEAR)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=17),
        strip.text = element_text(size=15)) +
  facet_wrap(~COUNTY+species, scales='free', ncol=4)
ggsave('./figs/species_dist_apx.pdf', height=25,width=20, unit='in')