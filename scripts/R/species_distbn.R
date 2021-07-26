# PROJECT: Wildlife and Lockdon
# PURPOSE: Marginal Species Distributions
# AUTHOR: Raahil Madhok
# DATE: Oct 24 2020

# Settings
rm(list=ls())
require(tidyverse)
require(fastDummies)
require(data.table)
require(stargazer)
require(ggsci)
source('/Users/rmadhok/Dropbox/ebird_lockdown/scripts/R/select_functions.R')
setwd('/Users/rmadhok/Dropbox/ebird_lockdown/')

#--------------------------------------------
# PREPARE DATA
#--------------------------------------------

# Read full data (2018-2020)
ebird_full <- readRDS('./data/ebd_full.rds')

# Select 2019/2020 DD Sample
sample <- did(ebird_full, num_cities=20) %>%
  filter(protocol == 'Stationary') %>%
  mutate(county = replace(county, str_detect(county, 'Delhi'), 'Delhi'))

# Cities with data in all four DD groups
cities <- sample %>%
  group_by(county, year, prepost) %>%
  mutate(n_trips = n_distinct(trip_id)) %>%
  summarize(mean =  mean(s_richness, na.rm=T),
            n_trips = first(n_trips)) %>%
  filter(n_trips >= 2) %>% # Must be more than 2 trips in a pre/post period
  group_by(county) %>%
  mutate(n = n()) %>%
  filter(n == 4) %>%
  distinct(county) 
sample <- merge(sample, cities, by = 'county') # trip-id's in selected cities

# Make data.table for big-data merge
ebird_full <- setDT(ebird_full)
sample <- setDT(sample)
setkey(ebird_full, trip_id)
setkey(sample, trip_id)

# Extract checklist for each trip in DD sample
sp_list <- ebird_full[sample[, c('trip_id', 'prepost', 'dif')], nomatch=0] # Merge
sp_list <- sp_list %>% 
  mutate(county = replace(county, stringr::str_detect(county, 'Delhi'), 'Delhi'))

#--------------------------------------------
# SPECIES DISTRIBUTIONS
#--------------------------------------------

# Number of checklists reporting each species
citylist <- cities$county
sp_dist <- list()
for(i in 1:length(citylist)){
  sp <- sp_list %>%
    filter(county == citylist[i]) %>%
    fastDummies::dummy_cols(select_columns='common_name') %>%
    group_by(county, year, dif) %>%
    summarize_at(vars(starts_with('common_name_')), sum, na.rm=T) %>%
    pivot_longer(starts_with('common_name_'), 
                 names_to = 'species', 
                 values_to = 'number', 
                 names_prefix='common_name_')
  sp_dist[[i]] <- sp
}
sp_dist <- dplyr::bind_rows(sp_dist)

# Merge w num trips/day 
n_trips <- sample %>%
  group_by(county, year, dif) %>%
  summarize(n_trips = n(), 
            year = first(year),
            prepost = first(prepost))
sp_dist <- left_join(sp_dist, n_trips, by=c('county', 'year', 'dif'))
sp_dist$prop = sp_dist$number / sp_dist$n_trips # proportion of daily checklists reporting species

# ----------------------------------------------------
# 2020 MARGINAL SPECIES (MAIN TEXT)
# ----------------------------------------------------

# Select marginal speices
m_sp <- marginal_species(sp_dist)

# Get daily distribution of marginal species
sp_dist_m_sp <- merge(sp_dist, m_sp, by = c('county', 'species'))
sp_dist_m_sp <- arrange(sp_dist_m_sp, county, species, year, dif)

# Selected species
sp_blr <- c('Black-rumped Flameback', 'Black-crowned Night-Heron', 'Black-headed Ibis', 'Indian Scops-Owl')
sp_delhi <- c('Black-rumped Flameback', 'Alexandrine Parakeet', 'Large-billed Crow', 'Coppersmith Barbet')
sp_bl <- sp_dist_m_sp %>%
  filter(county == 'Bangalore' & species %in% sp_blr) %>%
  mutate(species = factor(species, levels=sp_blr))
sp_dl <- sp_dist_m_sp %>%
  filter(county == 'Delhi' & species %in% sp_delhi) %>%
  mutate(species = factor(species, levels=sp_delhi))
sp_main <- rbind(sp_bl, sp_dl)
rm(list=c('sp_bl', 'sp_dl'))

# PLOT
ggplot(sp_main, 
       aes(x=as.numeric(dif), y=prop, group=as.character(year))) +
  geom_line(aes(color=as.character(year)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists\n',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=16),
        axis.title = element_text(size=20),
        strip.text = element_text(size=15),
        panel.spacing = unit(2, "lines")) +
  facet_wrap(~county+species, scales='free', ncol=4)
ggsave('./figs/species_dist_main.png', height=5,width=15, unit='in')

# ------- APPENDIX ------- #
sp_bl <- sp_dist_m_sp %>%
  filter(county == 'Bangalore' & !(species %in% sp_blr) & species!='bird sp.')
sp_dl <- sp_dist_m_sp %>%
  filter(county == 'Delhi' & !(species %in% sp_delhi))
#sp_apx <- rbind(sp_bl, sp_dl)
#rm(list=c('sp_bl', 'sp_dl'))

# Bangalore
ggplot(sp_bl, 
       aes(x=as.numeric(dif), y=prop, group=as.character(year))) +
  geom_line(aes(color=as.character(year)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists\n',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=20),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=40),
        legend.title = element_text(size=40),
        legend.text = element_text(size=40),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(30,30,30,30),
        strip.text = element_text(size=19),
        panel.spacing = unit(2.5, "lines")) +
  facet_wrap(~species, scales='free', ncol=4)
ggsave('./figs/species_dist_apx_blr.pdf', height=20,width=20, unit='in')

# Delhi
ggplot(sp_dl, 
       aes(x=as.numeric(dif), y=prop, group=as.character(year))) +
  geom_line(aes(color=as.character(year)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists\n',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=20),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=40),
        legend.title = element_text(size=40),
        legend.text = element_text(size=40),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(30,30,30,30),
        strip.text = element_text(size=17),
        panel.spacing = unit(2.5, "lines")) +
  facet_wrap(~species, scales='free', ncol=4)
ggsave('./figs/species_dist_apx_dl.pdf', height=20,width=20, unit='in')

#------------------------------------
# MARGINAL SPECIES RARITY
#------------------------------------
rm(list=c('sp_main', 'sp_apx', 'sp'))

# % of Checklists reporting each marginal species by city-year
citylist <- c('Bangalore', 'Delhi')
sp_dist_cy <- list()
for(i in 1:length(citylist)){
  sp <- sp_list %>%
    filter(county == citylist[i]) %>%
    fastDummies::dummy_cols(select_columns='common_name') %>%
    group_by(county, year) %>%
    summarize_at(vars(starts_with('common_name_')), sum, na.rm=T) %>% # daily num. of checklists observing species
    pivot_longer(starts_with('common_name_'), 
                 names_to = 'species', 
                 values_to = 'number', 
                 names_prefix='common_name_')
  sp_dist_cy[[i]] <- sp
}
sp_dist_cy <- dplyr::bind_rows(sp_dist_cy)

# Merge w num trips/year 
n_trips <- sample %>%
  group_by(county, year) %>%
  summarize(n_trips = n())
sp_dist_cy <- merge(sp_dist_cy, n_trips, by=c('county', 'year'))

# Marginal Species
m_stats <- sp_dist_cy %>%
  mutate(prop = round((number/n_trips)*100, 2)) %>% # Prop of checklists reporting species
  inner_join(m_sp, by=c('county', 'species')) %>%
  filter(year == 2019 & species!='bird sp.') %>%
  select('county', 'species', 'prop') %>%
  arrange(county, desc(prop)) %>%
  mutate(sp_clean = tolower(species),
         sp_clean = gsub('-', '', sp_clean))

# Merge IUCN Category
iucn <- read.csv('./data/india_birdlife.csv') %>%
  rename(species = English.name, iucn = Global.IUCN.Red.List.Category) %>%
  select(species, iucn) %>%
  mutate(sp_clean = tolower(species),
         sp_clean = gsub('-', '', sp_clean),
         iucn = as.character(iucn))

m_iucn <- left_join(m_stats, iucn[, c('sp_clean', 'iucn')], by='sp_clean') %>%
  select(-sp_clean) %>%
  mutate(iucn = replace(iucn, species=='Common/Jungle Myna', 'LC'),
         iucn = replace(iucn, species=='Gray Heron', 'LC'),
         iucn = replace(iucn, species=='Little/Indian Cormorant', 'LC'),
         iucn = replace(iucn, species=='Blue-throated Flycatcher', 'LC'),
         iucn = replace(iucn, species=='Black/Ashy Drongo', 'LC'),
         iucn = replace(iucn, species=='Rock Pigeon', 'LC'),
         iucn = replace(iucn, species=='Asian Koel', 'LC'),
         iucn = replace(iucn, species=='Indian Gray Hornbill', 'LC'),
         iucn = replace(iucn, species=='White-throated Kingfisher', 'LC'),
         iucn = replace(iucn, species=='Brown Rock Chat', 'LC'),
         iucn = replace(iucn, species=='Large Gray Babbler', 'LC')) %>%
  group_by(county) %>%
  mutate(Rarity = ifelse(prop < quantile(prop, 0.25), 'rare', 'common'))

# Write
stargazer(m_iucn[m_iucn$county=='Bangalore',], summary=F, rownames=F, digits=2, out= './tables/m_sp_blr.tex')
stargazer(m_iucn[m_iucn$county=='Delhi',], summary=F, rownames=F, digits=2, out= './tables/m_sp_dl.tex')

# ----------------------------------------------------
# 2019 MARGINAL SPECIES (APPENDIX)
# ----------------------------------------------------

# Select marginal speices
m_sp <- marginal_species(sp_dist, yr=2019, ref_yr=2020) # very few

# Get daily distribution of marginal species
sp_dist_m_sp <- merge(sp_dist, m_sp, by = c('county', 'species'))
sp_dist_m_sp <- arrange(sp_dist_m_sp, county, species, year, dif)

# PLOT
ggplot(filter(sp_dist_m_sp, county %in% c('Bangalore', 'Delhi')), 
       aes(x=as.numeric(dif), y=prop, group=as.character(year))) +
  geom_line(aes(color=as.character(year)), size=1.2) + 
  geom_vline(xintercept=0, linetype='dashed') +
  labs(y='% of Checklists\n',
       x='\nDays since 4th Wednesday in March', 
       color = 'Year') +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  scale_color_d3() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
              axis.line = element_blank(),
              text = element_text(size=18),
              axis.title = element_text(size=20),
              strip.text = element_text(size=15),
              panel.spacing = unit(2, "lines")) +
  facet_wrap(~county+species, scales='free', ncol=3)
ggsave('./figs/species_dist_ref2019.png', height=6,width=15, unit='in')

# 2019 Marginal Species Frequency
m_stats <- sp_dist_cy %>%
  mutate(prop = round((number/n_trips)*100, 2)) %>% # Prop of checklists reporting species
  inner_join(m_sp, by=c('county', 'species')) %>%
  filter(year == 2019) %>%
  select('county', 'species', 'prop') %>%
  arrange(county, desc(prop)) %>%
  mutate(sp_clean = tolower(species),
         sp_clean = gsub('-', '', sp_clean))

m_iucn <- left_join(m_stats, iucn[, c('sp_clean', 'iucn')], by='sp_clean') %>%
  select(-sp_clean)

# Write
stargazer(m_iucn, summary=F, rownames=F, digits=2, out= './tables/m_sp_ref2019.tex')
