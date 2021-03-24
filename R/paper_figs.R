#----------------------------------------------------------------------------
# PROJECT: Wildlife and Lockdon
# PURPOSE: Paper Figures/tables
# AUTHOR: Raahil Madhok
# DATE: June 24 2020 [created]
#----------------------------------------------------------------------------
# Settings
rm(list=ls())
require(tidyverse)
require(sf)
require(kableExtra)
require(modelsummary)
require(stargazer)
require(starpolishr)
require(estimatr)
require(cowplot)
require(fastDummies)
require(data.table)
source('/Users/rmadhok/Dropbox/ebird_lockdown/scripts/R/select_functions.R')
setwd('/Users/rmadhok/Dropbox/ebird_lockdown/')
#----------------------------------------------------------------------------

# Read data (2018-2020)
ebird_full <- readRDS('./data/ebd_full.rds') # Full 
ebird20 <- filter(ebird_full, year == 2020 & date <= '2020-04-17') # 2020 data

#-----------------------------------------------
# FIG 1 : Daily Activity
#-----------------------------------------------
stats <- ebird20 %>%
  group_by(date, protocol) %>%
  summarize(`A. Num. Active Users` = n_distinct(observer_id),
            `B. Num. Trips` = n_distinct(trip_id),
            `C. Num. Unique Species` = n_distinct(taxonomic_order)) %>%
  gather(key, value, contains('Num.'))

ggplot(stats, aes(x=date, y=value, group=protocol)) +
  geom_line(aes(color=protocol), size=1) + 
  labs(y='') +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-25")), linetype='dashed') +
  scale_x_date(date_breaks = '10 days', date_labels = '%b\n%d') +
  scale_colour_viridis_d() +
  labs(color='Trip Protocol') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=11)) +
  facet_wrap(~key, scales='free')
ggsave('./figs/daytrend.png', height=3,width=8)

#-----------------------------------------------
# FIG 2 : Daily Activity by City
#-----------------------------------------------
stats <- ebird20 %>%
  group_by(county, date, protocol) %>%
  summarize(`A. Num. Active Users` = n_distinct(observer_id),
            `B. Num. Trips` = n_distinct(trip_id),
            `C. Num. Unique Species` = n_distinct(taxonomic_order)) %>%
  gather(key, value, contains('Num.')) %>%
  filter(county %in% c('Mumbai', 'Bangalore', 'Chennai', 'Kolkata'))

ggplot(stats, aes(x=date, y=value, group=protocol)) +
  geom_line(aes(color=protocol), size=1) +
  labs(y='') +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-25")), linetype='dashed') +
  scale_x_date(date_breaks = '10 days', date_labels = '%b\n%d') +
  scale_colour_viridis_d() +
  labs(color='Trip Protocol') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=12)) +
  facet_wrap(~county+key, scales='free', ncol=3)
ggsave('./figs/4citytrend.png', height=10,width=8)

#-----------------------------------------------
# TABLE 0 : TOP 20 Cities
#-----------------------------------------------

# All Cities
ebird20.d <- filter(ebird20, date != '2020-03-22')
stats <- ebird20.d %>%
  select_sample(num_cities = 20) %>%
  group_by(county) %>%
  summarize(State = first(state))
stargazer(stats, summary=F, rownames=F, out= './tables/cities.tex')

#-----------------------------------------------
# TABLE 1 : PRE-POST ACTIVITY
#-----------------------------------------------
stats <- ebird20.d %>%
  select_sample() %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After')) %>%
  group_by(prepost) %>%
  summarize(`Num. Trips` = n_distinct(trip_id),
            `Num. Cities` = n_distinct(county),
            `Num. Users` = n_distinct(observer_id),
            `Num. Species` = n_distinct(taxonomic_order)) %>%
  arrange(desc(prepost)) %>%
  pivot_longer(starts_with('N'), names_to = 'Num') %>%
  pivot_wider(names_from = prepost, values_from = value)
panel1 <- stargazer(stats, summary=F, rownames=F)

# Top 20
stats <- ebird20.d %>%
  select_sample(num_cities = 20) %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After')) %>%
  group_by(prepost) %>%
  summarize(`Num. Trips` = n_distinct(trip_id),
            `Num. Cities` = n_distinct(county),
            `Num. Users` = n_distinct(observer_id),
            `Num. Species` = n_distinct(taxonomic_order)) %>%
  arrange(desc(prepost)) %>%
  pivot_longer(starts_with('N'), names_to = 'Num') %>%
  pivot_wider(names_from = prepost, values_from = value)
panel2 <- stargazer(stats, summary=F, rownames=F)
star_panel(panel1, panel2, 
           panel.names = c('All Cities', 'Top 20'), 
           reg = F,
           panel.label.fontface = 'bold') %>%
  star_tex_write(file = './tables/t1_activity.tex')

#-----------------------------------------------
# TABLE 2 : PRE-POST ACTIVITY (TRIP-LEVEL)
#-----------------------------------------------
# All Cities
stats <- ebird20.d %>%
  select_sample() %>%
  distinct(trip_id, .keep_all = T) %>%
  dplyr::select(date, trip_id, duration, s_richness, 
                county, temperature, rain, hotspot_km) %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After')) %>%
  group_by(prepost) %>%
  summarize(`Mean Trip Length` = round(mean(duration, na.rm=T), 2),
            `Mean Species Richness` = round(mean(s_richness, na.rm=T), 2),
            `Median Species Richness` = median(s_richness, na.rm=T),
            `Mean Dist. to Hotspot (km)` = round(mean(hotspot_km, na.rm = T), 2),
            `Mean Rainfall (mm)` = round(mean(rain, na.rm=T), 2),
            `Mean Temperature (Celsius)` = round(mean(temperature, na.rm=T), 2)) %>%
  arrange(desc(prepost)) %>%
  pivot_longer(starts_with('M'), names_to = ' ') %>%
  pivot_wider(names_from = prepost, values_from = value)
panel1 <- stargazer(stats, summary=F, digits=2, rownames=F)

# Top 20
stats <- ebird20.d %>%
  select_sample(num_cities = 20) %>%
  distinct(trip_id, .keep_all = T) %>%
  dplyr::select(date, trip_id, duration, s_richness, 
                county, temperature, rain, hotspot_km) %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After')) %>%
  group_by(prepost) %>%
  summarize(`Mean Trip Length` = round(mean(duration, na.rm=T),2),
            `Mean Species Richness` = round(mean(s_richness, na.rm=T),2),
            `Median Species Richness` = median(s_richness, na.rm=T),
            `Mean Dist. to Hotspot (km)` = round(mean(hotspot_km, na.rm = T), 2),
            `Mean Rainfall (mm)` = round(mean(rain, na.rm=T), 2),
            `Mean Temperature (Celsius)` = round(mean(temperature, na.rm=T), 2)) %>%
  arrange(desc(prepost)) %>%
  pivot_longer(starts_with('M'), names_to = ' ') %>%
  pivot_wider(names_from = prepost, values_from = value)

panel2 <- stargazer(stats, summary=F, rownames=F, digits=2)
star_panel(panel1, panel2, 
           panel.names = c('All Cities', 'Top 20'), 
           reg = F,
           panel.label.fontface = 'bold') %>%
  star_tex_write(file = './tables/t2_activity_trip.tex')

#---------------------------------------------------
# FIGURE 3: SPECIES RICHNESS/TRIP PRE-POST by CITY
#--------------------------------------------------
# By City
stats1 <- ebird20.d %>%
  select_sample(num_cities = 20) %>% 
  mutate(label = 'Stationary + Travelling')
stats2 <- ebird20.d %>%
  filter(protocol == 'Stationary') %>% 
  select_sample(num_cities=20) %>%
  mutate(label = 'Stationary')
stats <- rbind(stats1, stats2)
rm(list=c('stats1','stats2'))

stats <- stats %>%  
  distinct(label, trip_id, .keep_all = T) %>%
  dplyr::select(observer_id, date, trip_id, duration, 
                s_richness, county, label) %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After')) %>%
  group_by(label, county, prepost) %>%
  summarize(mean = mean(s_richness, na.rm=T),
            sd = sd(s_richness, na.rm=T),
            n = n(),
            se = sd/sqrt(n)) %>%
  filter(county %in% c('Bangalore', 'Chennai', 'Kolkata', 'Mumbai'))

ggplot(stats, aes(x=label, 
                  y=mean, 
                  fill=prepost)) + 
  geom_bar(stat = 'identity', position='dodge') + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(padding = 0.8)) +
  labs(y='Mean Species Richness Per Trip',
       fill='') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.title.y = element_text(size=17),
        legend.text=element_text(size=17),
        axis.line = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12)) +
  facet_wrap(~county, scales = 'free')
ggsave('./figs/srtrip_4cities.png', height=6,width=8)

#---------------------------------------------------
# FIGURE 4: HOUR HISTOGRAM
#--------------------------------------------------
stats <- ebird20.d %>%
  select_sample(num_cities = 20) %>%
  distinct(trip_id, .keep_all = T) %>%
  dplyr::select(date, trip_id, duration, hour) %>%
  mutate(prepost = if_else(date <= '2020-03-24', 'Before', 'After'))

ggplot(data = stats, 
       aes(x = hour, y = ..prop.., 
           fill=prepost)) +
  geom_bar(alpha=0.5, position='identity') +
  labs(x='\nHour of Day', y='% of Trips\n', fill = "") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=13),
        axis.line = element_blank(), 
        axis.ticks = element_blank())
ggsave('./figs/hour_distbn.png', height=6,width=8)

#-----------------------------------------------
# FIG 5 : DIFF IN DIFF GRAPH - 2019/2020
#-----------------------------------------------
ebird_1920 <- filter(ebird_full, year %in% c(2019, 2020))
sample <- did(ebird_1920, drop=F)
dd_type <- sample %>%
  group_by(dif, year, protocol) %>%
  summarize(`Num. Species/Trip` = mean(s_richness, na.rm=T),
            `B. Num. of Trips` = n_distinct(trip_id),
            `A. Num. Active Users` = n_distinct(observer_id)) %>%
  gather(var, value, contains('Num.')) %>%
  pivot_wider(names_from = c(year, protocol), values_from=value) %>%
  mutate(Stationary = `2020_Stationary` - `2019_Stationary`,
         Travelling = `2020_Traveling` - `2019_Traveling`) %>%
  dplyr::select(dif, var, Stationary, Travelling) %>%
  gather(key, value, c('Stationary', 'Travelling'))

# DD - Species Richness
ggplot(dd_type[dd_type$var == 'Num. Species/Trip',], aes(x=(dif), y=value, group=key)) +
  geom_line(aes(color=key), size=1.5) + 
  labs(y='',
       x='Days since 4th Wednesday in March', color = 'Trip Protocol') +
  geom_vline(xintercept=0, linetype='dashed') +
  geom_hline(yintercept=0, linetype='dashed') +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0','7', '14', '21')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=20))
ggsave('./figs/dd_sr_plot.png', height=6,width=15)

# Birding Activity
ggplot(dd_type[dd_type$var != 'Num. Species/Trip',], aes(x=(dif), y=value, group=key)) +
  geom_line(aes(color=key), size=1.5) + 
  labs(y='',
       x='\nDays since 4th Wednesday in March', color = 'Trip Protocol') +
  geom_vline(xintercept=0, linetype='dashed') +
  geom_hline(yintercept=0, linetype='dashed') +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0\n[4th Wed.]','7', '14', '21')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=20),
        strip.text = element_text(size=19)) +
  facet_wrap(~var, scales='free')
ggsave('./figs/dd_activity_plot.png', height=6,width=15)


#-----------------------------------------------
# FIG 6 : DIFF IN DIFF GRAPH - PLACEBO
#-----------------------------------------------
ebird_1819 <- filter(ebird_full, year %in% c(2018, 2019))
sample <- did_placebo(ebird_1819)
dd_type <- sample %>%
  group_by(dif, year, protocol) %>%
  summarize(`C. Num. Species/Trip` = mean(s_richness, na.rm=T),
            `B. Num. of Trips` = n_distinct(trip_id),
            `A. Num. Active Users` = n_distinct(observer_id)) %>%
  gather(var, value, contains('Num.')) %>%
  pivot_wider(names_from = c(year, protocol), values_from=value) %>%
  mutate(Stationary = `2019_Stationary` - `2018_Stationary`,
         Travelling = `2019_Traveling` - `2018_Traveling`) %>%
  dplyr::select(dif, var, Stationary, Travelling) %>%
  gather(key, value, c('Stationary', 'Travelling'))

# Birding Activity
ggplot(dd_type, aes(x=(dif), y=value, group=key)) +
  geom_line(aes(color=key), size=1.5) + 
  labs(y='',
       x='\nDays since 4th Wednesday in March', color = 'Trip Protocol') +
  geom_vline(xintercept=0, linetype='dashed') +
  geom_hline(yintercept=0, linetype='dashed') +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(-21, -14, -7, 0, 7, 14, 21),
                     labels= c('-21', '-14', '-7', '0\n[4th Wed.]','7', '14', '21')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        text = element_text(size=20),
        strip.text = element_text(size=19)) +
  facet_wrap(~var, scales='free')
ggsave('./figs/dd_placebo.png', height=6,width=15)

#------------------------------------------------------------
# TABLE 3 : DD REGRESSION RESULTS
#------------------------------------------------------------

# ----- Export for stata ------------------------------------

# Top 20, 2 trips
#sample <- did(ebird_full, num_cities=20)
#write.csv(sample, './data/dd_t20_2trips.csv', row.names = F)

# Top 20, 5 trips
#sample <- did(ebird_full, before=5, after=5, num_cities=20)
#write.csv(sample, './data/dd_t20_5trips.csv', row.names = F)

# Top 20, 10 trips
#sample <- did(ebird_full, before=10, after=10, num_cities=20)
#write.csv(sample, './data/dd_t20_10trips.csv', row.names = F)

# All cities, 2 trips
#sample <- did(ebird_full)
#write.csv(sample, './data/dd_all_2trips.csv', row.names = F)
#--------------------------------------------------------------------

# SPECIFICATION CURVE - MAIN RESULTS
estimates <- data.frame()

# Top 20, 2 Trips
sample <- did(ebird_full, num_cities=20)
estimates <- rbind(estimates, tidy(lm_robust(s_richness ~ TreatPost + Treatment + Post + 
                                               duration + rain + temperature + weekend + 
                                               number_observers + hotspot_km,
                                             data = sample,
                                             fixed_effects = county + protocol + as.factor(hour),
                                             se_type = 'stata')) %>%
                     filter(term == 'TreatPost') %>%
                     select(estimate, std.error) %>%
                     rename(est = 'estimate', se = 'std.error') %>%
                     mutate(`Fixed Effects` = 'District + Hour',
                            `Participation Constraint` = '2 Trips',
                            `Location Constraint` = 'Top 20',
                            `Home` = 'No',
                            `Controls` = 'Full',
                            spec_no = 1))

# Top 20, 5 Trips
sample <- did(ebird_1920, before=5, after=5, num_cities=20)
estimates <- rbind(estimates, tidy(lm_robust(s_richness ~ TreatPost + Treatment + Post + 
                                               duration + rain + temperature + weekend +
                                               number_observers + hotspot_km,
                                             data = sample,
                                             fixed_effects = county + protocol + as.factor(hour),
                                             se_type = 'stata')) %>%
                     filter(term == 'TreatPost') %>%
                     select(estimate, std.error) %>%
                     rename(est = 'estimate', se = 'std.error') %>%
                     mutate(`Fixed Effects` = 'District + Hour',
                            `Participation Constraint` = '5 Trips',
                            `Location Constraint` = 'Top 20',
                            `Home` = 'No',
                            `Controls` = 'Full',
                            spec_no = 2))

# Top 20, 10 Trips
sample <- did(ebird_1920, before=10, after=10, num_cities=20)
estimates <- rbind(estimates, tidy(lm_robust(s_richness ~ TreatPost + Treatment + Post + 
                                               duration + rain + temperature + weekend +
                                               number_observers + hotspot_km,
                                             data = sample,
                                             fixed_effects = county + protocol + as.factor(hour),
                                             se_type = 'stata')) %>%
                     filter(term == 'TreatPost') %>%
                     select(estimate, std.error) %>%
                     rename(est = 'estimate', se = 'std.error') %>%
                     mutate(`Fixed Effects` = 'District + Hour',
                            `Participation Constraint` = '10 Trips',
                            `Location Constraint` = 'Top 20',
                            `Home` = 'No',
                            `Controls` = 'Full',
                            spec_no = 3))

# PLOT
theme_set(theme_cowplot())
set.seed(42)

# Create a plot of the estimates ----
spec_cols <- c("Participation Constraint")
estimates <- estimates %>% 
  mutate(ci_l = est - 1.96 * se, ci_h = est + 1.96 * se) %>%
  arrange(spec_no) # Sort on group for horizontal ordering

coef_plot <- ggplot(estimates, aes(x = spec_no, y = est)) + 
  geom_linerange(aes(ymin = ci_l, ymax = ci_h), size = 1, alpha = 0.5) + 
  geom_point(fill = "white", shape = 21) + 
  labs(y = "Impact of Lockdown on \n Species Richness per Trip") + 
  geom_hline(yintercept=0, linetype='dashed') +
  geom_text(aes(label=round(est, 2)), nudge_x=0.1) +
  theme(axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.margin = unit(c(3,3,3,3), "lines"))
coef_plot

# Make spec plots
spec_plots <- lapply(spec_cols, make_spec_plot)
combined_plot_main <- plot_grid(plotlist = c(list(coef_plot), spec_plots), 
                           labels = c("", spec_cols), 
                           label_size = 8, 
                           label_fontface = "italic", 
                           vjust = 0.5, hjust = -0.1,
                           rel_heights = c(4, 1.1, 1.3, 1.1, 1.1, 1.1), 
                           ncol = 1,
                           align = "v")
save_plot("./figs/spec_curve_main.png", combined_plot_main)


# ROBUSTNESS ---------------------------------------------------
estimates <- data.frame()

# Top 20, 2 Trips, No Controls
sample <- did(ebird_1920, num_cities=20)
estimates <- rbind(estimates, tidy(lm_robust(s_richness ~ TreatPost + Treatment + Post + 
                                               duration + rain + temperature + weekend,
                                             data = sample,
                                             fixed_effects = county + protocol + as.factor(hour),
                                             se_type = 'stata')) %>%
                     filter(term == 'TreatPost') %>%
                     mutate(`Fixed Effects` = 'District + Hour',
                            `Participation Constraint` = '2 Trips',
                            `Location Constraint` = 'Top 20',
                            `Controls` = 'Basic',
                            spec_no = 1))

# All cities, 2 trips
sample <- did(ebird_1920)
estimates <- rbind(estimates, tidy(lm_robust(s_richness ~ TreatPost + Treatment + Post + 
                                               duration + rain + temperature + weekend +
                                               number_observers + hotspot_km,
                                             data = sample,
                                             fixed_effects = county + protocol + as.factor(hour),
                                             se_type = 'stata')) %>%
                     filter(term == 'TreatPost') %>%
                     mutate(`Fixed Effects` = 'District + Hour',
                            `Participation Constraint` = '2 Trips',
                            `Location Constraint` = 'All Cities',
                            `Controls` = 'Full',
                            spec_no = 2))

# Add SUEST results from stata
estimates <- estimates %>%
  rename(est = estimate, se = std.error) %>%
  add_row(est = .7587602, 
          se = .4535671 , 
          `Fixed Effects` = 'District + Hour + User',
          `Participation Constraint` = '2 Trips',
          `Location Constraint` = 'Top 20',
          `Controls` = 'Full',
          spec_no = 3) %>%
  add_row(est = 1.123959, 
          se = .4430729, 
          `Fixed Effects` = 'District + Hour + User',
          `Participation Constraint` = '5 Trips',
          `Location Constraint` = 'Top 20',
          `Controls` = 'Full',
          spec_no = 4)

# Create a plot of the estimates ----
spec_cols <- c("Fixed Effects", "Participation Constraint", "Location Constraint", "Controls")
# Note: This assumes the preferred ordering of the specification categories is the order in which they are given

estimates <- estimates %>% 
  mutate(ci_l = est - 1.96 * se, ci_h = est + 1.96 * se) %>%
  arrange(spec_no) # Sort on group for horizontal ordering

coef_plot <- ggplot(estimates, aes(x = spec_no, y = est)) + 
  geom_linerange(aes(ymin = ci_l, ymax = ci_h), size = 1, alpha = 0.5) + 
  geom_point(fill = "white", shape = 21) + 
  labs(y = "Impact of Lockdown\non Species Richness") + 
  geom_hline(yintercept=0, linetype='dashed') +
  #geom_text(aes(label=round(est, 2)), nudge_x=0.2) +
  theme(axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=11),
        plot.margin = unit(c(3,3,3,3), "lines"))
coef_plot

# Make spec plots
spec_plots <- lapply(spec_cols, make_spec_plot)
combined_plot_rb <- plot_grid(plotlist = c(list(coef_plot), spec_plots), 
                           labels = c("", spec_cols), 
                           label_size = 8, 
                           label_fontface = "italic", 
                           vjust = 0.5, hjust = -0.1,
                           rel_heights = c(6, 1.5, 1.5, 1.5, 1.5), 
                           ncol = 1,
                           align = "v")
save_plot("./figs/spec_curve_rb.png", combined_plot_rb)

# Dynamic DD ----------------------------------------------------------

# Top 20, 2 trips
dd_time <- data.frame()

sample <- did(ebird_1920, num_cities=20) %>%
  mutate(bin = relevel(cut(as.numeric(dif), 8), 4))

dd_time <- rbind(dd_time, tidy(lm_robust(s_richness ~ Treatment:bin + Treatment + bin + 
                 duration + rain + temperature + weekend +
                 number_observers + hotspot_km,
               data = sample,
               fixed_effects = county + protocol + as.factor(hour),
               se_type = 'stata')) %>%
  filter(str_detect(term, '^Treatment:')) %>%
  mutate(term = str_replace(term, 'Treatment:bin', ''),
         term = factor(term, as.character(term))))

# Plot
dd_plot_2 <- ggplot(dd_time, aes(x=term, y=estimate, group=1)) + 
  geom_line() + 
  geom_point(aes(color='Coefficient')) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.3)) +
  geom_hline(yintercept=0, linetype='dashed') +
  ylab('Species Richness Relative to Lockdown\n') +
  xlab('\n Days to Lockdown') +
  scale_colour_manual('' , values = 'black') +
  scale_fill_manual('', values = 'grey12') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour='black'), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=90),
        text=element_text(size=17))
ggsave('./figs/dynamic_dd_2trip.png', height=6,width=10)

# Top 20, 5 trips
dd_time <- data.frame()
sample <- did(ebird_1920, num_cities=20, before=5, after=5) %>%
  mutate(bin = relevel(cut(as.numeric(dif), 8), 4))

dd_time <- rbind(dd_time, tidy(lm_robust(s_richness ~ Treatment:bin + Treatment + bin + 
                                           duration + rain + temperature + weekend +
                                           number_observers + hotspot_km,
                                         data = sample,
                                         fixed_effects = county + protocol + as.factor(hour),
                                         se_type = 'stata')) %>%
                   filter(str_detect(term, '^Treatment:')) %>%
                   mutate(term = str_replace(term, 'Treatment:bin', ''),
                          term = factor(term, as.character(term))))

# Plot
dd_plot_5 <- ggplot(dd_time, aes(x=term, y=estimate, group=1)) + 
  geom_line() + 
  geom_point(aes(color='Coefficient')) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.3)) +
  geom_hline(yintercept=0, linetype='dashed') +
  ylab('Species Richness Relative to Lockdown\n') +
  xlab('\n Days to Lockdown') +
  scale_colour_manual('' , values = 'black') +
  scale_fill_manual('', values = 'grey12') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=90),
        text=element_text(size=17))
ggsave('./figs/dynamic_dd_5trip.png', height=6,width=10)

# COMBINE MAIN AND DYNAMIC RESULTS
require(ggpubr)
ggarrange(combined_plot_main, dd_plot_2, ncol=1)
ggsave('./figs/dd_combined.png', height=6,width=10)
