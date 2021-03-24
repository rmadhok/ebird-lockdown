# PROJECT: Nature in Lockdown
# PURPOSE: Sample Selection Functions
# AUTHOR: Raahil Madhok

#-----------------------------------
# Select Top Cities
#-----------------------------------
select_cities <- function(df, num) {
  
  # Function: select_cities
  ## Inputs: 
  ### df: a data frame - ebird
  ### num: select top 'num' cities by population density
  
  ## Outputs:
  ## df_cities: subset of df restricted to observations in the top 'num' cities
  
  city_list <- df %>%
    group_by(county) %>%
    summarize(state = first(state),
              pop_density = first(pop_density)) %>%
    arrange(desc(pop_density)) %>%
    head(num)
  df_cities <- merge(df, city_list, by = 'COUNTY')
  
  return(df_cities)
  
}

#-----------------------------------
# Impose Participation Constraint
#-----------------------------------
select_users <- function(df, before, after, lockdown) {
  
  # Function: select_users
  ## Inputs: 
  ### df: a data frame - ebird
  ### before: numeric; observer birdwatched during 'before' days before lockdown
  ### after: numeric; observer birdwatched during 'after' days after lockdown
  
  ## Outputs:
  ## df_select: subset of df restricted to users meeting selection criteria.
  
  user_list <- df %>%
    mutate(prepost = if_else(date <= lockdown, 'pre', 'post')) %>%
    group_by(observer_id, prepost) %>%
    summarize(n_trips = n_distinct(trip_id)) %>%
    spread(prepost, n_trips) %>%
    filter(pre >= before & post >= after) %>%
    dplyr::select(observer_id)
  
  df_select <- merge(df, user_list, by = 'observer_id')
  
  return(df_select)
  
}

#-----------------------------------
# Select Analysis Sample
#-----------------------------------
select_sample <- function(df, 
                          before = 2, 
                          after = 2, 
                          num_cities = NULL, 
                          lockdown = '2020-03-24') {
  
  # Function: select_sample
  ## Inputs: 
  ### df: a data frame - ebird
  ### before: numeric; observer logged 'before' trips before lockdown
  ### after: numeric; observer logged 'after' trips after lockdown
  ### num_cities: numeric; select observations in top 'num_cities' cities by pop. density
  ### home: TRUE/FALSE; select observations from home
  ### lockdown: date of lockdown
  
  ## Outputs:
  ## sample: dataframe restricted to meeting all selection criteria.
  
  # If cities not inputted 
  if(is.null(num_cities)) {
     
     # Select users from all cities meeting participation constraint
     sample <- select_users(df, before, after, lockdown)
     
     return(sample)
     }
  
  # All arguments inputted
  if(!is.null(num_cities)) {
  
    # Select top cities
    sample <- select_cities(df, num_cities)
    
    # Select users from top cities meeting participation constraint
    sample <- select_users(sample, before, after, lockdown)
    
    return(sample)
    }
}

#-----------------------------------
# Diff in Diff Sample
#-----------------------------------
did <- function(df,
                before = 2,
                after = 2,
                num_cities = NULL,
                drop = T){
  
  # 2019 Slice
  df_19 <- filter(df, year == 2019 & date <= '2019-04-20' & date >= '2019-03-03') 
  df_19 <- select_sample(df_19, before, after, num_cities, lockdown='2019-03-26')
  df_19 <- df_19 %>%
    distinct(trip_id, .keep_all = T) %>%
    dplyr::select(observer_id, date, trip_id, year, duration, s_richness, 
                  county, state, protocol, hour, rain, temperature, weekend, 
                  number_observers, hotspot_km, locality, locality_type) %>%
    mutate(prepost = if_else(date <= '2019-03-26', 'pre', 'post'),
           dif = date - as.Date('2019-03-27'))
  
  # 2020 slice
  df_20 <- filter(df, year == 2020 & date <= '2020-04-17')
  
  if(isTRUE(drop)) {
    df_20 <- filter(df_20, date != '2020-03-22')
  }
  
  df_20 <- select_sample(df_20, before, after, num_cities, lockdown='2020-03-24')
  df_20 <- df_20 %>%
    distinct(trip_id, .keep_all = T) %>%
    dplyr::select(observer_id, date, trip_id, year, duration, s_richness, 
                  county, state, protocol, hour, rain, temperature, weekend,
                  number_observers, hotspot_km, locality, locality_type) %>%
    mutate(prepost = if_else(date <= '2020-03-24', 'pre', 'post'),
           dif = date - as.Date('2020-03-25'))
  
  # stack
  sample <- rbind(df_19, df_20)
  
  # Treatment
  sample$Treatment <- as.numeric(sample$year == 2020)
  sample$Post <- as.numeric(sample$prepost == 'post')
  sample$TreatPost <- sample$Treatment * sample$Post
  
  return(sample)
  
}

#-----------------------------------------------
# Diff in Diff Sample - PLACEBO (FOR REFEREE 1)
#----------------------------------------------
did_placebo <- function(df,
                        before = 2,
                        after = 2,
                        num_cities = NULL){
  
  # 2018 Slice
  df_18 <- filter(df, year == 2018 & date <= '2018-04-20' & date >= '2018-03-04') 
  df_18 <- select_sample(df_18, before, after, num_cities, lockdown='2018-03-27')
  df_18 <- df_19 %>%
    distinct(trip_id, .keep_all = T) %>%
    dplyr::select(observer_id, date, trip_id, year, duration, s_richness, 
                  county, state, protocol, hour, rain, temperature, weekend, 
                  number_observers, hotspot_km, locality, locality_type) %>%
    mutate(prepost = if_else(date <= '2018-03-27', 'pre', 'post'),
           dif = date - as.Date('2018-03-28'))
  
  # 2019 Slice
  df_19 <- filter(df, year == 2019 & date <= '2019-04-20' & date >= '2019-03-03') 
  df_19 <- select_sample(df_19, before, after, num_cities, lockdown='2019-03-26')
  df_19 <- df_19 %>%
    distinct(trip_id, .keep_all = T) %>%
    dplyr::select(observer_id, date, trip_id, year, duration, s_richness, 
                  county, state, protocol, hour, rain, temperature, weekend, 
                  number_observers, hotspot_km, locality, locality_type) %>%
    mutate(prepost = if_else(date <= '2019-03-26', 'pre', 'post'),
           dif = date - as.Date('2019-03-27'))
  
  # stack
  sample <- rbind(df_18, df_19)
  
  # Treatment
  sample$Treatment <- as.numeric(sample$year == 2019)
  sample$Post <- as.numeric(sample$prepost == 'post')
  sample$TreatPost <- sample$Treatment * sample$Post
  
  return(sample)
  
}

#-----------------------------------
# Specification Plot
#-----------------------------------
make_spec_plot <- function(category) {
  
  if(category == 'Participation Constraint'){
    specs <- dummy_cols(estimates, select_columns = category, remove_selected_columns = T) %>%
      select(spec_no, starts_with(category)) %>% 
      pivot_longer(starts_with(category), names_prefix = paste0(category, '_')) %>%
      mutate(name = factor(name, levels = c('10 Trips', '5 Trips', '2 Trips'))) 
  } else{
  specs <- dummy_cols(estimates, select_columns = category, remove_selected_columns = T) %>%
    select(spec_no, starts_with(category)) %>% 
    pivot_longer(starts_with(category), names_prefix = paste0(category, "_")) %>%
    mutate(name = factor(name, levels = rev(unique(name)))) 
  }
  # category = spec_cols[1] # DEBUG
  
  spec_plot <- ggplot(specs, aes(x = spec_no, y = name, alpha = value)) +
    geom_point() + 
    scale_alpha_continuous(guide = FALSE) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.line.x = element_blank(), 
          axis.text.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 8), 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
  spec_plot    
}