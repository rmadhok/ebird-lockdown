# PROJECT: Nature in Lockdown
# PURPOSE: Sample Selection Functions
# AUTHOR: Raahil Madhok

select_cities <- function(df, num) {
  
  # Function: select_cities
  ## Inputs: 
  ### df: a data frame - ebird
  ### num: select top 'num' cities by population density
  
  ## Outputs:
  ## df_cities: subset of df restricted to observations in the top 'num' cities
  
  city_list <- df %>%
    group_by(COUNTY) %>%
    #arrange(desc(POP_DENSITY)) %>%
    summarize(state = first(STATE),
              pop_density = first(POP_DENSITY)) %>%
    arrange(desc(pop_density)) %>%
    head(num)
  df_cities <- merge(df, city_list, by = 'COUNTY')
  
  return(df_cities)
  
}

select_users <- function(df, before, after, lockdown) {
  
  # Function: select_users
  ## Inputs: 
  ### df: a data frame - ebird
  ### before: numeric; observer birdwatched during 'before' days before lockdown
  ### after: numeric; observer birdwatched during 'after' days after lockdown
  
  ## Outputs:
  ## df_select: subset of df restricted to users meeting selection criteria.
  
  user_list <- df %>%
    mutate(prepost = if_else(OBSERVATION.DATE <= lockdown, 'PRE', 'POST')) %>%
    group_by(OBSERVER.ID, prepost) %>%
    summarize(n_trips = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
    spread(prepost, n_trips) %>%
    filter(PRE >= before & POST >= after) %>%
    dplyr::select(OBSERVER.ID)
  
  df_select <- merge(df, user_list, by = 'OBSERVER.ID')
  
  return(df_select)
  
}

select_sample <- function(df, 
                          before = 2, 
                          after = 2, 
                          num_cities = NULL, 
                          home = FALSE,
                          lockdown = '2020-03-24') {
  
  # Function: select_sample
  ## Inputs: 
  ### df: a data frame - ebird
  ### before: numeric; observer logged 'before' trips before lockdown
  ### after: numeric; observer logged 'after' trips after lockdown
  ### num_cities: numeric; select observations in top 'num_cities' cities by pop. density
  ### home: TRUE/FALSE; select observations from home
  ## lockdown: date of lockdown
  
  ## Outputs:
  ## sample: dataframe restricted to meeting all selection criteria.
  
  
  # If cities and home not inputted 
  if(is.null(num_cities) & isFALSE(home)) {
     
     # Select users from all cities, all places, meeting participation constraint
     sample <- select_users(df, before, after, lockdown)
     
     return(sample)
     }
  
  # Only Home not inputted
  if(isFALSE(home) & !is.null(num_cities)) {
  
    # Select top cities
    sample <- select_cities(df, num_cities)
    
    # Select users from top cities meeting participation constraint
    sample <- select_users(sample, before, after, lockdown)
    
    return(sample)
    }
  
  # Only cities not inputted
  if(isTRUE(home) & is.null(num_cities)) {
    
    # Select users at home in all cities
    sample <- filter(df, LOCALITY.TYPE == 'P')
    
    # Select users from all cities meeting participation constraint
    sample <- select_users(sample, before, after, lockdown)
    
    return(sample)
    }
  
  # All arguments given
  if(isTRUE(home) & !is.null(num_cities)) {
    
    # Select top X cities
    sample <- select_cities(df, num_cities)
    
    # Select users at home in top X cities
    sample <- filter(sample, LOCALITY.TYPE == 'P')
    
    # Select users at home in top X cities meeting participation constraint
    sample <- select_users(sample, before, after, lockdown)
    
    return(sample)
    
  }
}

event_study <- function(df, 
                        before = 2, 
                        after = 2, 
                        num_cities = NULL, 
                        home = FALSE,
                        lockdown = '2020-03-25',
                        user_fe = F) {
  
  # Select Sample
  sample <- select_sample(df, before, after, num_cities, home, lockdown)
  
  # Trip level
  sample <- sample %>%  
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER, 
                  DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE,
                  HOUR, n_trips_pld, rain, temperature, weekend, 
                  NUMBER.OBSERVERS, hotspot_km) %>%
    mutate(dif = OBSERVATION.DATE - (as.Date(lockdown)-1)) %>%
    mutate(dif = str_replace(as.character(dif), '-','m'))
  
  # Estimate
  if(isFALSE(user_fe)) {
    
  est <- lm(s_richness ~ dif + DURATION.MINUTES + n_trips_pld + rain + temperature +
              COUNTY + as.factor(HOUR) + PROTOCOL.TYPE + weekend + hotspot_km + NUMBER.OBSERVERS, 
            data = sample)

  # tidy
  est_df <- broom::tidy(est, conf.int = T) %>% 
    filter(str_detect(term, "^dif")) %>%
    mutate(time = str_replace(term, 'dif', ''),
           time = as.numeric(str_replace(time, 'm', '-')))
  
  return(est_df)
  
  }
  
  if(isTRUE(user_fe)) {
    est <- lm(s_richness ~ dif + DURATION.MINUTES + rain + temperature +
                OBSERVER.ID + as.factor(HOUR) + PROTOCOL.TYPE + weekend + hotspot_km + NUMBER.OBSERVERS, 
              data = sample)

    # tidy
    est_df <- broom::tidy(est, conf.int = T) %>% 
      filter(str_detect(term, "^dif")) %>%
      mutate(time = str_replace(term, 'dif', ''),
             time = as.numeric(str_replace(time, 'm', '-')))
    
    return(est_df)
  }
 
}

did <- function(df,
                before = 2,
                after = 2,
                num_cities = NULL,
                home = F,
                drop = T){
  
  # 2019 Slice
  df_19 <- df[df$YEAR == 2019 & 
                df$OBSERVATION.DATE <= '2019-04-20' &
                ebird_full$OBSERVATION.DATE >= '2019-03-03',]
  df_19 <- select_sample(df_19, before, after, num_cities, home, lockdown='2019-03-26')
  df_19 <- df_19 %>%
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER,
                  YEAR, DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE, 
                  HOUR, n_trips_pld, rain, temperature, weekend, 
                  NUMBER.OBSERVERS, hotspot_km, LOCALITY, LOCALITY.TYPE, day_week) %>%
    mutate(prepost = if_else(OBSERVATION.DATE <= '2019-03-26', 'PRE', 'POST'),
           dif = OBSERVATION.DATE - as.Date('2019-03-27'))
  
  # 2020 slice
  df_20 <- df[df$YEAR == 2020 & df$OBSERVATION.DATE <= '2020-04-17',]
  
  if(isTRUE(drop)) {
    
    df_20 <- df_20[df_20$OBSERVATION.DATE != '2020-03-22',]
  }
  
  df_20 <- select_sample(df_20, before, after, num_cities, home, lockdown='2020-03-24')
  df_20 <- df_20 %>%
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER, 
                  YEAR, DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE,
                  HOUR, n_trips_pld, rain, temperature, weekend,
                  NUMBER.OBSERVERS, hotspot_km, LOCALITY, LOCALITY.TYPE, day_week) %>%
    mutate(prepost = if_else(OBSERVATION.DATE <= '2020-03-24', 'PRE', 'POST'),
           dif = OBSERVATION.DATE - as.Date('2020-03-25'))
  
  # stack
  sample <- rbind(df_19, df_20)
  
  # Treatment
  sample$Treatment <- as.numeric(sample$YEAR == 2020)
  sample$Post <- as.numeric(sample$prepost == 'POST')
  sample$TreatPost <- sample$Treatment * sample$Post
  
  return(sample)
  
}

# Function to create a specification plot for a single category. 
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