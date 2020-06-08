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
  ### before: numeric; observer birdwatched on 'before' days before lockdown
  ### after: numeric; observer birdwatched on 'after' days after lockdown
  
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
    
    # Select users from all cities meeting participation constraint
    sample <- select_users(df, before, after, lockdown)
    
    # Select users at home in all cities
    sample <- filter(sample, LOCALITY.TYPE == 'P')
    
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
                        lockdown = '2020-03-24',
                        user_fe = F) {
  
  # Select Sample
  sample <- select_sample(df, before, after, num_cities, home, lockdown)
  
  # Trip level
  sample <- sample %>%  
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER, 
                  DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE,
                  HOUR, n_trips_pld, rain, temperature) %>%
    mutate(dif = OBSERVATION.DATE - as.Date(lockdown)) %>%
    mutate(dif = str_replace(as.character(dif), '-','m'))
  
  # Estimate
  if(isFALSE(user_fe)) {
    
  est <- lm(s_richness ~ dif + DURATION.MINUTES + n_trips_pld + rain +
              OBSERVATION.DATE + COUNTY + HOUR + PROTOCOL.TYPE, 
            data = sample)

  # tidy
  est_df <- broom::tidy(est, conf.int = T) %>% 
    filter(str_detect(term, "^dif")) %>%
    mutate(time = str_replace(term, 'dif', ''),
           time = as.numeric(str_replace(time, 'm', '-')))
  
  return(est_df)
  }
  
  if(isTRUE(user_fe)) {
    est <- lm(s_richness ~ dif + DURATION.MINUTES + rain +
                OBSERVER.ID + OBSERVATION.DATE + HOUR + PROTOCOL.TYPE, 
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
                lockdown = NULL){
  
  # 2019 Slice
  df_19 <- df[df$YEAR == 2019 & df$OBSERVATION.DATE <= '2019-04-17',]
  df_19 <- select_sample(df_19, before, after, num_cities, home, lockdown='2019-03-24')
  df_19 <- df_19 %>%
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER,
                  YEAR, DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE, 
                  HOUR, n_trips_pld, rain, temperature) %>%
    mutate(prepost = if_else(OBSERVATION.DATE <= '2019-03-24', 'PRE', 'POST'))
  
  # 2020 slice
  df_20 <- df[df$YEAR == 2020 & df$OBSERVATION.DATE <= '2020-04-17' &
                df$OBSERVATION.DATE != '2020-03-22',]
  df_20 <- select_sample(df_20, before, after, num_cities, home, lockdown='2020-03-24')
  df_20 <- df_20 %>%
    distinct(SAMPLING.EVENT.IDENTIFIER, .keep_all = T) %>%
    dplyr::select(OBSERVER.ID, OBSERVATION.DATE, SAMPLING.EVENT.IDENTIFIER, 
                  YEAR, DURATION.MINUTES, s_richness, COUNTY, STATE, PROTOCOL.TYPE,
                  HOUR, n_trips_pld, rain, temperature) %>%
    mutate(prepost = if_else(OBSERVATION.DATE <= '2020-03-24', 'PRE', 'POST'))
  
  # stack
  sample <- rbind(df_19, df_20)
  
  # Treatment
  sample$Treatment <- as.numeric(sample$YEAR == 2020)
  sample$Post <- as.numeric(sample$prepost == 'POST')
  sample$TreatPost <- sample$Treatment*sample$Post
  
  return(sample)
  
}