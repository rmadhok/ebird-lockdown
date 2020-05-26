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
    arrange(-pop_density) %>%
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
    mutate(prepost = if_else(OBSERVATION.DATE < lockdown, 'PRE', 'POST')) %>%
    group_by(OBSERVER.ID, prepost) %>%
    summarize(n_days = n_distinct(OBSERVATION.DATE)) %>%
    spread(prepost, n_days) %>%
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
                          lockdown = '2020-03-17') {
  
  # Function: select_sample
  ## Inputs: 
  ### df: a data frame - ebird
  ### before: numeric; observer birdwatched on 'before' days before lockdown
  ### after: numeric; observer birdwatched on 'after' days after lockdown
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
    
    # Select users from top X cities meeting participation constraint
    sample <- select_users(sample, before, after, lockdown)
    
    # Select users at home
    sample <- filter(sample, LOCALITY.TYPE == 'P')
    
    return(sample)
    
  }
  
}