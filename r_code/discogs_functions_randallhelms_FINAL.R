nullToNA <- function(x) {
  
  x[sapply(x, is.null)] <- NA
  return(x)
  
}

discogs_pat <- function(pat) {
  
  Sys.setenv('DISCOGS_API_TOKEN' = pat)
  
}

#call the API for users

discogs_users <- function(username) {
  
  require(httr)
  require(tidyverse)
  require(glue)
  
  url_slug <- 'https://api.discogs.com'
  
  section <- 'users'
  
  uname <- username
  
  get_url <- paste(url_slug,section,uname,sep='/')
  
  access_token <- Sys.getenv('DISCOGS_API_TOKEN')
 
  result <- GET(get_url,add_headers(Authorization = glue("Discogs token={access_token}")))
  
  result2 <- content(result,'parsed') %>%
    as_tibble()
  
  return(result2)
  
}

#a function to fix lists

discogs_list_fixer <- function(list_to_fix) {
  
  require(tidyverse)
  
  df <- tibble()
  
  for (i in 1:length(list_to_fix)) {
    
    temp <- list_to_fix[[i]]
    
    temp_df <- as_tibble(temp) %>%
      select(-user) %>%
      distinct()
    
    df <- bind_rows(df,temp_df)
    
  }
  
  return(df)
  
}

#get all of a particular user's lists

discogs_user_lists <- function(username) {
  
  require(httr)
  require(tidyverse)
  require(glue)
  
  #build the URL
  
  url_slug <- 'https://api.discogs.com'
  
  section <- 'users'
  
  uname <- username 
  
  section2 <- 'lists'
  
  lists_url <- paste(url_slug,section,uname,section2,sep='/')
  
  #make the call
  
  access_token <- Sys.getenv('DISCOGS_API_TOKEN')
  
  lists_get <- GET(lists_url,add_headers(Authorization = glue("Discogs token={access_token}")))
  
  print(lists_get)
  
  lists <- content(lists_get,'parsed')
  
  list_of_lists <- lists$lists
  
  list_df <- discogs_list_fixer(list_of_lists)
  
  #check for pagination and repeat for multiple pages
  
  if (lists$pagination$pages > 1) {
    
    page_nr <- lists$pagination$pages
    
    for (i in 2:page_nr) {
      
      new_url <- paste(lists_url,'?page=',i,'&per_page=50',sep='')
      
      temp_get <- GET(new_url,add_headers(Authorization = glue("Discogs token={access_token}")))
      
      print(temp_get)
      
      temp_lists <- content(temp_get,'parsed')
      
      temp <- temp_lists$lists
      
      temp_df <- discogs_list_fixer(temp)
      
      list_df <- bind_rows(list_df,temp_df)
      
    }
    
  }
  
  return(list_df)
  
}

#get all results for a specific list

discogs_list <- function(list_id) {
  
  require(httr)
  require(tidyverse)
  require(glue)
  
  #build the URL
  
  url_slug <- 'https://api.discogs.com'
  
  section2 <- 'lists'
  
  lists_url <- paste(url_slug,section2,list_id,sep='/')
  
  #make the call
  
  access_token <- Sys.getenv('DISCOGS_API_TOKEN')
  
  lists_get <- GET(lists_url,add_headers(Authorization = glue("Discogs token={access_token}")))
  
  lists <- content(lists_get,'parsed')
  
  #check if you've been rate-limited
  
  if(is.null(lists$message) == FALSE) {
    
    print('Rate limited, pausing 20 seconds')
    
    Sys.sleep(20)
    
    #try again
    
    lists_get <- GET(lists_url,add_headers(Authorization = glue("Discogs token={access_token}")))
    
    lists <- content(lists_get,'parsed')
    
  }
  
  #reshape items into a df
  
  items_df <- tibble()
  
  for (i in 1:length(lists$items)) {
    
    temp_df <- as_tibble(lists$items[[i]]) %>%
      select(-stats) %>%
      rename(release_id = id,release_uri = uri, release_url = resource_url,release_image_url = image_url)
    
    items_df <- bind_rows(items_df,temp_df)
    
  }
  
  lists$items <- NULL
  
  #reshape user info into a df
  
  user_df <- as_tibble(lists$user) %>%
    rename(user_id = id,user_resource_url = resource_url)
  
  lists$user <- NULL
  
  metadata_df <- bind_cols(as_tibble(lists),user_df)
  
  #make final merge
  
  list_items_df <- bind_cols(metadata_df,items_df) %>%
    distinct()
  
  return(list_items_df)
  
}

#get all lists from a vector of list IDs

discogs_get_all_lists <- function(x) {
  
  require(tidyverse)
  
  df <- tibble()
  
  for (i in 1:length(x)) {
    
    nr = i
    
    temp_df <- discogs_list(x[[i]])
    
    temp_name <- temp_df %>%
      distinct(name)
    
    df <- bind_rows(df,temp_df)
    
    print(paste(i,': ',temp_name,' processed',sep=''))
    
  }
  
  return(df)
  
}

#get price suggestions

discogs_price_suggestion <- function(release_id) {
  
  require(tidyverse)
  require(glue)
  require(purrr)
  
  #generate URL
  
  url_slug <- "https://api.discogs.com"
  
  section <- 'marketplace'
  
  section2 <- 'price_suggestions'
  
  get_url <- paste(url_slug,section,section2,release_id,sep='/')
  
  access_token <- Sys.getenv('DISCOGS_API_TOKEN')
  
  # Set up the request with the authentication headers
  
  result <- GET(get_url,add_headers(Authorization = glue("Discogs token={access_token}")))
  
  result2 <- content(result,'parsed')
  
  #check for empty result (i.e. no suggestion)
  
  if (is_empty(result2)) {
    
    empty_df <- tibble(release_id = release_id,condition=NA,value=NA,currency=NA)
    
    print(paste('release_id',release_id,'has no result'))
    
    return(empty_df)
    
  } else {
    
    #create single columns from each list field, then bind them horizontally
    
    res3 <- map(result2,~.x[['value']])
    res4 <- try(do.call('bind_rows',map(res3,as_tibble)))
    
    curr <- map(result2,~.x[['currency']])
    curr2 <- try(do.call('bind_rows',map(curr,as_tibble))) %>%
      rename(currency = 1)
    
    price_estimate_df <- tibble(release_id = release_id,
                                condition = names(result2)) %>%
      bind_cols(res4) %>%
      bind_cols(curr2)
    
    print(paste('release_id',release_id,'processed'))
    
    return(price_estimate_df)
    
  }
  
}

