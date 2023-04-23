library(tidyverse)
library(httr)
library(purrr)
library(glue)
library(ggthemes)

#REMEMBER TO LOAD ALL THE FUNCTIONS

#authenticate

discogs_pat('PERSONAL_ACCESS_TOKEN')

#get user info

users <- discogs_users('hodderian')

#get a vector of all list IDs

hodderian_lists <- discogs_user_lists('hodderian')

all_list_ids <- hodderian_lists %>%
  filter(public == T) %>%
  filter(str_starts(name,'Pearsall') == T) %>%
  pull(id)

#get all the lists

hodderian_discogs_lists <- discogs_get_all_lists(all_list_ids)

#get pricing suggestions

#create df with first suggestion and get a vector of all IDs

release_ids <- hodderian_discogs_lists %>%
  distinct(release_id) %>%
  pull()

prices_df <- discogs_price_suggestion(release_ids[[1]])

#run through the rest of the IDs one-by-one to get price suggestions and join to the main DF

for (i in 2:length(release_ids)) {
  
  rid <- release_ids[[i]]
  
  pdf <- prices_df %>%
    filter(release_id == rid)
  
  if (nrow(pdf) > 0) {
    
    next
    
  } else {
    
    print(i)
    
    temp_df <- discogs_price_suggestion(release_ids[[i]])
    
    prices_df <- bind_rows(prices_df,temp_df)
    
    Sys.sleep(1)
    
  }
    
}

#filter to VG+ and create a single DF

prices2 <- prices_df %>%
  filter(condition == 'Very Good Plus (VG+)')

list_releases_df <- hodderian_discogs_lists %>%
  left_join(prices2,by='release_id') %>%
  distinct()

#summarize the results

values <- list_releases_df %>%
  filter(!is.na(value)) %>%
  group_by(id,name) %>%
  summarize(items = n(),
            med_eur = median(value),
            avg_eur = mean(value),
            max_eur = max(value,na.rm = T),
            min_eur = min(value,na.rm = T),
            max_min_diff = max_eur - min_eur) %>%
  arrange(desc(avg_eur))

#visualize the results

value_histogram <- values %>%
  ggplot(aes(x = avg_eur, y = after_stat(density), fill = after_stat(density))) + 
  geom_histogram(binwidth = 2, color = "white") +
  scale_fill_gradient(low = "#FF4B00", high = "#FFDD00") +
  labs(title = "Pearsall's Mixes: Distribution of Mean Discogs Suggested Prices", x = "Avg. Value EUR", y = "Frequency")+
  guides(fill='none')+
  theme_clean() 

value_scatter <- values %>% 
  ggplot(aes(x=items,y=avg_eur,size=items,col=avg_eur)) + 
  geom_point() + 
  theme_clean() + 
  scale_color_gradient(low = "#FF4B00", high = "#FFDD00")+
  labs(title = "Pearsall's Mixes: Avg Price EUR vs # of Records", y = "Avg. Value EUR", x = "# of Records Used")+
  theme(  legend.position = "bottom", # move both legends to bottom
          legend.box = "horizontal", # arrange the legends horizontally
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # remove margin around the legends
          legend.spacing = unit(0.2, "cm") # set spacing between the legends
  )

value_scatter_se <- value_scatter+
  geom_smooth(linetype=2,color=alpha('black',.5))

value_scatter_v2 <- values %>% 
  ggplot(aes(x=med_eur,y=avg_eur,size=max_min_diff,col=avg_eur)) + 
  geom_point() + 
  theme_clean() + 
  scale_color_gradient(low = "#FF4B00", high = "#FFDD00")+
  labs(title = "Pearsall's Mixes: Avg  vs Median Price EUR", y = "Avg. Value EUR", x = "Med. Value EUR")+
  theme(  legend.position = "bottom", # move both legends to bottom
          legend.box = "horizontal", # arrange the legends horizontally
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # remove margin around the legends
          legend.spacing = unit(0.2, "cm") # set spacing between the legends
  )+
  geom_smooth(linetype=2,color=alpha('black',0.5))
