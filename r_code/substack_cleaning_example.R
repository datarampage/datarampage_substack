library(readxl)
library(tidyverse)
library(lubridate)
library(googlesheets4)

#function for cleaning column names

tidycols <- function(df) {
  require(snakecase)
  
  dfnames <- colnames(df)
  
  dfnames <- gsub('#','nr',dfnames,fixed=TRUE)
  
  dfnames <- gsub('%','pct',dfnames,fixed=TRUE)
  
  dfnames <- gsub('Ø','avg',dfnames,fixed=TRUE)
  
  dfnames <- gsub('$','usd',dfnames,fixed=TRUE)
  
  dfnames <- gsub('€','eur',dfnames,fixed=TRUE)
  
  dfnames <- gsub('£','gbp',dfnames,fixed=TRUE)
  
  dfnames <- to_snake_case(dfnames,sep_out = "_")
  
  dfnames <- tolower(gsub(" ","_",dfnames))
  
  dfnames <- gsub(".","_",dfnames,fixed=TRUE)
  
  dfnames <- gsub("/","_per_",dfnames,fixed=TRUE)
  
  colnames(df) <- dfnames
  
  return(df)
  
}

#read the file in

gs4_auth()

url <- 'https://docs.google.com/spreadsheets/d/1n1YZSThrLEjqj7kvRD8rKJsPF5NAe_kGhSzmZtYv4RM/edit#gid=0'

test <- read_sheet(url,skip = 4,col_types = 'c') %>%
  tidycols()

#clean the data

test <- test %>%
  #remove missing rows
  filter(!is.na(time)) %>%
  #create a proper date column
  mutate(date = ifelse(is.na(variable_1),time,NA),
         date = dmy(date)) %>%
  #fill missing values downwards
  fill(date,.direction = 'down') %>%
  #remove empty rows
  filter(!is.na(variable_1)) %>%
  #fix variable data type
  mutate_at(vars(starts_with('variable')),as.numeric ) %>%
  #create a time stamp
  mutate(time_new = paste(time,':00',sep=''),
        timestamp = paste(as.character(date),time_new),
         timestamp = as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%S")) %>%
  #tidy up and reorder
  select(-time_new) %>%
  select(date:timestamp,everything()) 

#viz example using the latest timestamp from each day

test_clean <- test %>%
  pivot_longer(cols = c(variable_1,variable_2,variable_3),names_to = 'measurement') 

test_clean %>%
  group_by(date,measurement) %>%
  mutate(latest_timestamp = timestamp == max(timestamp)) %>%
  ungroup() %>%
  filter(latest_timestamp == T) %>%
  ggplot(aes(x = date,y = value,col=measurement ))+
  geom_line()+
  scale_x_date()+
  scale_y_continuous(limits=c(0,160))+
  theme_minimal()+
  geom_smooth(method='lm',se=F,linetype = 2,alpha=0.35)+
  ggtitle('Data Rampage Data Cleaning Example')+
  theme(legend.position = 'bottom')+
  labs(col='Measurement')+
  ylab('Value')+
  xlab('Date')

#write it

gfile_url <- 'YOUR NEW GOOGLE SHEETS URL'

sheet_write(test_clean,ss=gfile_url)