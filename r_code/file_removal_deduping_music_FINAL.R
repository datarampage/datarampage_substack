library(fs)
library(tidyverse)
library(purrr)

#make a function to generate a df of all files in a directory

dupe_finder <- function(directory,string) {

  require(tidyverse)

  #change the working directory to the specified directory
  setwd(directory)

  #recursively list all mp3 files
  df <- tibble(files = list.files(pattern='.mp3$',recursive = T),
               #create the full file path
               file_path = paste(directory,files,sep='/'),
               #check for duplicates using a string pattern
               duplicate = str_detect(files,fixed(string)))


  return(df)

}

#make a function to remove duplicates

dupe_remover <- function(df) {

  require(tidyverse)
  require(fs)
  require(purrr)

  #extract all duplicate file paths into a vector
  files_to_remove <- df %>%
    filter(duplicate == T) %>%
    pull(file_path)

  #iterate through them to delete them directly
  purrr::map(files_to_remove,file.remove)

  print('All files removed')


}

#make a function to move duplicates to a new destination folder

dupe_mover <- function(df,new_path) {

  require(tidyverse)
  require(fs)
  require(purrr)

  #extract all duplicate file paths into a vector
  files_to_move <- df %>%
    filter(duplicate == T) %>%
    pull(file_path)


  #iterate through them to move to the new path
  purrr::map(files_to_move,file_move,new_path)

  print(paste('All files moved to',new_path))

}

#test this by moving files

af <- dupe_finder('H:/My Music/rock/Arcade Fire - The Suburbs',' (1).mp3')

#check percentage of duplicates
af %>%
  group_by(duplicate) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

dupe_mover(af,'H:/My Music/dupe_test')

#test this by moving some files

rap <- dupe_finder('H:/My Music/rap',' (1).mp3')

dupe_mover(rap,'H:/My Music/dupe_test')

rock <- dupe_finder('H:/My Music/rock',' (1).mp3')

rock

rock %>%
  group_by(duplicate) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

dupe_remover(rock)

#go through all files

music <- dupe_finder('H:/My Music',' (2).mp3')
