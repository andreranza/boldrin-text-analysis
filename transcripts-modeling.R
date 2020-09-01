library(tidyverse)
library(tokenizers)
library(textrecipes)
library(tidytext)
library(stopwords)

stop_words <- read_file("stopwords-it.txt") %>% 
  str_split(pattern = "\n") %>% 
  unlist()

setwd("./transcripts-cleaned")
txt_files <- dir()
t_cleaned <- map(txt_files, ~ read_file(.x))
setwd("../")

names(t_cleaned) <- txt_files

tokenized_df <- map(t_cleaned, ~ tokenize_words(.x, stopwords = stop_words)) %>% 
  flatten() %>% 
  enframe() %>% 
  unnest(cols = value) %>% 
  mutate(name = str_remove_all(name, "(\\.it\\.txt)")) %>% 
  mutate(name = str_remove_all(name, ".+(?=.{11}$)")) %>% 
  rename(video_id = name) %>% 
  rename(text = value)

View(tokenized_df)

word_frequency <- tokenized_df %>% 
  count(value, sort = TRUE) 

bigram <- map(t_cleaned, ~ tokenize)
