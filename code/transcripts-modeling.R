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

tokenized_df <- map(t_cleaned, ~ tokenize_words(.x)) %>% #, stopwords = stop_words)) %>% 
  flatten() %>% 
  enframe() %>% 
  unnest(cols = value) %>% 
  mutate(name = str_remove_all(name, "(\\.it\\.txt)")) %>% 
  mutate(name = str_remove_all(name, "\\-.{11}$")) %>% 
  #mutate(name = str_remove_all(name, ".+(?=.{11}$)")) %>% 
  rename(video_id = name) %>% 
  rename(text = value)

word_frequency <- tokenized_df %>% 
  count(video_id, text, sort = TRUE)

total_words <- word_frequency %>% 
  group_by(video_id) %>% 
  summarise(total = sum(n))

transcripts_words <- left_join(word_frequency, total_words) %>% 
  group_by(video_id) %>% 
  mutate(tf = n/total,
         rank = row_number()) %>% 
  bind_tf_idf(text, video_id, n) %>% 
  arrange(idf) %>% 
  relocate(idf, tf_idf, .after = tf) %>% 
  relocate(rank, .after = total) %>% 
  ungroup()

write_csv(transcripts_words, path = "transcripts-words-df.csv")

print(transcripts_words, n = 10)

transcripts_words %>% 
  select(video_id) %>% 
  unique() %>%
  arrange() %>% 
  print(n = Inf)

transcripts_words <- read_csv("transcripts-words-df.csv")
transcripts_words %>% 
  filter(video_id != str_detect(transcripts_words$video_id, pattern = "([Ll]ectures?)"),
         video_id == str_detect(transcripts_words$video_id, pattern = "Route"),
         video_id %in% sample(video_id, size = 6)) %>% 
  mutate(text = factor(text, levels = unique(text)),
         text = reorder_within(text, tf_idf, video_id, fun = sum)) %>%
  group_by(video_id) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(text, tf_idf, fill = video_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  scale_x_reordered() +
  facet_wrap(~ video_id, ncol = 2, scales = "free") +
  coord_flip() +
  theme_bw()
  
  

# ggplot(transcripts_words, aes(rank, tf, color = video_id)) +
#   geom_line(show.legend = FALSE) +
#   theme_bw() +
#   scale_x_log10() +
#   scale_y_log10()


ggplot(transcripts_words, aes(n/total, fill = video_id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.009) +
  facet_wrap(~ video_id, nrow = 4, scales = "free_y")

bigrams <- map(t_cleaned, ~ tokenize_ngrams(.x, n = 2, stopwords = stop_words)) %>% 
  flatten() %>% 
  enframe() %>% 
  unnest(cols = value) %>% 
  mutate(name = str_remove_all(name, "(\\.it\\.txt)")) %>% 
  mutate(name = str_remove_all(name, ".+(?=.{11}$)")) %>% 
  rename(video_id = name) %>% 
  rename(bigram = value)

bigram_counts <- bigrams %>% 
  count(video_id, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, video_id, n) %>% 
  arrange(desc(tf_idf))

View(bigram_counts)

