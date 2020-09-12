library(tidyverse)
library(tokenizers)
library(textrecipes)
library(tidytext)
library(topicmodels)
library(stopwords)

txt_files <- dir("./transcripts-cleaned", full.names = TRUE)
t_cleaned <- map(txt_files, ~ read_file(.x)) %>% 
  set_names(nm = dir("./transcripts-cleaned"))

#### TIDY DATA ####

# Stop words
stop_words_it <- read_file("data/stopwords-it-topic-mod.txt") %>% 
  str_split(pattern = "\n") %>%
  unlist() %>%
  sort() %>%
  as_tibble() %>%
  rename(text = value) %>%
  arrange() %>%
  distinct()

write_lines(stop_words_it, 
            path = "./data/stopwords-it-topic-mod.txt", 
            sep = "\n")

# Tidy form one word per row
tidy_df <- map(t_cleaned, ~ tokenize_words(.x, stopwords = stop_words_it)) %>% 
  flatten() %>% 
  enframe() %>% 
  unnest(cols = value) %>% 
  mutate(name = str_remove_all(name, "(\\-.{11}.it.txt)")) %>% 
  rename(video_id = name, text = value) %>% 
  anti_join(stop_words_it) %>% 
  filter(!(text %in% c("michele", "boldrin")))

# Associate a number to each video 
document_number <- tidy_df %>% 
  distinct(video_id) %>% 
  mutate(document = row_number())

# Data set for topic modeling
tidy_df <- left_join(tidy_df, document_number, by = "video_id") 

# Word frequency in each video
word_frequency <- tidy_df %>% 
  count(video_id, text, document, sort = TRUE)

# Total number of words
total_number_words <- word_frequency %>% 
  distinct(text) %>% 
  count()

word_frequency %>% 
  group_by(text) %>% 
  summarise(total = sum(n)) %>% 
  arrange(total) %>% 
  print(n = Inf)

word_frequency %>% 
  distinct(text)

# Total number of words in each video
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

#write_csv(transcripts_words, path = "data/transcripts-words-df.csv")

#### TOPIC MODELING ####

dtm_df <- cast_dtm(tidy_df, term = text, document = video_id, value = document)

boldrin_lda <- LDA(dtm_df, k = 4, control = list(seed = 1234))

boldrin_topics <- tidy(boldrin_lda, matrix = "beta")

boldrin_top_terms <- boldrin_topics %>% 
  group_by(topic) %>% 
  top_n(20, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

boldrin_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  theme_bw() +
  geom_col(show.legend = F) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "The terms which are most common within each topic")

beta_spread <- boldrin_topics %>% 
  mutate(topic = str_c("topic", topic)) %>% 
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .0001 | topic2 > .0001) %>% 
  mutate(log_ratio = log2(topic2/topic1))

beta_spread %>% 
  top_n(10, log_ratio) %>% 
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  coord_flip()

#### PLOTS ####

# Good as exploratory analysis for topic modeling 
# The tf_idf statistic tells us how important it is the word in a certain 
# document compared to the others document in that collection

# Top 10 highest tf-idf words for each 66 Route videos
transcripts_words %>% 
  filter(str_detect(video_id, pattern = "Route")) %>% 
  mutate(text = factor(text, levels = unique(text)),
         text = reorder_within(text, tf_idf, video_id, fun = sum)) %>%
  group_by(video_id) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(text, tf_idf, fill = video_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, 
       y = "tf-idf",
       title = "Top 10 highest tf-idf words for each video of the series 'Da St Louis a Long Beach lungo la Route 66'",
       subtitle = "Notice: the tf-idf of this group of six video tells the importance of each word\n compared to the words in the entire collection.") +
  scale_x_reordered() +
  facet_wrap(~ video_id, ncol = 2, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
#ggsave("top-route66-tf-idf.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

# Plots with the term frequency distribution
transcripts_words %>% 
  filter(str_detect(video_id, "Route")) %>% 
  mutate(text = factor(text, levels = unique(text))) %>% 
  ggplot(aes(tf, fill = video_id)) +
  geom_histogram(show.legend = FALSE, bins = 35) +
  labs(title = "Term frequency distribution in the series 'Da St Louis a Long Beach lungo la Route 66'") +
  xlim(NA, 0.009) +
  facet_wrap(~ video_id, nrow = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("route66-tf.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")


#### BIGRAMS ####
# Bigrams: one token is made by two words
bigrams <- map(t_cleaned, ~ tokenize_ngrams(.x, n = 2)) %>% 
  flatten() %>% 
  enframe() %>% 
  unnest(cols = value) %>% 
  mutate(name = str_remove_all(name, "(\\-.{11}\\.it\\.txt$)")) %>% 
  #mutate(name = str_remove_all(name, ".+(?=.{11}$)")) %>% 
  rename(video_id = name) %>% 
  rename(bigram = value)

# Bigrams frequency
bigram_counts <- bigrams %>% 
  filter(str_detect(video_id, "Route")) %>% 
  count(video_id, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, video_id, n) %>% 
  arrange(desc(tf_idf))

bigram_counts %>% 
  mutate(bigram = factor(bigram, levels = unique(bigram)),
         bigram = reorder_within(bigram, tf_idf, video_id)) %>%
  group_by(video_id) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, tf_idf, fill = video_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, 
       y = "tf-idf",
       title = "Top 15 highest tf-idf bigram for each video of the series 'Da St Louis a Long Beach lungo la Route 66'") +
  scale_x_reordered() +
  facet_wrap(~ video_id, ncol = 2, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("route66-tf-idf-bigrams.png", path = "plots", width = 210, height = 297, units = "mm", dpi = 300, device = "png")

