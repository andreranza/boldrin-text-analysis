library(tidyverse)
library(lubridate)
library(plotly)
library(GGally)
library(tokenizers)
data <- read_csv("boldrin-videos.csv")
theme_set(theme_bw())

data %>% 
  filter(n_views > 25000) %>% 
  ggplot(aes(x = reorder(vidTitle, n_views), y = n_views)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(x = "Title", 
       y = "N. of views",
       title = "Videos with over 25k views from 'Michele Boldrin' YouTube channel",
       subtitle = str_c("Data retrieved on ", today()),
       caption = "Source: YouTube Data API") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("over25k.png", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

ggplot(data, aes(y = like_ratio, x = dislike_ratio)) +
  geom_point(size = 0.75, shape = 20) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggtitle("Michele Boldrin YT Channel") +
  labs(y = "Likes / Number of Views (%)", 
       x = "Dislikes / Number of Views (%)",
       title = "Videos from 'Michele Boldrin' YouTube Channel",
       subtitle = str_c("Data retrieved on ", today()),
       caption = "Source: YouTube Data API") 
  #coord_cartesian(xlim = c(0,15), ylim = c(0,15))

data %>% 
  filter(like_ratio > 10) %>% 
  ggplot(aes(x = reorder(vidTitle, like_ratio), y = like_ratio)) +
  geom_col() +
  coord_flip(ylim = c(10, 16)) +
  theme_bw() +
  labs(title = "Videos with over 10% of likes on views from 'Michele Boldrin' YouTube channel",
       subtitle = str_c("Data retrieved on ", today()),
       x = "Title",
       y = "(%) Likes/Views",
       caption = "Source: YouTube Data API") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
#ggsave("over10percent.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

scatter <- ggplot(data, aes(y = like_ratio, x = dislike_ratio)) +
  geom_point(size = 0.15, shape = 20) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.9,0.9),
        legend.background = element_rect(colour = "black")) +
  ggtitle("Michele Boldrin YT Channel") +
  labs(y = "Likes / Number of Views (%)", 
       x = "Dislikes / Number of Views (%)",
       title = "Videos from 'Michele Boldrin' YouTube Channel",
       subtitle = str_c("Data retrieved on ", today()),
       caption = "Source: YouTube Data API") 
  #coord_cartesian(xlim = c(0,15), ylim = c(0,15))
ggsave("scatter.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

my_text <- str_c("Title: ", data$vidTitle, "\n", 
                 "Views: ", data$n_views, "\n",
                 "Likes: ", data$n_likes, "\n",
                 "Like (%): ", data$like_ratio, "\n",
                 "Dislikes: ", data$n_dislikes, "\n",
                 "Dislike (%) ", data$dislike_ratio, "\n")
pp <- plotly::plotly_build(scatter)
style(pp, text = my_text, hoverinfo = "text")

data %>% 
  mutate(date = str_c(year(release_date), ".", 
                      str_pad(month(release_date), 
                              width = 2, 
                              side = "left", 
                              pad = "0"))) %>% 
  group_by(date) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Month",
       y = "Count",
       title = "Number of videos released monthly on 'Michele Boldrin' YouTube channel",
       subtitle = str_c("Data retrieved on ", today()),
       caption = "Source: YouTube Data API")
#ggsave("monthly-videos.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

glimpse(data)

#### WORDS TITLE ####
raw_words <- unlist(map(data$vidTitle, ~ tokenize_words(.x)))

stop_it <- c(stopwords::stopwords("it"), "cosa", "po", "fra", "perche", "ii",
             "iii", "de", "ovvero", "fa", "dopo", "due", "l'angolo")
stop_en <- c(stopwords::stopwords("en"), "two", "vs")

as_tibble(raw_words) %>% 
  group_by(value) %>% 
  filter(!(value %in% c(stop_it, stop_en))) %>% 
  filter(str_detect(value, pattern = "\\D")) %>% 
  #count(value, sort = TRUE) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 20) %>% 
  #filter(count < 20 & count > 5) %>% 
  ggplot(aes(x = reorder(value, count), y = count)) +
  theme_bw() +
  geom_col() +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(y = "Count",
       x = "Words",
       title = "Term frequency in video titles of 'Michele Boldrin' YouTube channel",
       subtitle = str_c("Data retrived on ", today()),
       caption = "Source: YouTube Data API")
ggsave("term-frequency.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

tibble(Count = as_factor(as.double(diff(data$vid_age)))) %>% 
  ggplot(aes(x = Count)) +
  geom_bar() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Delta Days",
       y = "Count",
       title = "Difference in days between consecutive videos on 'Michele Boldrin' YouTube Channel",
       subtitle = str_c("Data retrived on ", today()),
       caption = "Source: YouTube Data API")

tibble(Count = as.double(diff(data$vid_age)), 
       Delta_Views = diff(data$n_views)) %>% 
  filter(Count < 8) %>% 
  mutate(Count = as_factor(Count)) %>% 
  ggplot(aes(x = Count, y = Delta_Views)) +
  geom_boxplot(outlier.size = 0.5,
               size = 0.5) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(-15000, 15000)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Delta Days",
       y = "Delta Views",
       title = "Relationship between variation in views consecutive videos on 'Michele Boldrin' YouTube Channel",
       subtitle = str_c("Data retrived on ", today()),
       caption = "Source: YouTube Data API")
glimpse(data)
data %>% 
  ggplot(aes(x = vid_min, y = n_views)) +
  geom_point(size = 0.15, shape = 20) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggtitle("Michele Boldrin YT Channel") +
  labs(y = "Number of Views", 
       x = "Video Length (Min.)",
       title = "Videos from 'Michele Boldrin' YouTube Channel",
       subtitle = str_c("Data retrieved on ", today()),
       caption = "Source: YouTube Data API") 

data %>% 
  select_if(is.numeric) %>% 
  selec
ggsave("scatter_views_length.png", path = "plots", width = 297, height = 210, units = "mm", dpi = 300, device = "png")

data %>% 
  select(n_views, n_likes, n_comments, n_dislikes, vid_min) %>% 
  ggpairs(lower = list(continuous = wrap("points", size = 0.25))) 


