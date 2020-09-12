library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

uri <- "https://www.googleapis.com/youtube/v3"
resource <- "/channels"
pars <- "contentDetails%2Cstatistics"
name <- "MicheleBoldrin"

res_ch <- GET(str_c(uri, 
                    resource,
                    "?part=", 
                    pars, 
                    "&forUsername=", 
                    name, 
                    "&key=", 
                    Sys.getenv("YT_API"))
)

http_status(res_ch)$message

# Retrieve the contents of the request as a character vector in JSON format
ch_parsed <- content(res_ch, "text")

# Convert the JSON into a nested list
json_ch <- fromJSON(ch_parsed)

# Channel Id
ch_id <- json_ch$items$id

# Upload Id
up_id <- json_ch$items$contentDetails$relatedPlaylists$uploads

# Total number of videos
tot_vids <- json_ch$items$statistics$videoCount

# Total number of subscribers
tot_subs <- json_ch$items$statistics$subscriberCount

# Total channel views
tot_view <- json_ch$items$statistics$viewCount

info <- tibble(
  Info = c("Ch.Name", "N.Videos", "N.Views", 
           "N.Subs", "Upload Playlist Id"),
  Data = c("Michele Boldrin", tot_vids, 
           tot_view, tot_subs, ch_id)
) 

resource2 <- "/playlistItems"
pars2 <- "contentDetails%2C%20snippet%2C%20id&maxResults=50"
res_items <- GET(str_c(uri,
                       resource2,
                       "?part=", 
                       pars2, 
                       "&playlistId=", 
                       up_id, 
                       "&key=", 
                       Sys.getenv("YT_API"))
)

http_status(res_items)
prettify(res_items)

# Parse the content from JSON data
items_parsed <- content(res_items, "text")
playlists_items <- fromJSON(items_parsed)

# First next page token 
next_token <- playlists_items$nextPageToken

# Initialize data set: title, video Id, upload time
details <- as_tibble(playlists_items$items$contentDetails)
titles <- as_tibble(playlists_items$items$snippet$title)

df <- bind_cols(titles, details)

# Grow df by recursive calls to the API resource
while(TRUE) {
  
  if (nrow(df) < tot_vids) {
    # Call the API
    r_items <- GET(str_c(uri,
                         resource2,
                         "?part=", 
                         pars2, 
                         "&pageToken=", 
                         tail(next_token, n = 1), 
                         "&playlistId=", 
                         up_id, 
                         "&key=", 
                         Sys.getenv("YT_API"))
    )
    # Parsing
    p_items <- content(r_items, "text")
    play_items <- fromJSON(p_items)
    
    # Concatenate next 50 videos
    det <- as_tibble(play_items$items$contentDetails)
    tit <- as_tibble(play_items$items$snippet$title)
    df <- bind_rows(df, bind_cols(tit, det))
    
    # Update next page token to get info on next 50 videos
    next_token <- c(next_token, play_items$nextPageToken)
    print(next_token)
  } else {
    break
  }
}

df <- df %>% 
  distinct(videoId, .keep_all = TRUE) %>% 
  rename(vidTitle = value)

resource3 <- "/videos"
pars3 <- "snippet%2CcontentDetails%2Cstatistics"

res_vid <- vector(mode = "list", length = nrow(df))
for (i in seq_along(df$videoId)) {
  res_vid[[i]] <- GET(str_c(uri,
                            resource3, 
                            "?part=", 
                            pars3, 
                            "&id=", 
                            df$videoId[i], 
                            "&key=", 
                            Sys.getenv("YT_API"))
  )
}

videos <- map(res_vid, ~ content(.x, "text"))

# JSON example
prettify(videos[[100]])

# Views
n_views <- map_chr(videos, ~ fromJSON(.x)$items$statistics$viewCount)

# Likes
n_likes <- map_chr(videos, ~ fromJSON(.x)$items$statistics$likeCount)

# Dislikes
n_dislikes <- map_chr(videos, ~ fromJSON(.x)$items$statistics$dislikeCount)

# Comments: deactivated comments are returned as NULL 
n_comments <- map(videos, ~ fromJSON(.x)$items$statistics$commentCount) %>% 
  modify_if(is.null, ~ NA)
n_comments <- unlist(n_comments)

# Description
description <- map_chr(videos, ~ fromJSON(.x)$items$snippet$description) %>% 
  map_chr(~ na_if(.x,""))

# Video length
vid_duration <- map(videos, ~ fromJSON(.x)$items$contentDetails$duration) %>% 
  map_chr(~ str_remove_all(.x, pattern = "PT|\\d{1,2}S$")) %>% 
  map_chr(~ str_replace_all(.x, pattern = "(?<=H)(?=\\d)", replacement = " ")) %>% 
  map_dbl(~ duration(.x)/60)

# Tags
tags <- map(videos, ~fromJSON(.x)$items$snippet$tags) %>% 
  modify_if(is.null, as.character) %>% 
  modify_depth(1, unlist) %>% 
  modify_if(~ length(.x) == 0, ~ NA) %>% 
  map_chr(~ str_c(.x, collapse = ", "))

# Dataset
data <- df %>% 
  mutate(n_views = as.double(n_views),
         n_likes = as.double(n_likes),
         n_comments = as.double(n_comments),
         n_dislikes = as.double(n_dislikes),
         vid_min = as.double(vid_duration),
         like_ratio = round((n_likes/n_views)*100, 2),
         dislike_ratio = round((n_dislikes/n_views)*100, 2),
         release_date = date(videoPublishedAt),
         vid_age = today() - release_date,
         description = description,
         link = str_c("https://www.youtube.com/watch?v=", videoId),
         tags = tags) %>% 
  select(-videoPublishedAt) %>% 
  arrange(desc(release_date))
View(data)
print(data, n = 10)
glimpse(data)

write_csv(data, "boldrin-videos.csv")

# links.csv useful if we run the bash script to loop over the link columns
# in order to download the scripts.

data %>% 
  select(videoId, link) %>% 
  write_csv("data/links.csv")

# links.txt is useful since youtube-dl has an option -a that allows to specify
# the path of a file containing the links.
# This prevent the user from using the bash script and looping over the links.
data %>% 
  select(link) %>% 
  pull() %>% 
  write_lines("data/links.txt", sep = "\n")
 