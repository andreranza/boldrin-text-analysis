library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(ggExtra)
library(plotly)

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

ch_parsed <- content(res_ch, "text")
json_ch <- fromJSON(ch_parsed)
ch_id <- json_ch$items$id
up_id <- json_ch$items$contentDetails$relatedPlaylists$uploads
tot_vids <- json_ch$items$statistics$videoCount
tot_subs <- json_ch$items$statistics$subscriberCount
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

# Parse the content from JSON data
items_parsed <- content(res_items, "text")
playlists_items <- fromJSON(items_parsed)

# First next page token 
next_token <- playlists_items$nextPageToken

# Initialize data set: title, video Id, upload time
details <- as_tibble(playlists_items$items$contentDetails)
titles <- as_tibble(playlists_items$items$snippet$title)

df <- bind_cols(titles, details)

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

# Views
n_views <- map_chr(videos, ~ fromJSON(.x)$items$statistics$viewCount)

# Likes
n_likes <- map_chr(videos, ~ fromJSON(.x)$items$statistics$likeCount)

# Dislikes
n_dislikes <- map_chr(videos, ~ fromJSON(.x)$items$statistics$dislikeCount)

glimpse(df)

# Dataset
data <- df %>% 
  mutate(n_views = as.double(n_views),
         n_likes = as.double(n_likes),
         n_dislikes = as.double(n_dislikes),
         like_ratio = round((n_likes/n_views)*100, 2),
         dislike_ratio = round((n_dislikes/n_views)*100, 2),
         release_date = date(videoPublishedAt),
         vid_age = today() - release_date) %>% 
  select(-videoPublishedAt)
print(data, n = Inf)
glimpse(data)
#write_csv(data, "boldrin-videos.csv")

