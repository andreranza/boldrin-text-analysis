library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

url <- "https://www.googleapis.com/"
user_name <- "MicheleBoldrin"

# Fetch channel data and playlistItems ID ---------------------------------

response <- GET(
  url,
  path = "youtube/v3/channels",
  query = list(
    forUsername = user_name, 
    part = "statistics", 
    part = "contentDetails", 
    key = Sys.getenv("YT_API")
    )
)
status <- httr::http_status(response)
if (!str_detect(status[["message"]], "200")) {
  stop("Bad request")
}

# Retrieve the contents of the request as a character vector in JSON format
parsed_resp <- content(response, "text")

# Convert the JSON into a nested list
json_resp <- jsonlite::fromJSON(parsed_resp)

channel_df <- json_resp %>% 
  discard(is_empty) %>% 
  unlist() %>% 
  as_tibble(rownames = "names") %>% 
  pivot_wider(names_from = names, values_from = value) %>% 
  janitor::clean_names() %>% 
  select(matches("statistics|uploads")) %>% 
  rename(uploads_id = items_content_details_related_playlists_uploads)

channel_df
names(channel_df)
# Fetch data regarding videos ---------------------------------------------

videos_resp <- GET(
  url,
  path = "youtube/v3/playlistItems",
  query = list(
    forUsername = "MicheleBoldrin", 
    playlistId = pull(channel_df, uploads_id),
    part = "contentDetails", 
    part = "snippet",
    maxResults = 50, 
    key = Sys.getenv("YT_API")
    )
  )


status <- http_status(videos_resp)
if (!str_detect(status[["message"]], "200")) {
  stop("Bad request")
}

# Parse the content from JSON data
items_parsed <- content(videos_resp, "text")
first_json <- fromJSON(items_parsed)

hierarchichal_to_df <- function(json_response) {
  json_response %>% 
    discard(is_empty) %>% 
    map_if(is.data.frame, as.list) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(matches("token|published_at$|title$|description$|video_id")) %>%
    mutate(across(contains("published_at"), ~ lubridate::ymd_hms(.x))) 
}

videos_df <- hierarchichal_to_df(first_json) 
videos_df 
uploads_id <- pull(channel_df, uploads_id)
tot_videos <- pull(channel_df, items_statistics_video_count)

floor(900/50) -1

current_response <- GET(
  url,
  path = "youtube/v3/playlistItems",
  query = list(
    forUsername = "MicheleBoldrin",
    playlistId = uploads_id,
    pageToken ="EAAaB1BUOkNJUUg",
    part = "snippet",
    maxResults = 50,
    key = Sys.getenv("YT_API")
  )
)

# Grow df by recursive calls to the API resource
while(TRUE) {
  if (nrow(videos_df) < tot_videos) {
    current_response <- GET(
      url,
      path = "youtube/v3/playlistItems",
      query = list(
        forUsername = "MicheleBoldrin",
        playlistId = uploads_id,
        pageToken = unique(pull(videos_df, next_page_token)) %>% .[length(.)],
        part = "snippet",
        maxResults = 50,
        key = Sys.getenv("YT_API")
      )
    )
    current_parsed <- content(current_response, "text")
    current_json <- fromJSON(current_parsed)
    current_videos_df <- hierarchichal_to_df(current_json)
    print(unique(pull(current_videos_df, next_page_token)))
    videos_df <- bind_rows(videos_df, current_videos_df)
  } else {
    break
  }
}

videos_df
saveRDS(videos_df, str_c(lubridate::today(), "videos_df.rds",sep = "-"))

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

write_csv(data, "./data/boldrin-videos.csv")

# links.csv useful if we run the bash script to loop over the link columns
# in order to download the scripts.

data %>% 
  select(videoId, link) %>% 
  write_csv("./data/links.csv")

# links.txt is useful since youtube-dl has an option -a that allows to specify
# the path of a file containing the links.
# This prevent the user from using the bash script and looping over the links.
data %>% 
  select(link) %>% 
  pull() %>% 
  write_lines("./data/links.txt", sep = "\n")
 