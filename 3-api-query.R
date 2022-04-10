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
    purrr::flatten() %>% 
    discard(is_empty) %>% 
    map_if(is.data.frame, as.list) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(matches("token|published_at$|title$|description$|video_id|resource_id")) %>%
    mutate(across(contains("published_at"), ~ lubridate::ymd_hms(.x)))
}
videos_df <- hierarchichal_to_df(first_json) 
glimpse(videos_df)

uploads_id <- pull(channel_df, uploads_id)
flag_active_process <- TRUE
while(flag_active_process) {
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
  videos_df <- bind_rows(videos_df, current_videos_df)
  if (sum(str_detect(names(current_videos_df), "next_page_token")) != 0) {
    current_token <- unique(pull(current_videos_df, next_page_token))
    message("Fetched page with token: ", current_token)
  } else {
    flag_active_process <- FALSE
    message("Process completed!")
  }
}

videos_df
saveRDS(videos_df, str_c(lubridate::today(), "videos_df.rds",sep = "-"))

videos_id <- pull(videos_df, snippet_resource_id_video_id)
videos_response <- map(
  videos_id,
  function(x) {
    GET(
      url,
      path = "youtube/v3/videos",
      query = list(
        id = x,
        part = "snippet",
        part = "contentDetails",
        part = "statistics",
        key = Sys.getenv("YT_API")
      )
    )
  }
)

all_videos_raw <- videos_response %>% 
  map(~ content(.x, "text")) %>% 
  map(~ fromJSON(.x)) %>% 
  purrr::discard(is_empty) %>% 
  map(~ map_if(.x, .p = ~ is.data.frame(.x), .f = ~ as.list(.x))) %>% 
  map(as.data.frame) %>% 
  map(as_tibble) %>% 
  map_dfr(janitor::clean_names)

all_videos_raw <- all_videos_raw %>% 
  select(
    items_id,
    items_snippet_published_at,
    items_snippet_channel_id,
    items_snippet_title,
    items_snippet_description,
    items_snippet_channel_title,
    items_snippet_category_id,
    items_snippet_live_broadcast_content,
    items_snippet_default_language,
    items_snippet_default_audio_language,
    items_content_details_duration,
    items_content_details_dimension,
    items_content_details_definition,
    items_content_details_caption,
    items_content_details_licensed_content,
    items_statistics_view_count,
    items_statistics_favorite_count,
    items_statistics_comment_count,
    items_statistics_comment_count
  ) 
glimpse(all_videos_raw)

all_videos_raw %>% 
  mutate(items_snippet_published_at = lubridate::ymd_hms(items_snippet_published_at)) %>%
  mutate(across(matches("title$|description$"), str_squish)) %>% 
  mutate(across(contains("statistics"), as.numeric)) %>% 
  mutate(
    items_content_details_duration = str_remove(
    items_content_details_duration, 
    pattern = "^PT"
    )
  ) %>% glimpse()
  mutate(
    items_content_details_duration = lubridate::hms(
      items_content_details_duration
    )
  ) %>% 
  glimpse()

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
 