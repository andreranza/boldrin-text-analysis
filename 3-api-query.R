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

parsed_resp <- content(response, "text")
json_resp <- jsonlite::fromJSON(parsed_resp)

channel_df <- json_resp %>%
  discard(is_empty) %>%
  unlist() %>%
  as_tibble(rownames = "names") %>%
  pivot_wider(names_from = names, values_from = value) %>%
  janitor::clean_names() %>%
  select(matches("statistics|uploads")) %>%
  rename(uploads_id = items_content_details_related_playlists_uploads)

write_csv(all_videos_df, str_c("data/", lubridate::today(), "-channel_df.csv"))
saveRDS(all_videos_df, str_c("data/", lubridate::today(), "-channel_df.rds"))

# Fetch video Ids ---------------------------------------------------------

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
    select(
      matches("token|published_at$|title$|description$|video_id|resource_id")
    ) %>%
    mutate(across(contains("published_at"), ~ lubridate::ymd_hms(.x)))
}

# Sequential API calls ----------------------------------------------------

videos_df <- hierarchichal_to_df(first_json)
uploads_id <- pull(channel_df, uploads_id)

flag_active_process <- TRUE
while (flag_active_process) {
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

# For each video Id fetch data --------------------------------------------

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

# Cleaning data -----------------------------------------------------------

all_videos_raw <- all_videos_raw %>%
  select(
    items_id,
    # The date and time that the video was published.
    # Note that this time might be different than the time
    # that the video was uploaded.
    items_snippet_published_at,
    items_snippet_channel_id,
    items_snippet_title,
    items_snippet_description,
    items_snippet_channel_title,
    items_snippet_category_id,
    items_snippet_live_broadcast_content,
    items_snippet_default_language,
    items_snippet_default_audio_language,
    # The property value is an ISO 8601 duration.
    items_content_details_duration,
    items_content_details_dimension,
    items_content_details_definition,
    # Indicates whether captions are available for the video
    items_content_details_caption,
    items_content_details_licensed_content,
    items_statistics_view_count,
    items_statistics_favorite_count,
    items_statistics_like_count,
    items_statistics_comment_count
  )

all_videos_df <- all_videos_raw %>%
  mutate(items_snippet_published_at = lubridate::ymd_hms(
    items_snippet_published_at)
  ) %>%
  mutate(across(matches("title$|description$"), str_squish)) %>%
  mutate(across(contains("statistics"), as.numeric)) %>%
  mutate(items_content_details_duration = lubridate::duration(
    items_content_details_duration
    )
  ) %>%
  mutate(link = str_c("https://www.youtube.com/watch?v=", items_id)) %>%
  arrange(items_snippet_published_at)

# Save data ---------------------------------------------------------------

write_csv(all_videos_df, str_c("data/", lubridate::today(), "-videos_df.csv"))
saveRDS(all_videos_df, str_c("data/", lubridate::today(), "-videos_df.rds"))

# links.txt is useful since youtube-dl has an option -a that allows to specify
# the path of a file containing the links.
# This prevent the user from using the bash script and looping over the links.
all_videos_df %>%
  pull(link) %>%
  write_lines(str_c("data/", lubridate::today(), "-links.txt"), sep = "\n")
