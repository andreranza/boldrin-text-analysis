library(tidyverse)

# files <- list.files("transcripts", pattern = ".vtt", full.names = TRUE)
# new_files <- map_chr(files, ~ str_replace_all(.x, pattern = ".vtt$", replacement = ".txt"))
# file.rename(files, new_files)

txt_files <- dir("./transcripts")

# Filter only the txt files
txt_files <- keep(txt_files, ~ str_detect(.x, ".txt$"))

# Check: should be TRUE
sum(map_lgl(txt_files, ~ str_detect(.x, ".txt$"))) == length(txt_files)

setwd("./transcripts")
transcripts <- map(txt_files, ~ read_file(.x))
setwd("../")

p0 <- "0\\d\\:.+\\%"
p1 <- "[:alpha:]+\\<0\\d\\:.+c\\>"
p2 <- "WEBVTT"
p3 <- "Kind\\:\\scaptions"
p4 <- "Language\\:\\sit"
p5 <- "\\s\\\n\\n\\n[:alpha:]+\\n"
p6 <- "0\\d\\:.+\\d{3}"
p7 <- "(\\<\\>\\<c\\>)|(\\<\\/c\\>)|(l\\')|(un\\')|(d\\')"

# better to make a long regex and iterate once or smaller and iterate more than once?
transcripts_cleaned <- map(transcripts, ~ str_remove_all(.x, pattern = p0)) %>% 
  map(~ str_remove_all(.x, pattern = p1)) %>% 
  map(~ str_remove_all(.x, pattern = p2)) %>% 
  map(~ str_remove_all(.x, pattern = p3)) %>% 
  map(~ str_remove_all(.x, pattern = p4)) %>% 
  map(~ str_remove_all(.x, pattern = p5)) %>% 
  map(~ str_remove_all(.x, pattern = p6)) %>% 
  map(~ str_remove_all(.x, pattern = p7)) %>% 
  map(~ str_split(.x, pattern = "\n")) %>% 
  flatten() %>% 
  map(~ unique(.x)) %>% 
  map(~ str_c(.x, collapse = " ")) %>% 
  map(~ str_remove_all(.x, pattern = "(^\\s{3})|(\\[Musica\\])"))
  
names(transcripts_cleaned) <- txt_files

for(i in seq_along(transcripts_cleaned)) {
  write_file(transcripts_cleaned[[i]], 
             path = str_c("./transcripts-cleaned/", txt_files[i]))
}

