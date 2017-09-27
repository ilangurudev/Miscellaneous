library(tidytext)
library(rtweet)
library(tidyverse)
library(stringr)

tweet_info <- as_tibble(get_timeline("LucyStats", n = 2000)) 

find_tweet <- function(id){
  tweet <- tweet_info %>%
    filter(tweet_id == id) 
  
  status_id <- tweet %>%
    pull(status_id)
  
  print(tweet$text)
  
  utils::browseURL(paste0("https://twitter.com/i/web/status/",status_id))
  
}

emoji_tables <- read_delim("emDict.csv",delim = ";") %>%
  rename(r_enc = `R-encoding`)

emoji_regex <- str_c(emoji_tables$r_enc,collapse = "|")


tibble(id = 1:25,
       org = tweet_info$text[1:25]) %>%
  mutate(x = iconv(org, from = "latin1", to = "ascii", sub = ""),
         y = iconv(org, from = "ascii", to = "latin1", sub = ""))

(tweet_info <- 
    tweet_info %>%
    filter(!is_retweet) %>% 
    select(status_id,text,is_retweet) %>% 
    mutate(tweet_id = row_number(),
           text_no_names = str_replace_all(text,"@([A-Za-z0-9_]+)",""),
           text_no_names_url = str_replace_all(text_no_names,
                                               "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
                                               ""),
           text_emoji = iconv(text_no_names_url, from = "latin1", to = "ASCII", sub = "byte")))

tweet_info %>%
  mutate(emoji_count = str_count(text_emoji, emoji_regex)) %>%
    select(tweet_id, emoji_count) %>%
      arrange(desc(emoji_count))



