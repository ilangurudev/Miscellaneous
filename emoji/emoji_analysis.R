library(tidytext)
library(rtweet)
library(tidyverse)
library(stringr)


# emoji_tables <- read_csv("emoticon_conversion_noGraphic.csv")
# names(emoji_tables) <- c("unicode", "bytes","description")
tweet_info <- as_tibble(get_timeline("LucyStats", n = 2000)) 


# tweet_info <- 
#   tweet_info %>%
#   filter(!str_detect(text,"RT")) %>%
#     mutate(text_clean = iconv(text, "latin1", "ASCII", sub=""),
#            text_len = str_length(text),
#            len_diff = str_length(text) - str_length(text_clean),
#            id = row_number()) 

#https://twitter.com/i/web/status/

(tweet_info <- 
  tweet_info %>%
    filter(!is_retweet) %>% 
    select(status_id,text,is_retweet) %>% 
    mutate(tweet_id = row_number(),
           text_no_names = str_replace_all(text,"@([A-Za-z0-9_]+)",""),
           text_no_names_url = str_replace_all(text_no_names,
                                               "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
                                               "")))

find_tweet <- function(id){
  tweet <- tweet_info %>%
    filter(tweet_id == id) 
  
  status_id <- tweet %>%
    pull(status_id)
  
  print(tweet$text)
  
  utils::browseURL(paste0("https://twitter.com/i/web/status/",status_id))
  
}


tweet_info %>%
  mutate(tokens = str_split(text_no_names_url," "),
         emoji_count = str_count(text_no_names_url,emoji_codes_regex)) %>%
           arrange(desc(emoji_count))
  


tweet_info %>%
  select(id, text_len, len_diff) %>%
  arrange(desc(len_diff))

tweet_info %>%
   select(id, text_len, len_diff) %>%
     summarise(text_len_sum = sum(text_len), emoji_sum = sum(len_diff)) %>%
       mutate(prop = emoji_sum/text_len_sum * 100)


emoji_codes <- 
  emoji_tables %>%
    pull(2)
  

emoji_codes_regex <- paste0("[",str_c(emoji_codes,collapse = "|"),"]")

