# if (!file.exists("cookbooks.zip")) {
#   download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
# }
# unzip("cookbooks.zip",exdir="cookbooks")

# devtools::install_github("mukul13/rword2vec")

pacman::p_load(tidyverse, rword2vec)

library(wordVectors)
library(softmaxreg)



rword2vec::bin_to_txt(bin_file = "GoogleNews-vectors-negative300.bin", txt_file = "google_word2vec.txt")


data <- as.data.frame(read.table("google_word2vec.txt",skip=1))

data("word2vec")

cosine_similarity <- function(x, ...){
  UseMethod("cosine_similarity", x)
}

cosine_similarity.list <-function(list_of_2){
  x <- list_of_2[[1]]
  y <- list_of_2[[2]]
  cosine_similarity(x, y)
}

cosine_similarity.numeric <- function(x, y){
  mag_x <- sqrt(sum(x^2))
  mag_y <- sqrt(sum(y^2))
  mat <- (x %*% y)/(mag_x * mag_y)
  mat[1, 1]
}

cosine_similarity(c(0,2,3), c(1,0,0))

cosine_similarity.data.frame <- function(df){
  df%>% 
    gather("col", "val", starts_with("col")) %>% 
    arrange(word, col) %>% 
    select(-col) %>% 
    group_by(word) %>% 
    nest() %>% 
    mutate(word_vec = map(data, ~.$val)) %>% 
    summarise(word = str_c(word, collapse = "-"),
              sim = cosine_similarity(word_vec))
}

word2vec %>% 
  filter(word == "tax") %>% 
  crossing(word2vec %>% 
             filter(word %in% c("government")))


word2vec %>% 
  filter(word %in% c("prince","princess")) %>% 
  cosine_similarity()
  
  
  
  
  
  
  
  
