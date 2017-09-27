

d <- 1:5
d[d%%2==1]


library(dplyr)
library(stringr)

x <- "hi,hello"

length(unique(str_split(x, ",")[[1]]))


num_facs <- function(x){
  y = 1:(x-1)
  length(y[x%%y == 0])
}
num_facs <- Vectorize(num_facs)
arr <- c(3,6,7,9,21,100)
sum(arr[num_facs(arr) == 3])

