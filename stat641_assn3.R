library(purrr)
library(tidyverse)
library(forcats)

calc_area <- function(f,...){
  f(1:4,...) - f(-(1:4),...)
}


calc_area(pnorm)
calc_area(plaplace)
calc_area(pcauchy)
calc_area(plogis)


frac_ord <- function(d, vec){
  vec <- sort(vec)
  x <- d * (length(vec) + 1)
  y = x %% 1
  z = 1 - y
  (z * vec[floor(x)]) + (y * vec[floor(x) + 1])
}


b_add <- c(151, 450, 124, 235, 357, 110, 302, 671, 118, 115, 275, 275, 2550, 243, 201, 199,
130, 119, 92, 91, 92, 98, 1650, 1200, 1180, 900, 700, 460, 340, 330)

b_w_add <- c(246, 268, 275, 348, 305, 311, 206, 279, 426, 269, 257, 299, 337, 329, 319, 312,
             327, 342, 351, 205, 151, 426, 154, 353, 396, 441, 254, 263, 278, 281)

b_add_sorted <- sort(b_add)
b_w_add_sorted <- sort(b_w_add)
length(b_add)

frac_ord(0.25, b_add)
frac_ord(0.5, b_add)
frac_ord(0.75, b_add)



kernel_density <- function(x, vec, h){
  u <- (x - vec)/h
  k_u <- (1/sqrt(2 * pi)) * exp(-((u^2)/2))
  k_u
}

kernel_density_sum <- function(x, vec, h){
  u <- (x - vec)/h
  k_u <- (1/sqrt(2 * pi)) * exp(-((u^2)/2))
  (1 / (length(vec) * h)) * sum(k_u)
}

# kernel_density_sum <- Vectorize(kernel_density_sum)

kernel_density_sum(100,b_add_sorted, 135)
kernel_density_sum(1500,b_add_sorted, 135)

b_add_sorted[kernel_density(1000, b_add_sorted, 135) == min(kernel_density(1000, b_add_sorted, 135))]
b_add_sorted[kernel_density(1000, b_add_sorted, 135) == max(kernel_density(1000, b_add_sorted, 135))]

make_tibble <- function(vec, x, vec_name){
  tibble(x = 0:x, 
         pdf = map_dbl(x,~kernel_density_sum(., vec, 135))) %>%
           mutate(vec_n = vec_name)
}

mt <- 
make_tibble(b_add_sorted, 3000, "b_add") %>%
  bind_rows(make_tibble(b_w_add, 3000, "b_w_add")) %>%
  group_by(vec_n) %>%
  arrange(vec_n,x) %>% 
  mutate(cdf = cumsum(pdf))
  
  
  
mt %>% 
  ggplot(aes(x, pdf, col = fct_rev(vec_n))) + geom_line()
ggsave("pdf.pdf")

mt %>% 
  ggplot(aes(x, cdf, col = fct_rev(vec_n))) + geom_line()
ggsave("cdf.pdf")


frac_ord <- function(d, vec){
  vec <- sort(vec)
  x <- (d * length(vec)) + 0.5
  y = x %% 1
  z = 1 - y
  (z * vec[floor(x)]) + (y * vec[floor(x) + 1])
}


prob <- seq(0.02,0.98,0.01)
map_dbl(prob,~frac_ord(.,b_add_sorted))
tibble(prob = prob,
       q_add = map_dbl(prob,~frac_ord(.,b_add_sorted)),
       q_w_add = map_dbl(prob,~frac_ord(.,b_w_add_sorted))) %>%
  ggplot(aes(x = prob)) +
  geom_line(aes(y = q_add), col = "darkgreen") +
  geom_line(aes(y = q_w_add), col = "red")
ggsave("quantile.pdf")
