pacman::p_load(gtrendsR, tidyverse, plotly, prophet)

trends <- 
  gtrends(c("Artificial Intelligence", "Data Science", "Machine Learning", "Bitcoin", "Blockchain"), time = "all")

saveRDS(trends, "trends.RDS")
trends <- readRDS("trends.RDS")

glimpse(trends)

p <- 
trends$interest_over_time %>% as_tibble() %>% 
mutate(hits = if_else(hits == "<1", "0.5", hits) %>% as.double()) %>% 
arrange(date) %>% 
ggplot(aes(date, hits, col = keyword)) +
geom_line() 

ggplotly(p)

trends$related_topics
trends$related_queries %>% 
  filter(keyword == "Machine Learning")


plot_seasonality <- function(term){
  trends <- 
    gtrends(term, time = "all")
  
  seasonal_data <- 
    trends$interest_over_time %>% 
    select(ds = date, y = hits) %>% 
    as_tibble() %>% 
    mutate(y = if_else(y == "<1", "0.5", as.character(y)) %>% as.double())
 
  m <- prophet(seasonal_data, changepoint.prior.scale = 1)
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  # plot(m, forecast)
  
  prophet_plot_components(m, forecast)
  
}

plot_seasonality("Christmas")

plot_seasonality("Daylight Savings")
