library(dplyr)
library(tidyr)
library(mclust)
library(purrr)

map = purrr::map # make sure this is coming from the right package

load("./data/weather_data.Rdata")

df2 = df %>%
  mutate(wind_speed = log1p(wind_speed)) %>%
  filter(wind_speed > 0)

train_gmm = function(df){
  densityMclust(df[c("temperature", 
                     "humidity",
                     "wind_speed",
                     "pressure")],
                G=seq(1,6))
}


gmms = df2 %>%
  nest(-city) %>%
  mutate(gmm = map(data, train_gmm))

scores = gmms %>%
  mutate(score = map(gmm, "density")) %>%
  select(-gmm) %>%
  unnest() %>%
  arrange(score) %>%
  mutate(wind_speed = expm1(wind_speed))

par(cex=1.3)

#### Making gif


plot1 = function(name, threshold){
  
  dfs = gmms$data
  scores = map(gmms$gmm, "density")
  
  names(dfs) = gmms$city
  names(scores) = gmms$city
  
  df = dfs[[name]]
  score = scores[[name]]
  
  plot(df[c("temperature", 
            "humidity",
            "wind_speed",
            "pressure")],
       pch=ifelse(score <= threshold, 4, 20),
       col=ifelse(score <= threshold, "red", "grey"),
       main=name)
}

par(mfrow=c(1,3))

library(animation)

ani.options(interval = 0.2)

saveGIF({
  par(mfrow=c(1,3))
  breaks = seq(min(scores$score), max(scores$score), length.out=2500)
  for(i in 1:25){
    plot1("Paris", breaks[i])
  }
}, movie.name="mov3_paris.gif")


saveGIF({
  par(mfrow=c(1,3))
  breaks = seq(min(scores$score), max(scores$score), length.out=2500)
  for(i in 1:25){
    plot1("London", breaks[i])
  }
}, movie.name="mov3_london.gif")

saveGIF({
  par(mfrow=c(1,3))
  breaks = seq(min(scores$score), max(scores$score), length.out=2500)
  for(i in 1:25){
    plot1("New York", breaks[i])
  }
}, movie.name="mov3_nyc.gif")

