library(dplyr)
library(tidyr)
library(mclust)

map = purrr::map # make sure this is coming from the right package

load("./data/weather_data.Rdata")

gmms = df %>%
  nest(-city) %>%
  mutate(gmm = map(data, train_gmm))

train_gmm = function(df){
  densityMclust(df[c("temperature", "humidity")],
                  G=seq(1,6))
}

scores = gmms %>%
  mutate(score = map(gmm, "density")) %>%
  select(-gmm) %>%
  unnest() %>%
  arrange(score) 

### Function for making GIF

plot3 = function(threshold){
  plot1 = function(name){
    df = dfs[[name]]
    score = scores[[name]]
    
    plot(df$temperature, 
         df$humidity,
         bty="n",
         xlab="temperature",
         ylab="humidity",
         pch=ifelse(score <= threshold, 4, 20),
         col=ifelse(score <= threshold, "red", "grey"),
         main=name,
         xlim=c(-15, 35),
         ylim=c(30, 100))
  }
  
  dfs = gmms$data
  scores = map(gmms$gmm, "density")
  
  names(dfs) = gmms$city
  names(scores) = gmms$city
  
  map(names(dfs)[c(1,3,2)], plot1)
}

saveGIF({
  par(mfrow=c(1,3))
  breaks = seq(min(scores$score), max(scores$score), length.out=500)
  for(i in 1:25){
    plot3(breaks[i])
  }
}, movie.name="mov2.gif")
