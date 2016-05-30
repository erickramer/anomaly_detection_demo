library(RColorBrewer)
library(dplyr)
library(purrr)

load("./data/weather_data.Rdata")

d = df %>%
  select(-date, -city)

cols = brewer.pal(6, "Set2")
par(mfrow=c(2,3))
par(cex=1)
map(1:6, function(i){
  hist(d[[i]], 
       col=cols[[i]],
       breaks=30,
       main=names(d)[i],
       xlab=names(d)[i],
       cex=1.3)
})