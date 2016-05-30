library(mclust)
library(dplyr)
library(ggplot2)

### Plot for theory slide

par(mfrow=c(1,1))
par(cex=1)
d1 = rnorm(200, mean=5, sd=1)
d2 = rnorm(100, mean=-2, sd=2)
hist(c(d1, d2), 
     breaks=20,
     col="grey",
     border=NA,
     xlab="Value",
     main="Mixed Distribution",
     cex=1.3)

x = seq(-6, 7, length.out=100)
dd1 = dnorm(x, mean=5, sd=1)
dd2 = dnorm(x, mean=-2, sd=2)

par(mfrow=c(1,2))
plot(x, dd2, type="l", bty="n", main="Distribution 1", 
     ylab="Probability Density", xlab="value", ylim=c(0,0.4))
plot(x, dd1, type="l", bty="n", main="Distribution 2", 
     ylab="Probability Density", xlab="Value", ylim=c(0,0.4))


load('./data/weather_data.Rdata')

### Two dimensional GMM


gmm = Mclust(df[c("temperature", 
                   "humidity")],
             G=seq(1,6))
             
plot(gmm, what="classification", bty="n")

gmm = densityMclust(df[c("temperature", 
                          "humidity")],
                    G=seq(1,6))
plot(gmm, what="density", bty="n", main="Density") 
                    
df %>% 
  mutate(score = as.numeric(gmm$density)) %>%
  arrange(score) %>%
  head(1)

df_scored = df %>%
  mutate(score = as.numeric(gmm$density))

ani.options(interval = 0.2,
            ani.width=600,
            ani.height=400)
saveGIF({
  breaks = seq(min(df_scored$score), max(df_scored$score), length.out=500)
  for(i in 1:25){
    plot(df$temperature, 
         df$humidity,
         pch=ifelse(df_scored$score <= breaks[i], 4, 20),
         col=ifelse(df_scored$score <= breaks[i], "red", "grey"),
         bty="n",
         xlab="temperature",
         ylab="humidity",
         main=paste("Threshold =", format(breaks[i], digits=2)))
  }
}, movie.name="mov.gif")

plot(df$temperature, 
     df$humidity,
     pch=20,
     col="grey",
     bty="n",
     xlab="temperature",
     ylab="humidity",
     size=-log10(df_scored$score),
     main=paste("Threshold =", format(breaks[i], digits=2)))

