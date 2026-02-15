library(tidyverse)
library(dslabs)
ds_theme_set()

#全部の変数を消す
rm(list=ls())


## simulate data 
N <- 100
Sigma <- matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means <- list(c(11,3), c(9,5), c(7,7), c(5,9), c(3,11))
dat <- lapply(means, function(mu) 
  MASS::mvrnorm(N, mu, Sigma))
dat<-as.data.frame(Reduce(rbind, dat)) %>%
  mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("Control", "Score", "School")

view(dat)

## First plot
dat %>% ggplot(aes(dat$Control,dat$Score)) + 
  geom_point(alpha = .5) +
  ggtitle(paste("correlation = ", round(cor(dat$Control, dat$Score), 2))) 

## 回帰直線をかく
dat %>% ggplot(aes(dat$Control,dat$Score)) +  
  geom_point(alpha = .5) +
  ggtitle(paste("correlation = ", round(cor(dat$Control, dat$Score), 2))) +
  geom_smooth(method = "lm")

## second plot
means <- as.data.frame(Reduce(rbind, means)) %>% setNames(c("dat$Control","dat$Score")) %>%
  mutate(z = as.character(seq_along(means)))

corrs <- dat %>% group_by(dat$School) %>% summarize(cor = cor(dat$Control,dat$Score)) %>% .$cor 

p <- dat %>% ggplot(aes(dat$Control,dat$Score, color = dat$School)) + 
  geom_point(show.legend = FALSE, alpha = 0.5) +
  ggtitle(paste("correlations =",  paste(signif(corrs,2), collapse=" ")))
p

## third plot
p + annotate("text", x = dat$Control, y = dat$Score, 
             label = paste("Z=", means$z), cex = 5)  

