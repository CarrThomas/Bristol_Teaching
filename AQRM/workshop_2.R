# Simple script to generate simulated data and fit regressions
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(1234)

B <- c(0, 1)

N <- 100

# true DGP
data.frame(x = c(-1, 1), y = B[1] + B[2] * c(- 1, 1)) %>%
  ggplot() + 
  geom_line(aes(x = x, y = y, col = "true")) + 
  theme_bw() + 
  theme(legend.title = element_blank())

# low variance
X <- runif(N, -1, 1)
e <- rnorm(N, 0.5)
Y <- B[1] + X * B[2] + e

model <- lm(Y ~ X, data = data.frame(X = X, Y = Y))

data.frame(x = X, y = Y) %>%
  ggplot() + 
  geom_point(aes(x = x, y = y, col = "data")) +
  geom_abline(aes(intercept = 0, slope = 1, col = "true")) +
  geom_abline(aes(intercept = model$coefficients[1], slope = model$coefficients[2], col = "ols")) +
  theme_bw() + 
  theme(legend.title = element_blank())

# draw samples until we find one with a significant negative slope
stop <- 0
iter <- 0

while (stop == 0){
  
  iter <- iter + 1
  
  X <- runif(N, -1, 1)
  e <- rnorm(N, 0.5)
  Y <- B[1] + X * B[2] + e
  
  model <- lm(Y ~ X, data = data.frame(X = X, Y = Y))
  temp <- summary.lm(model)
  
  stop <- (temp$coefficients[2, 4] < 0.05) * (temp$coefficients[2, 1] < 0)
  
}

