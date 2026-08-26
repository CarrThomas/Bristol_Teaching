# Script to make a figure for Workshop 7

rm(list = ls())
gc()

library(ggplot2)
library(tidyr)

data <- data.frame("CS" = seq.int(0, 50))
data$Score <- 640.3 - data$CS * 4.93

data %>% ggplot() + 
  geom_line(aes(x = CS, y = Score, col = "ols")) + 
  geom_line(aes(x = x, y = y, col = "mean"), 
            data = data.frame(x = c(22.8, 22.8),
                              y = c(0, 527.89)),
            linetype = "dashed") +
  geom_line(aes(x = x, y = y, col = "mean"), 
            data = data.frame(x = c(0, 22.8),
                              y = c(527.89, 527.89)),
            linetype = "dashed") +
  theme_bw() + 
  labs(x = "X", y = "Y") + 
  ylim(c(0, 800)) +
  theme(legend.position = "null")


