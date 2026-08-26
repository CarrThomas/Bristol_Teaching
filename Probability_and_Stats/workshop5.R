# Code for workshop 5

library(ggplot2)
library(dplyr)
library(tidyr)

# Question 1
data <- c(255.8, 252.0, 270.6, 200.0, 298.4, 362.3, 
          297.2, 266.8, 261.7, 247.5, 259.1, 220.6)
summary(data)
sort(data)
sd(data)
Z_score <- (data - mean(data)) /  sd(data)

# box and whisker plot
data.frame(x = data) %>% ggplot() +
  geom_boxplot(aes(x = x, col = "data")) +
  theme_bw() + 
  theme(legend.position = "null")

data.frame(x = data) %>% ggplot() +
  geom_boxplot(aes(x = x, col = "data")) +
  geom_point(aes(x = x, y = 0, col = "points")) +
  theme_bw() + 
  theme(legend.position = "null")

# manual box and whisker plot
# median 260.4
# 1st quartile 249.75
# 3rd quartile 283.9
# IQR 34.15
# 1.5% IQR 51.225
# whiskers 198.525, 335.125
# outliers 362.3

ggplot() +
  geom_segment(aes(x = x, xend = xend, y = y, col = "median"),
               data = data.frame(x = -2, xend = 2, y = 260.2)) +
  geom_rect(aes(xmin = -2, xmax = 2, ymin = 249.75, ymax = 283.9, 
                col = "median",
                fill = "white"), alpha = 0.2) +
  geom_segment(aes(x = -2, xend = 2, y = 198.525, 
                   col = "median")) +
  geom_segment(aes(x = -2, xend = 2, y = 335.125, 
                   col = "median")) +
  geom_segment(aes(x = 0, y = 198.525, yend = 335.125, 
                   col = "median")) +
  geom_point(aes(x = 0, y = 362.3, col = "data")) +
  theme_bw() +
  scale_fill_manual(values = "#619CFF") + 
  ylim(c(150, 400)) +
  xlim(c(-2.5,2.5)) +
  labs(x = "", y = "Average Weekly Income (£)") +
  theme(legend.position = "null") + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
  






