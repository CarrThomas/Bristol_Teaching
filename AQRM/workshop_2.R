# Simple script to generate simulated data and fit regressions
rm(list = ls())
gc()

path <- getwd()
figure_path <- paste0(path, "/AQRM/")

library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(1234)

B <- c(0, 1)

# true DGP
data.frame(x = c(-1, 1), y = B[1] + B[2] * c(- 1, 1)) %>%
  ggplot() + 
  geom_line(aes(x = x, y = y, col = "true")) + 
  theme_bw() + 
  theme(legend.position = "null")

# draw samples until we find one with a significant negative slope
N <- 100
stop <- 0
N_samples <- 10000
B2_store <- rep(0, N_samples)

full_results <- vector(mode = "list", length = 6)

for (i in 1:N_samples){
  
  X <- runif(N, -1, 1)
  e <- rnorm(N, sd = 2)
  Y <- B[1] + X * B[2] + e
  
  model <- lm(Y ~ X, data = data.frame(X = X, Y = Y))
  B2_store[i] <- model$coefficients[2]
  
   if(i <= 5){
     
    temp <- summary.lm(model)
    full_results[[i]] <- list(
      sample = i,
      data = data.frame(x = X, y = Y),
      B = temp$coefficients
    )
    
   }
  
  if (stop == 0){
    if (model$coefficients[2] < 0){
      
      temp <- summary.lm(model)
      full_results[[6]] <- list(
        sample = i,
        data = data.frame(x = X, y = Y),
        B = temp$coefficients
      )
      
      stop <- 1
      
    }
  }
}

# plots of example draws
# X <- runif(N, -1, 1)
# e <- rnorm(N, sd = 2)
# Y <- B[1] + X * B[2] + e

# model <- lm(Y ~ X, data = data.frame(X = X, Y = Y))
# summary.lm(model)

# get limits
min_y <- 0
max_y <- 0
for (i in 1:length(full_results)){
  
  min_y <- min(min_y, min(full_results[[i]]$data[, 2]))
  max_y <- max(max_y, max(full_results[[i]]$data[, 2]))
  
}


for (i in 1:6){
  
  # png(filename = paste0(figure_path, "plot_", i, ".png"), 
  #     width = 1150,
  #     height = 752)
  #
  
  pdf(file = paste0(figure_path, "plot_", i, ".pdf"), 
      width = 10.5)

  print(full_results[[i]]$data %>% 
          ggplot() +
          geom_point(aes(x = x, y = y, col = "data")) +
          geom_abline(aes(intercept = 0, slope = 1, col = "true parameters")) +
          geom_abline(aes(intercept = full_results[[i]]$B[1, 1], 
                          slope = full_results[[i]]$B[2, 1], 
                          col = "estimates")) +
          ylim(1.01 * min_y, 1.01 * max_y) +
          xlim(-1.05, 1.05) +
          labs(title = paste0("Sample ", full_results[[i]]$sample)) +
          theme_bw() +
          theme(legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5)))
  
  dev.off()
}

for (i in 1:length(full_results)){
  
  temp <- full_results[[i]]$B[, 1:2]
  colnames(temp) <- c("Coef.", "Std. Err.")
  rownames(temp) <- c("_cons", "X")
  temp <- temp[c(2, 1), ]
  print(i)
  print(temp)
  
}

# histogram of estimates
data.frame(b2 = B2_store) %>% 
  ggplot() + geom_histogram(aes(x = b2),
                            col = "white",
                            fill = "skyblue",
                            binwidth = 0.1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(y = "", x = expression(b[2]), title = "Histogram of Estimates") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# summary statistics
mean(B2_store)
sd(B2_store)
