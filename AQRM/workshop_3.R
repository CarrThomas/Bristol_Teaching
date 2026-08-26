# Examples and figures for workshop 3

rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

# Plot of the t-distribution
B4 <- seq(-3, 3, length.out = 101)
dens <- dt(B4, 205, 0)

data <- data.frame(B4 = B4, dens = dens)
shade_1 <- data[1:15, ]
shade_2 <- data[86:101, ]


data %>% ggplot() +
  geom_line(aes(x = B4, y = dens, col = "dens")) +
  geom_area(data = shade_1, aes(x = B4, y = dens), 
            fill = "red", 
            alpha = 0.5) +
  geom_area(data = shade_2, aes(x = B4, y = dens), 
            fill = "red", 
            alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "null") + 
  labs(y = "", x = "") + 
  scale_x_continuous(breaks = 0)


###################### Example of Mutlicollinearity ######################

rm(list = ls())
gc()

runs <- 3
N <- 100
X <- runif(N, 0 , 2)
X_2 <- X ^ 2
X_3 <- X ^ 3

results <- vector(mode = "list", length = runs)
for (i in 1:runs){
  
  Y <- X + X_2 + X_3 + rnorm(N)
  results[[i]] <- lm(Y ~ X + X_2 + X_3)$coefficients
  
}

# estimates functions
X <- seq(0, 2, length.out = 101)
Y <- matrix(0, length(X), runs)

for (i in 1:runs){
  
  B <- results[[i]]
  Y[, i] <- B[1] + B[2] * X + B[3] * X ^ 2 + B[4] * X ^ 3
  
}

true <- X + X ^ 2 + X ^ 3

data <- data.frame("X" = rep(X, runs + 1), 
                   "Y" = c(Y, true), 
                   "run" = c(rep(paste0("Sample", 1:runs), 
                               each = length(X)),
                             rep("true function", length(X))))

data %>% ggplot(aes(x = X, y = Y, col = run)) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank())
  

results_tab <- matrix(0, 4, runs)
for (i in 1:runs){
  results_tab[, i] <- results[[i]]
}
colnames(results_tab) <- paste0("sample ", 1:runs)
print(results_tab)
rownames(results_tab) <- c(expression(b[1]),
                           expression(b[2]),
                           expression(b[3]),
                           expression(b[4]))
