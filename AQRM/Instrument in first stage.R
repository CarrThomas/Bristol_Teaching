library(clusterGeneration)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mvtnorm)
library(AER)

N <- 1000
Z <- rbinom(N, 1, 1/2)

rho <- 0.5
sigma <- rbind(c(1, rho), 
               c(rho, 1))
U <- rmvnorm(N, mean = rep(0, 2), sigma = sigma)
alpha <- qnorm(0.55)
q <- Z * alpha + (1 - Z) * (-alpha)
D <- as.numeric(U[, 1] <= q)
Y <- D + U[, 2]

summary(lm(Y ~ D + Z))
lm(Y ~ Z)
mean(D[Z == 0])
mean(D[Z == 1]) - mean(D[Z == 0])
lm(D ~ Z)

Y_D0 <- mean(Y[D == 0])
Y_D1 <- mean(Y[D == 1])
Z_D0 <- mean(Z[D == 0])
Z_D1 <- mean(Z[D == 1])
U_D0 <- mean(U[D == 0, 2])
U_D1 <- mean(U[D == 1, 2])
Y_tilde <- Y - (1 - D) * Y_D0 - D * Y_D1
Z_tilde <- Z - (1 - D) * Z_D0 - D * Z_D1
U_tilde <- U[, 2] - (1 - D) * U_D0 - D * U_D1

lm(Y_tilde ~ Z_tilde - 1)
lm(U_tilde ~ Z_tilde - 1)
 
Y_temp <- Y - D - (1 - D) * u_D0 - D * U_D1

lm(Z ~ D)
mean(Z[D == 0])
mean(Z[D == 1]) - mean(Z[D == 0])

 