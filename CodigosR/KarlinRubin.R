rm(list = ls())
cat("\014")
alpha <- 0.05
n <- 100
theta0 <- 1
t0 <- qpois(alpha, n*theta0, lower.tail = F)
ppois(t0, theta0*n, lower.tail = F)

rm(list = ls())
cat("\014")
alpha <- 0.05
n <- 5
theta0 <- 3
t0 <- qgamma(alpha, n, scale = theta0)
pgamma(t0, n, scale = theta0)
