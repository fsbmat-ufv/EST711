rm(list = ls())
cat("\014")
n <- 100
p <- 0.6
x <- rbinom(n,1,p)
y <- sum(x)
loglik <- function(p){
  ell <- y*log(p)+(n-y)*log(1-p)
  return(ell)
}
curve(loglik,from = 0,to = 1)
abline(v=(y/n))