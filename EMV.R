rm(list = ls())
cat("\014")
##Modelo Bernoulli
n <- 10
k <- 1
p <- 0.9
N <- 1000
res <- numeric(N)
ell <- numeric(N)
for (i in 1:N) {
  x <- rbinom(n,1,p)
fn <- function(theta,y){
  
  n<-length(x)
  y <- sum(x)
  p <- theta[1]
  logl <- y*log(p)+(n-y)*log(1-p)
  return(-logl)
}

res[i] <- optim(0.1,
      fn,
      y=x,
      "BFGS",
      hessian = F, control = list(reltol = 1e-15))$par
ell[i] <- optim(0.1,
                fn,
                y=x,
                "BFGS",
                hessian = F)$value
}
(phat <- mean(res))

fn <- function(p){
  n<-length(x)
  y <- sum(x)
  logl <- y*log(p)+(n-y)*log(1-p)
  return(logl)
}
pteste <- 0.9
x <- rbinom(n,1,pteste)
sum(dbinom(x,1,pteste, log=T))
fn(pteste)
curve(fn,from = 0, to=1, lty="solid", type="l")
p <- seq(0, 1, .01) 
ll <- sapply(p, fn)
plot(ll ~ p) 
p[which(ll == max(ll))] #ponto de maximo
