rm(list=ls())
cat("\014")
VarX <- 30
VarY <- 40
nX <- 6
dfX <- nX-1
nY <- 21
dfY <- nY-1
Fcal <- (VarX/dfX)/(VarY/dfY)
Pvalor <- pf(Fcal, dfX,dfY, lower.tail = FALSE)
Ftab <- qf(0.05, dfX, dfY, lower.tail = F)
Poder <- pf(Ftab*(1/3), dfX, dfY)

library(ggplot2)
df1 <- 5
df2 <- 20
Alpha <- 0.05

df <- data.frame(X = seq(from = 0, to = 10, length = 500))

p <- ggplot(data=df, mapping=aes(x = X, y = df(x = X, df1 = df1, df2 = df2, ncp=0)))+
  geom_area(color="black", fill="white")
p <- p + geom_area(aes(x=X, y=df(x = X, df1 = df1, df2 = df2, ncp=2)), color="blue", fill="blue", alpha = 1/6) ## this is the only line I've changed.
p <- p + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(breaks=seq(0, 10, 1))
p <- p + geom_area(data = subset(df, X > qf(p = 1-Alpha, df1 = df1, df2 = df2, ncp=0)), fill = "red")
p <- p + geom_area(data = subset(df, X > qf(p = 1-Alpha, df1 = df1, df2 = df2, ncp=0)),
                   aes(x=X, y=df(x = X, df1 = df1, df2 = df2, ncp=2)), fill = "green", alpha=1/3)
p