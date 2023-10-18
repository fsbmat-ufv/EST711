rm(list = ls())
cat("\014")
library("pwr")
#Cria a curva da densidade de chi quadrado
dados <- c(13,19,11,8,5,4)
n <- length(dados)
df <- n-1
init <- 0
end <- 25
alpha <- 0.05
ChisqCal <- chisq.test(dados,p=rep(1/6,6))$statistic
ChisqTab <- qchisq(alpha,df,lower.tail = FALSE)
pvalor <- pchisq(ChisqCal, df = df, lower.tail = FALSE)
curve(dchisq(x, df = 5), from = init, to = end,
      main = bquote("Distribuição Chi-Quadrado  (df" == .(df)*")"),
      ylab = 'Densidade',
      xlab = "x",
      lwd = 2,
      col = "black")


#cria um vetor de valor a partir de ChisqTab
x_ChisqTab <- seq(ChisqTab, end)
#Cria um vetor de densidades a partir de x_ChisqTab
p1 <- dchisq(x_ChisqTab, df = df)
#cria um vetor de valor a partir de ChisqCal
x_ChisqCal <- seq(ChisqCal, end)
#Cria um vetor de densidades a partir de x_ChisqCal
p2 <- dchisq(x_ChisqCal, df = df)

#fill in portion of the density plot from 0 to 40
#Desenha a porcao do grafico referente ao valor tabela
polygon(c(x_ChisqTab, rev(x_ChisqTab)), c(p1, rep(0, length(p1))),
        col = adjustcolor('blue', alpha=0.6), border = NA)
#Desenha a porcao do grafico referente ao p-valor
polygon(c(x_ChisqCal, rev(x_ChisqCal)), c(p2, rep(0, length(p2))),
        col = adjustcolor('red', alpha=0.9), border = NA)
text(x=ChisqCal, y=dchisq(ChisqCal, df = df)+0.01, "ChisqCal")
text(x=ChisqTab-3, y=dchisq(ChisqTab, df = df), "ChisqTab")
Resultado <- ifelse(ChisqCal>ChisqTab, 
                    "Rejeitamos H0, pois ChisqCal>ChisqTab", 
                    "Não Rejeitamos H0, pois ChisqCal<ChisqTab")
text(x=end-9, y=0.1, Resultado)
pvalue <- round(pvalor, digits = 3) 
text(x=end-9, y=0.05, bquote("P-valor"==.(pvalue)))

#############
#Poder do Teste

#chi squared test
h <- function(mu, alpha, no, degree) {#calculate the power of a particular value for the chi squared test
  p01 <- 1/6 # these constructs the effect size (which is a bit different for the chi squared)
  p02 <- 5/6
  
  p11 <- mu
  p12 <- 1-p11
  
  effect.size <- sqrt(((p01-p11)^2/p01)+((p02-p12)^2/p02)) # effect size
  
  pwr.chisq.test(N=no, df=degree, sig.level = alpha, w=effect.size)$power
}
curve(h(x, alpha = 0.05, no = 100, degree=1), 
      from = .00, 
      to = .75, 
      col="red",
      lwd=c(2.5,2.5),
      main=expression("Função Poder para cada p"[i]))

