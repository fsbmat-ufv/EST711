rm(list = ls())
cat("\014")
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
