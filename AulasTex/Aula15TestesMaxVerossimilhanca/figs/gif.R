# Carregando o pacote necessário
library(animation)

# Definindo os parâmetros constantes
theta_0 <- 5  # Valor de 'theta_0', pode ser ajustado
x_bar <- seq(0.1, 10, length.out = 100)  # Valores de x_bar

# Criando o GIF
saveGIF({
  # Loop para gerar gráficos variando o valor de t
  for (t_mult in seq(0.5, 2, by = 0.1)) {
    # Atualizando o valor de t multiplicando x_bar por t_mult
    t <- x_bar / theta_0 * t_mult
    
    # Calculando a expressão para cada valor de t
    n <- 10  # Valor de 'n' constante
    g_t <- exp(n) * t^n * exp(-n * t)
    
    # Plotando o gráfico
    plot(t, g_t, type = "l", col = "red", lwd = 2,
         xlab = "t", ylab = "Valor de g(t)",
         main = bquote(g(t) == e^.(n) * t^.(n) * e^{-n * t} ~ ", " ~ t == .(round(t_mult, 2))))
    grid()
  }
}, movie.name = "variacao_g_t.gif", interval = 0.5, ani.width = 600, ani.height = 400)
