# Definindo os parâmetros
n <- 10   # Valor de 'n', pode ser ajustado
theta_0 <- 5  # Valor de 'theta_0', pode ser ajustado
x_bar <- seq(0.1, 10, length.out = 1000)  # Valores de x_bar (mais pontos para melhor precisão)
t <- x_bar / theta_0  # Valores de t

# Calculando a expressão para cada valor de t
g_t <- exp(n) * t^n * exp(-n * t)

# Encontrando os valores de t tal que g(t) = 0.25
t_values <- t[abs(g_t - 0.25) < 0.001]  # Encontrando todos os valores de t para os quais g(t) está próximo de 0.25

# Plotando o gráfico
plot(t, g_t, type = "l", col = "red", lwd = 2,
     xlab = "t", ylab = "Valor de g(t)",
     main = expression(g(t) == e^n * t^n * e^{-n * t}),
     xlim = c(-0.1, max(t)), ylim = c(-0.1, max(g_t)),
     axes = FALSE)  # 'axes = FALSE' remove os eixos
grid()


# Acrescentando as linhas horizontais de t = 0 até t = t_values onde g(t) = 0.25
for (t_target in t_values) {
  lines(c(0, t_target), c(0.25, 0.25), col = "blue", lwd = 2, lty = 2)  # Linhas azuis tracejadas
  points(t_target, 0.25, pch = 19, col = "blue")  # Pontos indicando os valores de t_target
}
# Adicionando as linhas verticais tracejadas ligando (0.56, 0) até (0.56, 0.25) e (1.62, 0) até (1.62, 0.25)
lines(c(0.56, 0.56), c(0, 0.25), col = "blue", lty = 2, lwd = 2)  # Linha vertical tracejada em t = 0.56 até g(t) = 0.25
lines(c(1.62, 1.62), c(0, 0.25), col = "blue", lty = 2, lwd = 2)  # Linha vertical tracejada em t = 1.62 até g(t) = 0.25

# Adicionando a letra "C" ao lado do valor y = 0.25
text(x = -0.15, y = 0.25, labels = "C", pos = 4, col = "blue", cex = 1.2)  # 'pos = 4' coloca à direita do ponto

# Adicionando os rótulos "C1" e "C2" acima dos pontos específicos
text(x = 0.54, y = -0.15, labels = "C1", pos = 3, col = "blue", cex = 1.2)  # 'pos = 3' coloca acima do ponto
text(x = 1.64, y = -0.15, labels = "C2", pos = 3, col = "blue", cex = 1.2)  # 'pos = 3' coloca acima do ponto
abline(h=0, v=0)