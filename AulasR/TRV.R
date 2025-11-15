# ===============================================
# Teste da Razão de Verossimilhança (Exemplo Gráfico)
# ===============================================

# Gerar dados simulados de uma Normal com média verdadeira theta = 2
#set.seed(123)
n <- 30
theta_verdadeiro <- 2
x <- rnorm(n, mean = theta_verdadeiro, sd = 1)

# Estimativa de máxima verossimilhança (media amostral)
theta_hat <- mean(x)
theta_hat

# Valores de theta em torno da estimativa
theta_seq <- seq(theta_hat - 2, theta_hat + 2, length.out = 200)

# Função de log-verossimilhança para a Normal(θ, 1)
loglik <- sapply(theta_seq, function(theta) {
  sum(dnorm(x, mean = theta, sd = 1, log = TRUE))
})

# Valor de theta0 (hipótese nula)
theta0 <- 2 # valor verdadeiro

# Calcular razão de verossimilhança
Lambda <- exp(loglik[which.min(abs(theta_seq - theta0))] - max(loglik))
Lambda
LR <- -2 * log(Lambda)

# ---- Gráfico ----
plot(theta_seq, loglik, type = "l", lwd = 2, col = "blue",
     main = "Função de Log-Verossimilhança e Teste da Razão de Verossimilhança",
     xlab = expression(theta), ylab = expression(ell(theta)))

# Marcar pontos importantes
abline(v = theta_hat, col = "red", lwd = 2, lty = 2)
abline(v = theta0, col = "darkgreen", lwd = 2, lty = 2)
points(theta_hat, max(loglik), pch = 19, col = "red")
points(theta0, loglik[which.min(abs(theta_seq - theta0))], pch = 19, col = "darkgreen")

# Linhas auxiliares
segments(theta0, loglik[which.min(abs(theta_seq - theta0))],
         theta_hat, loglik[which.min(abs(theta_seq - theta0))],
         lty = 3)
text(theta_hat, max(loglik), labels = expression(hat(theta)), pos = 3, col = "red")
text(theta0, loglik[which.min(abs(theta_seq - theta0))], labels = expression(theta[0]), pos = 3, col = "darkgreen")

# Adicionar diferença da log-verossimilhança
arrows(theta0, loglik[which.min(abs(theta_seq - theta0))],
       theta0, max(loglik),
       length = 0.1, col = "purple", lwd = 2)
text(theta0 - 0.1, mean(c(loglik[which.min(abs(theta_seq - theta0))], max(loglik))),
     expression(Delta == -2*(ell(theta[0]) - ell(hat(theta)))), col = "purple", pos = 2)

# Mostrar valor de LR
legend("bottomright",
       legend = bquote(Lambda == .(round(Lambda, 4)) ~ ", " ~ -2*log(Lambda) == .(round(LR, 3))),
       bty = "n", cex = 0.9)
