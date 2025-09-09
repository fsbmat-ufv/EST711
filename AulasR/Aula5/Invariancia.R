# Exemplo: X ~ N(theta, sigma^2) e g(theta) = theta^2 (não injetiva)
set.seed(1)
n     <- 30
sigma <- 1
theta_true <- 0.8
x <- rnorm(n, mean = theta_true, sd = sigma)

# Likelihood (proporcional) em theta
L_theta <- function(theta) {
  # verossimilhança até constante em theta
  mu <- theta
  sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))  # log-likelihood
}

# EMV em theta: média amostral
theta_hat <- mean(x)
eta_hat   <- theta_hat^2

# Grade para plotar
theta_grid <- seq(min(-2, theta_hat)-1, max(2, theta_hat)+1, length.out = 1000)
ll_theta   <- sapply(theta_grid, L_theta)

# Construir L^g(eta) = max{ L(+sqrt(eta)), L(-sqrt(eta)) }
eta_grid <- seq(0, max(theta_grid^2), length.out = 600)
ll_g <- sapply(eta_grid, function(eta) {
  t1 <-  sqrt(eta); t2 <- -sqrt(eta)
  max(L_theta(t1), L_theta(t2))
})

# Normalizar para uma escala comparável (opcional, só para visual)
ll_theta_n <- ll_theta - max(ll_theta)
ll_g_n     <- ll_g     - max(ll_g)

# ---- Plots com ggplot2 ----
library(ggplot2)
library(patchwork)

p1 <- data.frame(theta = theta_grid, ll = ll_theta_n) |>
  ggplot(aes(theta, ll)) +
  geom_line(size = 1) +
  geom_vline(xintercept = theta_hat, linetype = 2) +
  geom_point(aes(x = theta_hat, y = 0), color = "black", size = 2) +
  annotate("text", x = theta_hat, y = 0.1, label = expression(hat(theta)), vjust = -0.5) +
  labs(title = "Verossimilhança em θ",
       y = "log L(θ) (normalizada)", x = expression(theta))

p2 <- data.frame(eta = eta_grid, ll = ll_g_n) |>
  ggplot(aes(eta, ll)) +
  geom_line(size = 1) +
  geom_vline(xintercept = eta_hat, linetype = 2) +
  geom_point(aes(x = eta_hat, y = 0), color = "black", size = 2) +
  annotate("text", x = eta_hat, y = 0.1, label = expression(hat(eta)==g(hat(theta))), vjust = -0.5) +
  labs(title = "Verossimilhança induzida em η = g(θ)",
       y = "log L^g(η) (normalizada)", x = expression(eta))

# Exibir lado a lado
p1 + p2
