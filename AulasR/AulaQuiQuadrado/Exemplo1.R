# Parâmetros do problema
k    <- 6
n    <- 60
p0   <- rep(1/6, k)
alpha<- 0.05
df   <- k - 1
crit <- qchisq(1 - alpha, df = df)

# --- 1) Função de poder (aproximação por qui-quadrado não-central) ---
power_chisq_approx <- function(p, n, p0, alpha = 0.05) {
  stopifnot(length(p) == length(p0))
  stopifnot(abs(sum(p) - 1) < 1e-12, all(p >= 0))
  df   <- length(p) - 1
  crit <- qchisq(1 - alpha, df = df)
  lambda <- n * sum((p - p0)^2 / p0)
  # Poder aproximado:
  1 - pchisq(crit, df = df, ncp = lambda)
}

# --- (opcional) Função de poder por simulação (Monte Carlo) ---
power_chisq_mc <- function(p, n, p0, alpha = 0.05, B = 5e4, seed = 123) {
  set.seed(seed)
  df   <- length(p) - 1
  crit <- qchisq(1 - alpha, df = df)
  X    <- rmultinom(B, size = n, prob = p) # cada coluna é um vetor de contagens
  E0   <- n * p0
  # Estatística de Pearson Q = sum ( (O - E0)^2 / E0 )
  Q    <- colSums((X - E0)^2 / E0)
  mean(Q >= crit)
}

# --- 2) Curvas de poder variando cada p_i ---
# Estratégia: para cada i, mova p_i <- p0[i] + delta e redistribua -delta igualmente nas outras (mantendo soma = 1)
# Limites de delta: para manter probabilidades no [0,1]
build_p <- function(i, delta, p0) {
  k <- length(p0)
  p <- p0
  p[i] <- p0[i] + delta
  p[-i] <- p0[-i] - delta / (k - 1)
  p
}

# grade de deltas (respeitando não-negatividade)
# para p0=1/6, o limite é: delta_min = -p0[i] = -1/6; delta_max = (k-1)*p0[-i] = 5*(1/6) = 5/6,
# MAS a restrição mais apertada vem de p[-i]>=0 -> delta <= (k-1)*min(p0[-i]) = 5*(1/6)=0.833...
# e p[i]<=1 -> delta <= 1 - p0[i] = 5/6. O limitante à esquerda é -1/6.
# Vamos usar uma faixa mais modesta, prática:
deltas <- seq(-0.12, 0.30, by = 0.01)

# Tabela com poder aproximado para cada i e cada delta
library(dplyr)
library(tidyr)
library(purrr)

power_grid <- map_dfr(1:k, function(i) {
  map_dfr(deltas, function(d) {
    p <- build_p(i, d, p0)
    if (any(p < -1e-12) || any(p > 1 + 1e-12)) return(NULL)
    tibble(cat = paste0("p", i),
           delta = d,
           power = power_chisq_approx(p, n, p0))
  })
})

# (Opcional) Checagem por simulação para alguns pontos de cada curva
# isto pode ser pesado; selecione poucos deltas
check_points <- c(-0.10, -0.05, 0.05, 0.10, 0.20, 0.30)
mc_checks <- map_dfr(1:k, function(i) {
  map_dfr(check_points, function(d) {
    p <- build_p(i, d, p0)
    if (any(p < -1e-12) || any(p > 1 + 1e-12)) return(NULL)
    tibble(cat = paste0("p", i),
           delta = d,
           power_mc = power_chisq_mc(p, n, p0, B = 20000, seed = 2025),
           power_ap = power_chisq_approx(p, n, p0))
  })
})

print(mc_checks)  # para comparar aproximação vs MC

# --- 3) Gráfico das curvas de poder ---
library(ggplot2)
ggplot(power_grid, aes(x = delta, y = power, color = cat)) +
  geom_line(size = 1) +
  geom_hline(yintercept = alpha, linetype = 3) +
  labs(x = expression(delta~"(variação em "~p[i]*")"),
       y = "Poder (aprox.)",
       color = "Categoria",
       title = "Curvas de poder do teste qui-quadrado de aderência (k=6, n=60, α=5%)",
       subtitle = "Perturbação: p_i = 1/6 + δ; demais categorias recebem -δ/5 cada") +
  theme_minimal(base_size = 12)
