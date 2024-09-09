rm(list = ls())
cat("\014")
# Carregar pacotes necessários
library(dplyr)

# Parâmetros da distribuição normal
mu <- 10      # Média da distribuição normal
sigma <- 2    # Desvio padrão da distribuição normal

# Gerar uma amostra de 10000 dados com distribuição normal
set.seed(123)  # Definimos a semente para garantir a reprodutibilidade dos resultados
data <- rnorm(10000, mean = mu, sd = sigma)  # Geramos uma amostra de 10.000 dados com distribuição normal

# Valores de 'a' variando de 1 até 100
a_values <- 1:15

# Função para calcular a probabilidade máxima usando desigualdade de Markov
markov_prob <- function(a) {
  min(mu / a, 1)  # Garantir que a probabilidade não seja maior que 1
}

# Função para calcular a probabilidade máxima usando desigualdade de Tchebychev
chebyshev_prob <- function(a) {
  k <- a / sigma  # Calcula o valor de k, que é o número de desvios padrão
  min(1 / k^2, 1)  # Garantir que a probabilidade não seja maior que 1
}

# Função para calcular a probabilidade real de X ser maior que a
real_prob_greater_than_a <- function(a) {
  mean(data > a)  # Calcula a probabilidade real de X ser maior que 'a'
}

# Criar a tabela de resultados
results <- data.frame(
  a = a_values,  # Coluna com os valores de 'a'
  Real_Prob = sapply(a_values, real_prob_greater_than_a),  # Probabilidade real para cada valor de 'a'
  Markov_Prob = sapply(a_values, markov_prob),  # Probabilidade máxima para a desigualdade de Markov
  Chebyshev_Prob = sapply(a_values, chebyshev_prob)  # Probabilidade máxima para a desigualdade de Tchebychev
)

# Exibir a tabela
print(results)


#Com apenas um valor de a


# Parâmetros da distribuição normal
mu <- 10
sigma <- 2

# Gerar uma amostra de 1000 dados com distribuição normal
set.seed(123)
data <- rnorm(1000, mean = mu, sd = sigma)

# Calcular a média e o desvio padrão da amostra
mean_data <- mean(data)
sd_data <- sd(data)

# Parâmetro a para a desigualdade de Markov
a <- 15

# Aplicar a desigualdade de Markov
markov_bound <- mean_data / a

# Parâmetro k para a desigualdade de Tchebychev
k <- 2

# Aplicar a desigualdade de Tchebychev
chebyshev_bound <- 1 / k^2

# Calcular a probabilidade real de X ser maior que 'a'
prob_greater_than_a <- mean(data > a)

# Calcular a probabilidade real de X estar a mais de k desvios padrão da média
prob_outside_k_sd <- mean(abs(data - mean_data) >= k * sd_data)

# Mostrar resultados
cat("Desigualdade de Markov: P(X >= ", a, ") <= ", markov_bound, "\n")
cat("Probabilidade real de X ser maior que ", a, ": ", prob_greater_than_a, "\n")
cat("Desigualdade de Tchebychev: P(|X - mu| >= ", k, "* sigma) <= ", chebyshev_bound, "\n")
cat("Probabilidade real de X estar a mais de ", k, " desvios padrão da média: ", prob_outside_k_sd, "\n")

# Visualizar a distribuição dos dados
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_data + k * sd_data), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean_data - k * sd_data), color = "red", linetype = "dashed") +
  labs(title = "Distribuição Normal com Linhas de Desvio Padrão",
       x = "Valor", y = "Frequência") +
  theme_minimal()
