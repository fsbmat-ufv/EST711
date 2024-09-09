rm(list=ls())
cat("\014")
# Carregar pacotes necessários
library(tidyverse)

# Definir funções para gerar gráficos e estimadores
plot_histogram <- function(data, true_value, estimator_name) {
  ggplot(data, aes(x = value)) +
    geom_histogram(binwidth = 0.5, fill = 'darkgray', alpha = 0.7) +
    geom_vline(aes(xintercept = true_value), color = 'red', linetype = 'dashed') +
    geom_vline(aes(xintercept = mean(value)), color = 'black', linetype = 'dotted') +
    labs(title = paste("Distribuição dos Estimadores -", estimator_name),
         x = "Valor", y = "Frequência") +
    theme_minimal()
}

# Função para gerar amostras e calcular estimadores
generate_estimators <- function(n, dist_name, params, estimator_func, true_value) {
  estimators <- replicate(1000, {
    sample_data <- do.call(dist_name, params)
    estimator_func(sample_data)
  })
  
  data.frame(value = estimators)
}

# Exemplos de estimadores não viesados e viesados

# 1. Média de uma amostra normal
true_mean <- 10
params_normal <- list(n = 30, mean = true_mean, sd = 5)
estimators_mean <- generate_estimators(30, "rnorm", params_normal, mean, true_mean)
plot_histogram(estimators_mean, true_mean, "Média Normal")

# 2. Variância de uma amostra normal (estimador viesado e não viesado)

# Estimador viesado da variância (usando n no denominador)
true_variance <- 25
params_normal_var <- list(n = 30, mean = true_mean, sd = sqrt(true_variance))
estimators_var_viesado <- generate_estimators(30, "rnorm", params_normal_var, function(x) var(x) * (length(x) - 1) / length(x), true_variance)
plot_histogram(estimators_var_viesado, true_variance, "Variância Normal (Estimador Viesado)")

# Estimador não viesado da variância (usando n-1 no denominador) - var() em R já faz isso automaticamente
estimators_var_nao_viesado <- generate_estimators(30, "rnorm", params_normal_var, var, true_variance)
plot_histogram(estimators_var_nao_viesado, true_variance, "Variância Normal (Estimador Não Viesado)")

# 3. Proporção de sucessos em uma amostra binomial
true_proportion <- 0.7
params_binom <- list(n = 30, size = 1, prob = true_proportion)
estimators_proportion <- generate_estimators(30, "rbinom", params_binom, mean, true_proportion)
plot_histogram(estimators_proportion, true_proportion, "Proporção Binomial")

# 4. Média de uma amostra exponencial
true_lambda <- 1
params_exp <- list(n = 30, rate = true_lambda)
estimators_exp_mean <- generate_estimators(30, "rexp", params_exp, mean, 1 / true_lambda)
plot_histogram(estimators_exp_mean, 1 / true_lambda, "Média Exponencial")

# 5. Variância de uma amostra exponencial (estimador viesado e não viesado)

# Variância verdadeira da distribuição exponencial
true_exp_variance <- 1 / true_lambda^2
params_exp_var <- list(n = 30, rate = true_lambda)

# Estimador viesado da variância (usando n no denominador)
estimators_exp_var_viesado <- generate_estimators(30, "rexp", params_exp_var, function(x) var(x) * (length(x) - 1) / length(x), true_exp_variance)
plot_histogram(estimators_exp_var_viesado, true_exp_variance, "Variância Exponencial (Estimador Viesado)")

# Estimador não viesado da variância (usando n-1 no denominador) - var() em R já faz isso automaticamente
estimators_exp_var_nao_viesado <- generate_estimators(30, "rexp", params_exp_var, var, true_exp_variance)
plot_histogram(estimators_exp_var_nao_viesado, true_exp_variance, "Variância Exponencial (Estimador Não Viesado)")

# 6. Média de uma amostra uniforme
true_uniform_mean <- 5
true_uniform_sd <- sqrt(1 / 12)
params_unif <- list(n = 30, min = 0, max = 10)
estimators_unif_mean <- generate_estimators(30, "runif", params_unif, mean, true_uniform_mean)
plot_histogram(estimators_unif_mean, true_uniform_mean, "Média Uniforme")

# 7. Variância de uma amostra uniforme (estimador viesado e não viesado)

# Variância verdadeira da distribuição uniforme
true_uniform_variance <- (10 - 0)^2 / 12
params_unif_var <- list(n = 30, min = 0, max = 10)

# Estimador viesado da variância (usando n no denominador)
estimators_unif_var_viesado <- generate_estimators(30, "runif", params_unif_var, function(x) var(x) * (length(x) - 1) / length(x), true_uniform_variance)
plot_histogram(estimators_unif_var_viesado, true_uniform_variance, "Variância Uniforme (Estimador Viesado)")

# Estimador não viesado da variância (usando n-1 no denominador) - var() em R já faz isso automaticamente
estimators_unif_var_nao_viesado <- generate_estimators(30, "runif", params_unif_var, var, true_uniform_variance)
plot_histogram(estimators_unif_var_nao_viesado, true_uniform_variance, "Variância Uniforme (Estimador Não Viesado)")

# 8. Média de uma amostra Poisson
true_lambda_poisson <- 5
params_pois <- list(n = 30, lambda = true_lambda_poisson)
estimators_pois_mean <- generate_estimators(30, "rpois", params_pois, mean, true_lambda_poisson)
plot_histogram(estimators_pois_mean, true_lambda_poisson, "Média Poisson")

# 9. Variância de uma amostra Poisson (estimador viesado e não viesado)

# Variância verdadeira da distribuição Poisson
true_pois_variance <- true_lambda_poisson
params_pois_var <- list(n = 30, lambda = true_lambda_poisson)

# Estimador viesado da variância (usando n no denominador)
estimators_pois_var_viesado <- generate_estimators(30, "rpois", params_pois_var, function(x) var(x) * (length(x) - 1) / length(x), true_pois_variance)
plot_histogram(estimators_pois_var_viesado, true_pois_variance, "Variância Poisson (Estimador Viesado)")

# Estimador não viesado da variância (usando n-1 no denominador) - var() em R já faz isso automaticamente
estimators_pois_var_nao_viesado <- generate_estimators(30, "rpois", params_pois_var, var, true_pois_variance)
plot_histogram(estimators_pois_var_nao_viesado, true_pois_variance, "Variância Poisson (Estimador Não Viesado)")

# 10. Média de uma amostra binomial
true_p <- 0.5
true_size <- 10
params_binom_mean <- list(n = 30, size = true_size, prob = true_p)
estimators_binom_mean <- generate_estimators(30, "rbinom", params_binom_mean, mean, true_size * true_p)
plot_histogram(estimators_binom_mean, true_size * true_p, "Média Binomial")

# # Salvar gráficos em arquivos PNG
# ggsave("histograma_media_normal.png", plot_histogram(estimators_mean, true_mean, "Média Normal"))
# ggsave("histograma_variancia_normal_viesado.png", plot_histogram(estimators_var, true_variance, "Variância Normal (Estimador Viesado)"))
# ggsave("histograma_variancia_normal_naoviesado.png", plot_histogram(estimators_var_unbiased, true_variance, "Variância Normal (Estimador Não Viesado)"))
# ggsave("histograma_proporcao_binomial.png", plot_histogram(estimators_proportion, true_proportion, "Proporção Binomial"))
# ggsave("histograma_media_exponencial.png", plot_histogram(estimators_exp_mean, 1 / true_lambda, "Média Exponencial"))
# ggsave("histograma_variancia_exponencial_viesado.png", plot_histogram(estimators_exp_var, true_exp_variance, "Variância Exponencial (Estimador Viesado)"))
# ggsave("histograma_variancia_exponencial_naoviesado.png", plot_histogram(estimators_exp_var_unbiased, true_exp_variance, "Variância Exponencial (Estimador Não Viesado)"))
# ggsave("histograma_media_uniforme.png", plot_histogram(estimators_unif_mean, true_uniform_mean, "Média Uniforme"))
# ggsave("histograma_variancia_uniforme_viesado.png", plot_histogram(estimators_unif_var, true_uniform_variance, "Variância Uniforme (Estimador Viesado)"))
# ggsave("histograma_variancia_uniforme_naoviesado.png", plot_histogram(estimators_unif_var_unbiased, true_uniform_variance, "Variância Uniforme (Estimador Não Viesado)"))
# ggsave("histograma_media_poisson.png", plot_histogram(estimators_pois_mean, true_lambda_poisson, "Média Poisson"))
# ggsave("histograma_variancia_poisson_viesado.png", plot_histogram(estimators_pois_var, true_pois_variance, "Variância Poisson (Estimador Viesado)"))
# ggsave("histograma_variancia_poisson_naoviesado.png", plot_histogram(estimators_pois_var_unbiased, true_pois_variance, "Variância Poisson (Estimador Não Viesado)"))
# ggsave("histograma_media_binomial.png", plot_histogram(estimators_binom_mean, true_size * true_p, "Média Binomial"))
