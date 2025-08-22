set.seed(123)

m <- 1000    # número de amostras binomiais (tamanho da amostra)
n <- 100     # parâmetro da Binomial
p <- 0.7

# Gerar m observações iid de Binomial(n, p)
x <- rbinom(m, size = n, prob = p)

# Média amostral das Binomiais
xBarra <- mean(x)

# Média e desvio-padrão da Binomial(n, p)
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

# Estatística do TLC (média de Binomiais)
z <- sqrt(m) * (xBarra - mu) / sigma
z

set.seed(123)

## Parâmetros
n <- 100      # parâmetro da Binomial (cada observação é Bin(n, p))
p <- 0.7
m <- 1000     # tamanho da amostra de Binomiais (qtd de Binomiais iid por experimento)
B <- 10000    # número de experimentos Monte Carlo

mu <- n * p
sigma <- sqrt(n * p * (1 - p))

## Monte Carlo: em cada experimento,
## 1) geramos m observações Bin(n,p)
## 2) calculamos a média amostral
## 3) computamos Z = sqrt(m)*(xBarra - np)/sqrt(np(1-p))
Z <- replicate(B, {
  x <- rbinom(m, size = n, prob = p)
  xBarra <- mean(x)
  sqrt(m) * (xBarra - mu) / sigma
})

## Checagens rápidas
mean(Z)       # ~ 0
var(Z)        # ~ 1

## Histograma + curva Normal(0,1)
hist(Z, breaks = 60, freq = FALSE,
     main = sprintf("CLT: média de Bin(n=%d, p=%.2f) com m=%d; B=%d", n, p, m, B),
     xlab = "Z = sqrt(m)*(x̄ - np)/sqrt(np(1-p))")
curve(dnorm(x), add = TRUE, lwd = 2)

## QQ-plot para ver aderência à Normal(0,1)
qqnorm(Z, main = "QQ-plot de Z"); qqline(Z, lwd = 2)


########################

set.seed(123)
n <- 100; p <- 0.7; mu <- n*p; sigma <- sqrt(n*p*(1-p))
B <- 4000
m_vals <- c(20, 50, 100, 300, 1000)

par(mfrow = c(2, 3))
for (m in m_vals) {
  Zm <- replicate(B, {
    x <- rbinom(m, n, p)
    xBarra <- mean(x)
    sqrt(m) * (xBarra - mu) / sigma
  })
  hist(Zm, breaks = 50, freq = FALSE,
       main = paste("m =", m), xlab = "Z")
  curve(dnorm(x), add = TRUE, lwd = 2)
}
par(mfrow = c(1, 1))
