library(shiny)
library(ggplot2)

# Função que calcula o poder da hipótese
poder <- function(mu, mu0, sigma, n, alpha) {
  z_alpha <- qnorm(1 - alpha / 2)
  term1 <- pnorm((sqrt(n) * (mu0 - mu) / sigma) - z_alpha)
  term2 <- 1 - pnorm((sqrt(n) * (mu0 - mu) / sigma) + z_alpha)
  return(term1 + term2)
}

# Define UI for application
ui <- fluidPage(
  
  # Título da aplicação
  titlePanel("Gráfico da Função de Poder"),
  
  # Sidebar layout com um slider para alterar o valor de mu
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Valor de μ:", min = 25000, max = 35000, value = 30000, step = 100)
    ),
    
    # Mostra o gráfico na mainPanel
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define o servidor
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # Gera os valores de mu para o gráfico
    mu_values <- seq(25000, 35000, by = 10)
    poder_values <- sapply(mu_values, poder, mu0 = 30000, sigma = 5000, n = 30, alpha = 0.01)
    
    # Cria o dataframe para o gráfico
    df <- data.frame(mu = mu_values, poder = poder_values)
    
    # Gera o gráfico com ggplot2
    ggplot(df, aes(x = mu, y = poder)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Função de Poder", x = expression(mu), y = expression(gamma(mu))) +
      theme_minimal() +
      geom_vline(xintercept = input$mu, linetype = "dashed", color = "red") +
      annotate("text", x = input$mu, y = 0.1, label = paste("μ =", input$mu), color = "red") + 
      annotate("text", x = input$mu, y = 0.3, label = paste("Poder =", round(sapply(input$mu, poder, mu0 = 30000, sigma = 5000, n = 30, alpha = 0.01),digits = 3)), color = "blue")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
