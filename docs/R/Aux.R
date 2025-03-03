# -------------------------
# [1.1] Ativação de Pacotes
# -------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# -------------
# [2] Simulação
# -------------
set.seed(123456789) # Semente
n <- 500 # Tamanho amostral

# Geração de dados simulados
dates <- sample(seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day"), n, replace = TRUE)
cidades <- c("Belém", "Ananindeua", "Marituba", "Benevides", "Santa Bárbara")
produtos <- c("Produto A", "Produto B", "Produto C", "Produto D", "Produto E", 
              "Produto F", "Produto G", "Produto H", "Produto I", "Produto J")
categorias <- c("Eletrônicos", "Roupas", "Alimentos", "Livros", "Brinquedos")
vendas <- data.frame(
  DATA_VENDA = dates,
  CIDADE = sample(cidades, n, replace = TRUE),
  PRODUTO = sample(produtos, n, replace = TRUE),
  CATEGORIA = sample(categorias, n, replace = TRUE),
  PRECO = round(runif(n, 10, 200), 2),
  QUANTIDADE = sample(1:50, n, replace = TRUE),
  CANAL_VENDA = sample(c("Online", "Loja Física", "Revendedor"), n, replace = TRUE),
  DESCONTO_APLICADO = round(runif(n, 0, 0.3), 2)
)

# ----------------
# [3] Server Logic
# ----------------
server <- function(input, output, session) {
  # --------------
  # [3.1] Filtro
  # --------------
  DataFiltro <- reactive({
    vendas %>%
      filter(
        DATA_VENDA >= input$date_range[1] & DATA_VENDA <= input$date_range[2],
        (CIDADE == input$municipio | input$municipio == "Todos"),
        (CANAL_VENDA == input$canalvenda | input$canalvenda == "Todos")
      )
  })
  
  # --------------
  # [3.2] Métricas
  # --------------
  output$total_vendas <- renderValueBox({
    total <- sum(DataFiltro()$PRECO * DataFiltro()$QUANTIDADE)
    valueBox(formatC(total, format = "f", big.mark = ","), "Total de Vendas", icon = icon("dollar-sign"))
  })
  
  output$quantidade_vendida <- renderValueBox({
    total_quantidade <- sum(DataFiltro()$QUANTIDADE)
    valueBox(total_quantidade, "Quantidade Vendida", icon = icon("shopping-cart"))
  })
  
  output$desconto_medio <- renderValueBox({
    desconto_medio <- mean(DataFiltro()$DESCONTO_APLICADO)
    valueBox(paste0(round(desconto_medio * 100, 2), "%"), "Desconto Médio", icon = icon("percent"))
  })
  
  # --------------
  # [3.3] Gráficos
  # --------------
  output$vendas_por_cidade <- renderPlotly({
    data <- DataFiltro() %>%
      group_by(CIDADE) %>%
      summarise(Total_Vendas = sum(PRECO * QUANTIDADE))
    
    ggplot(data, aes(x = CIDADE, y = Total_Vendas, fill = CIDADE)) +
      geom_bar(stat = "identity") +
      labs(title = "Vendas por Cidade", x = "Cidade", y = "Total de Vendas") +
      theme_minimal()
  })
  
  output$vendas_por_categoria <- renderPlotly({
    data <- DataFiltro() %>%
      group_by(CATEGORIA) %>%
      summarise(Total_Vendas = sum(PRECO * QUANTIDADE))
    
    ggplot(data, aes(x = CATEGORIA, y = Total_Vendas, fill = CATEGORIA)) +
      geom_bar(stat = "identity") +
      labs(title = "Vendas por Categoria", x = "Categoria", y = "Total de Vendas") +
      theme_minimal()
  })
}
