# Carregar pacotes necessários
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Dados simulados
set.seed(123)
num_linhas <- 300
dates <- sample(seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day"), num_linhas, replace = TRUE)
cidade <- sample(c("Belém", "Ananindeua", "Marituba", "Benevides", "Santa Bárbara"), num_linhas, replace = TRUE)
produtos <- c("Produto A", "Produto B", "Produto C", "Produto D", "Produto E", 
              "Produto F", "Produto G", "Produto H", "Produto I", "Produto J")
categorias <- c("Eletrônicos", "Roupas", "Alimentos", "Livros", "Brinquedos")
canal_venda <- sample(c("Online", "Loja Física", "Revendedor"), num_linhas, replace = TRUE)
fpagamento <- sample(c("Cartão de Crédito", "Cartão de Débito", "Pix", "Dinheiro"), num_linhas, replace = TRUE)
precos <- round(runif(num_linhas, 10, 200), 2)
quantidades <- sample(1:50, num_linhas, replace = TRUE)
descontos <- round(runif(num_linhas, 0, 0.3), 2)

vendas <- data.frame(
  data_venda = dates,
  cidade = cidade,
  produto = sample(produtos, num_linhas, replace = TRUE),
  categoria = sample(categorias, num_linhas, replace = TRUE),
  preco = precos,
  quantidade = quantidades,
  canal_venda = canal_venda,
  desconto_aplicado = descontos
)

# UI (Interface do Usuário)
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Vendas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
      menuItem("Detalhes por Categoria", tabName = "category", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Aba Visão Geral
      tabItem(
        tabName = "overview",
        fluidRow(
          box(title = "Filtros", width = 12, collapsible = TRUE,
              dateRangeInput("date_range", "Selecione o Período", 
                             start = min(vendas$data_venda), 
                             end = max(vendas$data_venda)),
              selectInput("cidade_filter", "Cidade", 
                          choices = c("Todas", unique(vendas$cidade)), 
                          selected = "Todas"),
              selectInput("canal_filter", "Canal de Venda", 
                          choices = c("Todos", unique(vendas$canal_venda)), 
                          selected = "Todos")
          )
        ),
        fluidRow(
          valueBoxOutput("total_vendas"),
          valueBoxOutput("quantidade_vendida"),
          valueBoxOutput("desconto_medio")
        ),
        fluidRow(
          box(title = "Vendas por Cidade", width = 6, plotOutput("vendas_por_cidade")),
          box(title = "Vendas por Categoria", width = 6, plotOutput("vendas_por_categoria"))
        )
      ),
      # Aba Detalhes por Categoria
      tabItem(
        tabName = "category",
        fluidRow(
          box(title = "Detalhes por Categoria", width = 12, collapsible = TRUE,
              selectInput("categoria_filter", "Categoria", 
                          choices = c("Todas", unique(vendas$categoria)), 
                          selected = "Todas"),
              plotOutput("categoria_detalhes")
          )
        )
      )
    )
  )
)

# Server (Lógica do Servidor)
server <- function(input, output) {
  # Dados filtrados
  dados_filtrados <- reactive({
    df <- vendas %>%
      filter(
        data_venda >= input$date_range[1] & data_venda <= input$date_range[2],
        (cidade == input$cidade_filter | input$cidade_filter == "Todas"),
        (canal_venda == input$canal_filter | input$canal_filter == "Todos")
      )
    return(df)
  })
  
  # Métricas
  output$total_vendas <- renderValueBox({
    total <- sum(dados_filtrados()$preco * dados_filtrados()$quantidade)
    valueBox(formatC(total, format = "f", big.mark = ","), "Total de Vendas", icon = icon("dollar-sign"))
  })
  
  output$quantidade_vendida <- renderValueBox({
    total_quantidade <- sum(dados_filtrados()$quantidade)
    valueBox(total_quantidade, "Quantidade Vendida", icon = icon("shopping-cart"))
  })
  
  output$desconto_medio <- renderValueBox({
    desconto_medio <- mean(dados_filtrados()$desconto_aplicado)
    valueBox(paste0(round(desconto_medio * 100, 2), "%"), "Desconto Médio", icon = icon("percent"))
  })
  
  # Gráficos
  output$vendas_por_cidade <- renderPlot({
    dados_filtrados() %>%
      group_by(cidade) %>%
      summarise(total_vendas = sum(preco * quantidade)) %>%
      ggplot(aes(x = cidade, y = total_vendas, fill = cidade)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Vendas por Cidade", y = "Total de Vendas", x = "")
  })
  
  output$vendas_por_categoria <- renderPlot({
    dados_filtrados() %>%
      group_by(categoria) %>%
      summarise(total_vendas = sum(preco * quantidade)) %>%
      ggplot(aes(x = categoria, y = total_vendas, fill = categoria)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Vendas por Categoria", y = "Total de Vendas", x = "")
  })
  
  output$categoria_detalhes <- renderPlot({
    dados_filtrados() %>%
      filter(categoria == input$categoria_filter | input$categoria_filter == "Todas") %>%
      group_by(produto) %>%
      summarise(total_vendas = sum(preco * quantidade)) %>%
      ggplot(aes(x = produto, y = total_vendas, fill = produto)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Detalhes por Produto", y = "Total de Vendas", x = "")
  })
}

# Rodar o aplicativo
shinyApp(ui, server)