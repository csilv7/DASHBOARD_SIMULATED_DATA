# -----------------
# [1] Parte Inicial
# -----------------

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

# -----------------
# [1.2] Script Base
# -----------------
rodapeL <- "Dashboard criado via Pacote Shiny em R"
rodapeR <- "Elaborado por Breno C R Silva"

# -----------------
# [1.2.1] Simulação
# -----------------
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

# -----------------------------------
# [2] FrontEnd - Interface do Usuário
# -----------------------------------
ui <- dashboardPage(
  title = "Dashboard de Vendas",
  skin = "blue",
  
  # ---------------
  # [2.1] CABEÇALHO
  # ---------------
  header = dashboardHeader(
    title = "Painel de Vendas - 2024",
    titleWidth = 220
  ),
  
  # -------------------
  # [2.2] BARRA LATERAL
  # -------------------
  sidebar = dashboardSidebar(
    width = 220,
    tags$img(src = "LOGOSHINY.png", width = "100%", height = "auto"),
    
    sidebarMenu(
      menuItem("PROJETO", tabName = "project", icon = icon("book")),
      
      menuItem("EXPLORATÓRIA", tabName = "aed", icon = icon("chart-bar"),
               
               menuSubItem("VISÃO GERAL", tabName = "VisionGeral", icon = icon("dashboard")),
               menuSubItem("POR CATEGORIA", tabName = "DetalCat", icon = icon("chart-bar"))),
      
      menuItem("CONCLUSÕES", tabName = "conclu", icon = icon("book"))
    ),
    
    selectInput("municipio", "MUNICÍPIO",
                choices = c("Todos", unique(vendas$CIDADE)), 
                selected = "Todos"),
    
    selectInput("canalvenda", "CANAL DE VENDA",
                choices = c("Todos", unique(vendas$CANAL_VENDA)),
                selected = "Todos")
    
  ),
  
  # -----------
  # [2.3] CORPO
  # -----------
  body = dashboardBody(
    tabItems(
      # ---------------------
      # [2.3.1] ABA - PROJETO
      # ---------------------
      tabItem(tabName = "project",
              tabBox(
                id = "tabs", width = 12, height = "600px",
                tabPanel("Referencial", icon = icon("book"), 
                         h3("Conteúdo do Referencial"),
                         p("O referencial teórico aborda as bases que fundamentam o desenvolvimento do projeto."),
                         tags$ul(
                           tags$li("Ponto 1: Descrição inicial do projeto."),
                           tags$li("Ponto 2: Contexto teórico e objetivo principal."),
                           tags$li("Ponto 3: Revisão de literatura relevante.")
                         )),
                tabPanel("Material e Métodos", icon = icon("chart-bar"), 
                         h3("Material e Métodos"),
                         p("Nesta seção, são apresentados os dados simulados e a metodologia aplicada.")),
                tabPanel("Recurso Computacional", icon = icon("desktop"),
                         fluidRow(
                           column(6, h3("Software"),
                                  p("Painel criado com R-Project (Versão 4.4.1)."),
                                  tags$img(src = "https://www.r-project.org/Rlogo.png", 
                                           alt = "Software R", height = "100px")),
                           column(6, h3("IDE"),
                                  p("Ambiente utilizado: RStudio (Versão 1.4.1.7)."),
                                  tags$img(src = "RStudioLogo.png", 
                                           alt = "RStudio", height = "100px"))
                         )),
                tabPanel("Créditos", icon = icon("phone"), 
                         h3("Créditos"),
                         tags$ul(
                           tags$li("Desenvolvedor: Breno C R Silva"),
                           tags$li("Contato: breno@example.com")
                         )),
                tabPanel("Responsável Técnico", icon = icon("user"), 
                         h3("Responsável Técnico"),
                         tags$ul(
                           tags$li("Nome: Breno C R Silva"),
                           tags$li("Formação: Estatística"),
                           tags$li("Área: Análise de Dados e Desenvolvimento em Shiny")
                         ))
              )),
      # ----------------------------------
      # [2.3.2] ABA - ANÁLISE EXPLORATÓRIA
      # ----------------------------------
      tabItem(
        tabName = "VisionGeral",
        fluidRow(
          box(title = "Filtros", width = 12, collapsible = TRUE,
              dateRangeInput("date_range", "Selecione o Período", 
                             start = min(vendas$DATA_VENDA), 
                             end = max(vendas$DATA_VENDA))
          ),
          
          fluidRow(
            valueBoxOutput("total_vendas"),
            valueBoxOutput("quantidade_vendida"),
            valueBoxOutput("desconto_medio")
          )
        )
      ),
      
      
      # ------------------------
      # [2.3.3] ABA - CONCLUSÕES
      # ------------------------
      tabItem(tabName = "conclu", h3("Conclusões"))
    ),
    
  # Rodapé
  footer = dashboardFooter(left = rodapeL, right = rodapeR)
  )
)

# -----------------------------
# [3] BackEnd - Camada Servidor
# -----------------------------

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
  
  
}

# ----------------------------
# [4] Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)
