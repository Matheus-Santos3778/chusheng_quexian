#Parâmetros do Servidor

source("r/mod_univariate.r")
source("r/mod_bivariate.r")
source("r/mod_temporal.r")

server <- function(input, output, session) {
  
  #Painel tipo de análise
  output$painel_graficos <- renderUI({
    if (input$tipo_analise == "Bivariado") {
      fluidRow(
        plotOutput("grafico_bi", height = "500px")
      )
    } else if (input$tipo_analise == "Univariado") {
      fluidRow(
        plotOutput("grafico_uni", height = "500px"),
        tableOutput("tabela_resumo")
      )
    }
  })
  
  mod_univariate_server(input, output, session)
  mod_bivariate_server(input, output, session)
  mod_temporal_server(input, output, session)
  
}