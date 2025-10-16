#Pacotes
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(bslib)

data <- read_csv("data/dados_finais.csv")

#Ajustando fatores Escolaridade, local de nascimento, estado civil, raça/cor, e identificação de AC
data$ESCMAE <-factor(x=data$ESCMAE,
                              levels = c("1", "2", "3", "4", "5", "9"),
                              labels = c("Nenhuma", "1a3anos", "4a7anos", "8a11anos", "12anosemais", "ignorado"))

data$LOCNASC <-factor(x=data$LOCNASC,
                               levels = c("1", "2", "3", "4"),
                               labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))

data$ESTCIVMAE <-factor(x=data$ESTCIVMAE,
                                 levels = c("1", "2", "3", "4", "5", "9"),
                                 labels = c("solteira", "Casada", "Viúva", "Separada jud.", "União estável", "Ignorada"))

data$RACACORMAE <-factor(x=data$RACACORMAE,
                                  levels = c("1", "2", "3", "4", "5"),
                                  labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

data$IDANOMAL <-factor(x=data$IDANOMAL,
                                levels = c("1", "2", "9"),
                                labels = c("Sim", "Não", "Ignorada"))

#Transformando colunas para numéricas
cols_num <- c("IDADEMAE", "QTDFILVIVO", "QTDFILMORT", "QTDGESTANT", "QTDPARTCES", "QTDPARTNOR", "GRAVIDEZ", "PARTO", "CONSULTAS", "APGAR1", "APGAR5", "PESO", "SEMAGESTAC", "GESTACAO")
data[cols_num] <- lapply(data[cols_num], as.numeric)

#Transformando colunas para datas
cols_data <- c('DTNASC', 'DTCADASTRO')
data[cols_data] <- lapply(data[cols_data], as.Date, format= "%d%m%Y")

ac_agrupadas <- c("Defeito do tubo Neural", "Microcefalia", "Cardiopatias congenitas", "Fendas Orais", "Órgãos genitais", "Defeitos de membros", "Defeitos de parede abdominal", "Sindrome de Dow")

#Variáveis Contínuas
var_num <- c('IDADEMAE', 'PESO')

#Variáveis Categóricas ou Discretas com poucos valores
var_cat <- c('ESCMAE', 'SEXO', 'RACACORMAE', 'ESTCIVMAE', 'QTDFILVIVO', 'CONSULTAS', 'APGAR1', 'APGAR5')

#Tema ggplot
ggplot2::theme_set(ggplot2::theme_minimal())

#Aplica o CSS do Shiny app nos plots do ggplot
thematic_shiny()

tam_fonte_eixos <- 10

#Parâmetros da UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel(h2("Análise de Anomalias Congênitas", align="center")),
  
  fluidRow(
    column(4,
           selectInput("tipo_analise", "Tipo de análise:",
                       choices = c("Bivariado", "Univariado"))
    ),
    column(4,
           selectInput("anomalia", "Selecione a AC:",
                       choices = ac_agrupadas,
                       selected = ac_agrupadas[1])
    ),
    column(4,
           selectInput("variavel", "Selecione variável explicativa:",
                       choices = c(var_num, var_cat),
                       selected = var_num[1])
    )
  ),
  
  hr(),
  
  uiOutput("painel_graficos")
)

#Parâmetros do Servidor
server <- function(input, output, session) {
  
  #Painel tipo de análise
  output$painel_graficos <- renderUI({
    if (input$tipo_analise == "Bivariado") {
      fluidRow(
        column(8, offset = 2,
               plotOutput("grafico_bi", height = "500px"))
      )
    } else {
      fluidRow(
        column(8, offset = 2,
               plotOutput("grafico_uni_var", height = "500px"),
               br(),
               h4(textOutput("titulo_tabela"), align = "center"),
               div(
                 style = "display: flex; justify-content: center;",
                 tableOutput("tabela_anomalia")
               )
        )
      )
    }
  })
  
  #Bivariado
  output$grafico_bi <- renderPlot({
    var <- input$variavel
    anom <- input$anomalia
    
    if (var %in% var_num) {
      ggplot(data, aes(x = factor(.data[[anom]]), y = .data[[var]])) +
        geom_boxplot(fill = "#5bc0de", alpha = 0.7) +
        labs(x = anom, y = var, title = paste(var, "x", anom)) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold"),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold")
        )
      
    } else if (var %in% var_cat) {
      data %>%
        group_by(.data[[var]]) %>%
        summarise(prop_sim = mean(as.numeric(.data[[anom]]) == 1, na.rm = TRUE)) %>%
        ggplot(aes(x = as.factor(.data[[var]]), y = prop_sim)) +
        geom_col(fill = "#5bc0de") +
        geom_text(aes(label = scales::percent(prop_sim, accuracy = 0.001)),
                  vjust = -0.5, size = 4) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        labs(
          x = var, y = "Proporção de 'Sim'",
          title = paste("Proporção de", anom, "por", var)
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold"),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold")
        )
    }
  })
  
  #Univariado
  output$grafico_uni_var <- renderPlot({
    var <- input$variavel
    
    if (var %in% var_num) {
      ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "#5bc0de", color = "white") +
        labs(x = var, y = "Frequência", title = paste("Distribuição -", var)) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold"),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold")
        )
      
    } else if (var %in% var_cat) {
      ggplot(data, aes(x = as.factor(.data[[input$variavel]]))) +
        geom_bar(fill = "#5bc0de") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
        labs(x = var, y = "Frequência", title = paste("Frequência -", var)) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold"),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold")
          )
    }
  })
  
  #Título tabela
  output$titulo_tabela <- renderText({
    paste("Resumo - ", input$anomalia)
  })
  
  #Tabela resumo da anomalia
  output$tabela_anomalia <- renderTable({
    anom <- input$anomalia
    
    data %>%
      count(valor = .data[[anom]]) %>%
      mutate(
        Proporção = scales::percent(n / sum(n), accuracy = 0.01)
      ) %>%
      rename(!!input$anomalia := valor,
             `Frequência` = n)
  })
}


#Criar o Shiny APP
shinyApp(ui = ui, server = server)