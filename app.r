#Pacotes
packages_list <- c('shiny', 'bslib', 'thematic', 'tidyverse', 'gitlink', 'bslib', 'leaflet', 'geobr', 'dplyr')

lapply(packages_list, library, character.only=TRUE)

data <- read_csv("data/dados_finais.csv")
prevalencias <- read_csv("data/prevalencias.csv")

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

pr_mun <- geobr::read_municipality(code_muni = 41, year = 2020) %>%
  sf::st_transform(4326)

#Parâmetros da UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel(h2("Análise de Anomalias Congênitas", align="center")),
  
  fluidRow(
    column(4,
           selectInput("tipo_analise", "Tipo de análise:",
                       choices = c("Univariado", "Bivariado", "Temporal"))
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
  
  conditionalPanel(
    condition = "input.tipo_analise == 'Temporal'",
    fluidRow(
      column(
        width = 12,
        h3("Mapa Temporal de Prevalência por Município", class = "text-center"),
        sliderInput("ano_mapa", "Selecione o ano:",
                    min = 2013, max = 2022, value = 2013, step = 1,
                    sep = "", animate = TRUE),
        leafletOutput("mapa_anomalia", height = "600px")
      )
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
        plotOutput("grafico_bi", height = "500px")
      )
    } else if (input$tipo_analise == "Univariado") {
      fluidRow(
        plotOutput("grafico_uni", height = "500px"),
        tableOutput("tabela_resumo")
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
  output$grafico_uni <- renderPlot({
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
  
  #Temporal
  output$mapa_anomalia <- renderLeaflet({
    req(input$anomalia, input$ano_mapa)
    
    # filtra o DF de prevalências conforme anomalia e ano
    dados_filtrados <- prevalencias %>%
      filter(anomalia == input$anomalia, ANO_NASC == input$ano_mapa)
    
    # junta com o shape (garantir tipo numérico e código compatível)
    pr_mun <- pr_mun %>%
      mutate(code_muni = as.numeric(code_muni))
    
    dados_mapa <- pr_mun %>%
      left_join(dados_filtrados, by = c("code_muni" = "COD7"))
    
    # cria paleta de cores com base nas prevalências existentes
    dom <- dados_mapa$prevalencia
    dom_valid <- dom[!is.na(dom)]
    pal <- colorNumeric("YlOrRd",
                        domain = if (length(dom_valid) > 0) dom_valid else c(0, 1),
                        na.color = "gray90")
    
    leaflet(dados_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(prevalencia),
        color = "white", weight = 0.5,
        fillOpacity = 0.9,
        label = ~paste0(
          name_muni, "<br>",
          ifelse(is.na(prevalencia),
                 "Sem dados",
                 paste0(round(prevalencia, 2), " /10k nascidos"))
        ),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = dom_valid,
        title = paste("Prevalência de", input$anomalia, "-", input$ano_mapa),
        labFormat = labelFormat(suffix = " /10k")
      )
  })
}

#Criar o Shiny APP
shinyApp(ui = ui, server = server)
