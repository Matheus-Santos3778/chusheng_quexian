#Pacotes
packages_list <- c('shiny', 'bslib', 'thematic', 'tidyverse', 'gitlink', 'bslib', 'leaflet', 'geobr', 'dplyr', 'sf')

lapply(packages_list, library, character.only=TRUE)

data <- read_csv("data/dados_finais.csv")
prev_mun <- read_csv("data/prev_mun.csv")
prev_rm <- read_csv("data/prev_reg.csv")

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

pr_mun   <- read_municipality(code_muni = 41, year = 2020) %>% st_transform(4326)

pr_reg   <- read_health_region(year = 2013) %>% 
  filter(abbrev_state == "PR") %>% st_transform(4326)

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
        uiOutput("titulo_mapa"),
        fluidRow(
          column(4,
                 selectInput("nivel_geo", "Nível geográfico:",
                             choices = c("Municipal", "Regional", "Macroregional"),
                             selected = "Municipal")
          ),
          column(8,
                 sliderInput("ano_mapa", "Selecione o ano:",
                             min = 2013, max = 2022, value = 2013,
                             step = 1, sep = "", animate = TRUE)
          )
        ),
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
  
  #Título Mapa Temporal
  output$titulo_mapa <- renderUI({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    nivel_texto <- switch(
      input$nivel_geo,
      "Municipal" = "Município",
      "Regional" = "Regional de Saúde",
      "Macroregional" = "Macroregional de Saúde"
    )
    
    titulo <- paste0(
      "Prevalência de ", input$anomalia, 
      " por ", nivel_texto, 
      " em ", input$ano_mapa
    )
    
    h3(
      titulo,
      style = "text-align:center; margin-top:10px; font-weight:600;"
    )
  })
  
  #Temporal
  output$mapa_anomalia <- renderLeaflet({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    # 🔹 Dicionário com metadados de cada nível
    niveis <- list(
      "Municipal" = list(
        df = prev_mun,
        shape = pr_mun,
        join_key = c("code_muni" = "COD7"),
        col_prev = "prevalencia",
        label_col = "name_muni"
      ),
      "Regional" = list(
        df = prev_rm,
        shape = pr_reg,
        join_key = c("code_health_region" = "REGIONAL"),
        col_prev = "prev_reg",
        label_col = "name_health_region"
      ),
      "Macroregional" = list(
        df = prev_rm,
        shape = pr_reg,  # usa o mesmo shape do regional
        join_key = c("code_health_region" = "REGIONAL"),
        col_prev = "prev_macro",
        label_col = "name_health_region"
      )
    )
    
    # 🔹 Recupera as configurações do nível selecionado
    nivel_info <- niveis[[input$nivel_geo]]
    
    # 🔹 Filtra os dados correspondentes
    dados <- nivel_info$df %>%
      filter(anomalia == input$anomalia, ANO_NASC == input$ano_mapa)
    
    # 🔹 Faz o join com o shape
    dados_mapa <- nivel_info$shape %>%
      left_join(dados, by = nivel_info$join_key)
    
    # 🔹 Extrai os campos corretos
    var_prev <- dados_mapa[[nivel_info$col_prev]]
    label_nome <- dados_mapa[[nivel_info$label_col]]
    
    #Define domínio fixo por tipo geográfico e anomalia
    if (input$nivel_geo == "Municipal") {
      dom_fixo <- range(
        prev_mun$prevalencia[prev_mun$anomalia == input$anomalia],
        na.rm = TRUE
      )
    } else if (input$nivel_geo == "Regional") {
      dom_fixo <- range(
        prev_rm$prev_reg[prev_rm$anomalia == input$anomalia],
        na.rm = TRUE
      )
    } else { # Macroregional
      dom_fixo <- range(
        prev_rm$prev_macro[prev_rm$anomalia == input$anomalia],
        na.rm = TRUE
      )
    }
    
    # 🔹 Paleta com domínio fixo da anomalia selecionada
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = dom_fixo,
      na.color = "gray90"
    )
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = dom_fixo,
      na.color = "gray90"
    )
    
    # 🔹 Renderiza o mapa
    leaflet(dados_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(var_prev),
        color = "#BBB",         # cor da borda
        weight = 1,              # espessura da linha
        opacity = 1,             # opacidade da linha
        fillOpacity = 0.8,       # transparência do preenchimento
        smoothFactor = 0.2,      # suaviza os contornos
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(label_nome, ": ", round(var_prev, 1), " /10k")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = dom_fixo,
        title = paste("Prevalência de", input$anomalia, "-", input$ano_mapa),
        labFormat = labelFormat(suffix = " /10k")
      )
  })
}

#Criar o Shiny APP
shinyApp(ui = ui, server = server)
