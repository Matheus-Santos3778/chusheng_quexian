#Análise Temporal
mod_temporal_server <- function(input, output, session) {

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
  
  #Mapa
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
    
    # Paleta com domínio fixo da anomalia selecionada
    pal <- colorNumeric(
      palette = "Greens",
      domain = dom_fixo,
      na.color = "gray90"
    )
    
    # Renderiza o mapa
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
