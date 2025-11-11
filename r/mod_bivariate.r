#Análise Bivariada
mod_bivariate_server <- function(input, output, session){

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
}
