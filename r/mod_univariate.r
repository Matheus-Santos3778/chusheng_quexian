#Análise Univariado
mod_univariate_server <- function(input, output, sessions){
  
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
}
