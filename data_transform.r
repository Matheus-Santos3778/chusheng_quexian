#Depara das colunas
transform_data <- function(data){
  data$ESCMAE <- factor(x = data$ESCMAE,
                        levels = c("1", "2", "3", "4", "5", "9"),
                        labels = c("Nenhuma", "1a3anos", "4a7anos"
                                   , "8a11anos", "12anosemais", "ignorado"))
  
  data$LOCNASC <- factor(x = data$LOCNASC,
                         levels = c("1", "2", "3", "4"),
                         labels = c("Hospital", "Outros estabelecimentos de saúde"
                                    , "Domicílio", "Outros"))
  
  data$ESTCIVMAE <- factor(x=data$ESTCIVMAE,
                           levels = c("1", "2", "3", "4", "5", "9"),
                           labels = c("solteira", "Casada", "Viúva", "Separada jud.", "União estável", "Ignorada"))
  
  data$RACACORMAE <- factor(x=data$RACACORMAE,
                            levels = c("1", "2", "3", "4", "5"),
                            labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
  
  data$IDANOMAL <- factor(x=data$IDANOMAL,
                          levels = c("1", "2", "9"),
                          labels = c("Sim", "Não", "Ignorada"))
  
  data$SEXO[data$SEXO == "M"] <- 1
  data$SEXO[data$SEXO == "F"] <- 2
  data$SEXO[data$SEXO == "I"] <- 9
  
  data$SEXO <- factor(x=data$SEXO,
                      levels = c("1", "2", "9"),
                      labels = c("M", "F", "I"))
  
  data$CONSULTAS <- factor(x=data$CONSULTAS,
                           levels = c("1", "2", "3", "4", "9"),
                           labels = c("Nenhuma", "De 1 a 3", "De 4 a 6", "7 e mais", "Ignorado"))
  
  data$IDADEMAE[data$IDADEMAE == 99] <- NA
  
  #Transformando colunas para numéricas
  cols_num <- c("IDADEMAE", "QTDFILVIVO", "QTDFILMORT", "QTDGESTANT", "QTDPARTCES", "QTDPARTNOR", "GRAVIDEZ", "PARTO", "CONSULTAS", "APGAR1", "APGAR5", "PESO", "SEMAGESTAC", "GESTACAO")
  data[cols_num] <- lapply(data[cols_num], as.numeric)
  
  #Transformando colunas para datas
  cols_data <- c('DTNASC', 'DTCADASTRO')
  data[cols_data] <- lapply(data[cols_data], as.Date, format= "%d%m%Y")
  
  return(data)
}