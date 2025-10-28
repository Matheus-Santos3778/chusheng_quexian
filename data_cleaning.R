#importação da base
packages_list <- c('readxl', 'foreign', 'Epi', 'this.path', 'dplyr', 'stringr', 'tidyr')

#install.packages(packages_list)

lapply(packages_list, library, character.only=TRUE)

#----------------------------------------Importando bases e arquivos -------------------
setwd(this.path::here())
#utilizando o argumento "as.is=TRUE"para transformar os dados em caracteres
BANCOTOTAL<- read.dbf(file = 'data/BANCO_RES.dbf', as.is = TRUE)

#importar o arquivo em excel MUNICIPIOS DO PR com códigos. Para encontrar a pasta aqui usa-se 'path'
MUNICIPIOS<-read_excel(path = 'data/MUNICIPIOS_RS.xlsx',
sheet="Muni_cod",
skip=0)

# importar lista AC prioritária
LISTA_AC<-read_excel(path = 'data/LISTA_AC.xlsx',
sheet="COD",
skip=0)

regxmacro <- read.csv('data/regionaisxmacro.csv')

#============= selecionar algumas variaveis do BANCOTOTAL  =================================

# selecao de variaveis
BANCORESUMIDO <-  BANCOTOTAL |> 
  select(CODMUNNASC, CODESTAB, CODMUNRES, LOCNASC, IDADEMAE, ESTCIVMAE, ESCMAE, CODOCUPMAE, QTDFILVIVO, QTDFILMORT,QTDGESTANT, QTDPARTCES, QTDPARTNOR, GRAVIDEZ, PARTO, CONSULTAS, DTNASC, SEXO, APGAR1, APGAR5, RACACORMAE, PESO, IDANOMAL, CODANOMAL, DTCADASTRO, SEMAGESTAC, GESTACAO)

##Criando variáveis que identificam a presença de cada tipo de Anomalia
#Limpa os códigos na base de tipos (remove o "-descrição")
LISTA_AC[] <- lapply(LISTA_AC, function(x) sub("-.*", "", x))

#Para cada tipo, junta os códigos válidos em um vetor
tipos_lista <- apply(LISTA_AC[-1], 1, function(x) na.omit(x))

names(tipos_lista) <- LISTA_AC$AC

regxmacro$REGIONAL <- as.character(regxmacro$REGIONAL)

#Substitui NA por string vazia (para evitar erros no str_detect)
BANCORESUMIDO$CODANOMAL[is.na(BANCORESUMIDO$CODANOMAL)] <- ""

#Cria colunas booleanas TRUE/FALSE para cada tipo
for (tipo in names(tipos_lista)) {
  codigos_desse_tipo <- tipos_lista[[tipo]]
  padrao <- paste(codigos_desse_tipo, collapse = "|")  # expressão regex "Q000|M233|W223"
  
  BANCORESUMIDO[[tipo]] <- str_detect(BANCORESUMIDO$CODANOMAL, padrao)
}

MUNICIPIOS <- MUNICIPIOS %>% 
  distinct(MUNICIPIOS$'COD(6) tipo string', .keep_all = TRUE)

BANCORESUMIDO <- left_join(BANCORESUMIDO, MUNICIPIOS[, c("MUNICIPIOS$\"COD(6) tipo string\"", "COD(7)", "MACRO", "REGIONAL")], by = c("CODMUNRES" = "MUNICIPIOS$\"COD(6) tipo string\""))

cols_data <- c('DTNASC', 'DTCADASTRO')
BANCORESUMIDO[cols_data] <- lapply(BANCORESUMIDO[cols_data], as.Date, format= "%d%m%Y")

BANCORESUMIDO$ANO_NASC <- format(as.Date(BANCORESUMIDO$DTNASC), "%Y")

BANCORESUMIDO <- BANCORESUMIDO %>%
  rename('COD7' = 'COD(7)')

BANCORESUMIDO <- BANCORESUMIDO |>
  mutate(REGIONAL = paste0("410", sprintf("%02s", REGIONAL)))

ac_agrupadas <- c("Defeito do tubo Neural", "Microcefalia", "Cardiopatias congenitas", "Fendas Orais", "Órgãos genitais", "Defeitos de membros", "Defeitos de parede abdominal", "Sindrome de Dow")

prevalencias_mun <- BANCORESUMIDO %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(COD7, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_reg <- BANCORESUMIDO %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(REGIONAL, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_macro <- BANCORESUMIDO %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(MACRO, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_mun <- prevalencias_mun[!is.na(prevalencias_mun$COD7),]
prevalencias_reg <- prevalencias_reg[!is.na(prevalencias_reg$REGIONAL),]
prevalencias_macro <- prevalencias_macro[!is.na(prevalencias_macro$MACRO),]

prevalencias_reg <- left_join(prevalencias_reg, regxmacro[, c("REGIONAL", "MACRO")], by = c("REGIONAL" = "REGIONAL"))

prevalencias_reg <- left_join(prevalencias_reg, prevalencias_macro[, c("MACRO", "ANO_NASC", "anomalia" , "prevalencia")], by = c("MACRO", "ANO_NASC", "anomalia"))

prevalencias_reg <- prevalencias_reg %>%
  rename('prev_macro' = 'prevalencia.y')

prevalencias_reg <- prevalencias_reg %>%
  rename('prev_reg' = 'prevalencia.x')

write.csv(BANCORESUMIDO, "data/dados_finais.csv", row.names = FALSE)
write.csv(prevalencias_mun, "data/prev_mun.csv", row.names = FALSE)
write.csv(prevalencias_reg, "data/prev_reg.csv", row.names = FALSE)
