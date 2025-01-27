#------------------------------------------------------------------------------#
####------------------ LEITURA DOS mun_res -----------------------------------####
#------------------------------------------------------------------------------#   

# Instale a biblioteca 
install.packages("haven")
install.packages("read.dbc")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("survival")
install.packages("writexl")
install.packages("ggsurvplot")

# Carregue a biblioteca
library(haven)
#library(read.dbc)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(survival)
library(ggsurvplot)

# Caminho para a pasta "18"
caminho_pasta18 <- ".\\DataBase\\18"
# Caminho para a pasta "19"
caminho_pasta19 <- ".\\DataBase\\19"
# Caminho para a pasta "20"
caminho_pasta20 <- ".\\DataBase\\20"
# Caminho para a pasta "21"
caminho_pasta21 <- ".\\DataBase\\21"
# Caminho para a pasta "22"
caminho_pasta22 <- ".\\DataBase\\22"

# Lista de arquivos na pasta
arquivos_dbc18 <- list.files(caminho_pasta18, pattern = "\\.dbc$", full.names = TRUE)
arquivos_dbc19 <- list.files(caminho_pasta19, pattern = "\\.dbc$", full.names = TRUE)
arquivos_dbc20 <- list.files(caminho_pasta20, pattern = "\\.dbc$", full.names = TRUE)
arquivos_dbc21 <- list.files(caminho_pasta21, pattern = "\\.dbc$", full.names = TRUE)
arquivos_dbc22 <- list.files(caminho_pasta22, pattern = "\\.dbc$", full.names = TRUE)

# Crie uma lista para armazenar os data.frames
lista_dataframes18 <- lapply(arquivos_dbc18, read.dbc)
lista_dataframes19 <- lapply(arquivos_dbc19, read.dbc)
lista_dataframes20 <- lapply(arquivos_dbc20, read.dbc)
lista_dataframes21 <- lapply(arquivos_dbc21, read.dbc)
lista_dataframes22 <- lapply(arquivos_dbc22, read.dbc)

#passando para csv

library(writexl)

# Função para salvar cada lista de dataframes em um arquivo .xlsx
salvar_dataframes <- function(lista_dataframes, ano) {
  # Criar um nome de arquivo com o ano
  nome_arquivo <- paste0("dataframes_", ano, ".xlsx")
  
  # Salvar os dataframes da lista no arquivo Excel
  write_xlsx(lista_dataframes, nome_arquivo)
}

# Salvar cada lista de dataframes em arquivos .xlsx separados
salvar_dataframes(lista_dataframes18, 2018)
salvar_dataframes(lista_dataframes19, 2019)
salvar_dataframes(lista_dataframes20, 2020)
salvar_dataframes(lista_dataframes21, 2021)
salvar_dataframes(lista_dataframes22, 2022)
# Combinando as listas de data.frames
todas_listas <- c(lista_dataframes18, lista_dataframes19, lista_dataframes20, 
                  lista_dataframes21, lista_dataframes22)

# Use a função do.call para concatenar os data.frames
df_completo <- do.call(rbind, todas_listas)


df <- df_completo %>%
  mutate(DIAG_PRINC = as.character(DIAG_PRINC)) %>%
  filter(
    grepl("^C[0-26-9]", DIAG_PRINC) |  # Inclui C00 a C26 e C30 a C75
      grepl("^C3[0-9]", DIAG_PRINC) | # Inclui C30 a C75
      grepl("^D[0-4][0-8]", DIAG_PRINC)  # Inclui D00 a D48
  )


cid_dif <- df %>%
  mutate(DIAG_PRINC = as.character(DIAG_PRINC)) %>%
  group_by(DIAG_PRINC) %>%
  summarise(quantidade = n())


#------------------------------------------------------------------------------#
####---------------- ANALISE EXPLORATÓRIA DOS DADOS ------------------------####
#------------------------------------------------------------------------------# 

# Verificando variaveis e seus nomes do df
names(df) 
# Transformando em m
#names(df)<-toupper(names(df))

# Excluindo colunas que não serão utilizadas no estudo
colunas_a_excluir <- c("UTI_MES_IN", "UTI_MES_AN", "UTI_MES_AL",
                       "UTI_INT_IN","UTI_INT_AN","UTI_INT_AL",
                       "DIAR_ACOM","VAL_SADT","VAL_RN","VAL_ACOMP","VAL_ORTP",
                       "VAL_SANGUE","VAL_SADTSR","VAL_TRANSP","VAL_OBSANG","VAL_PED1AC",
                       "NATUREZA","NAT_JUR","DESTAO","RUBRICA","IND_VDRL",
                       "NACIONAL","NUM_PROC","TOT_PT_SP","CPF_AUT","HOMONIMO",
                       "SEQ_AIH5","CNAER","VINCPREV","GESTOR_COD",
                       "GESTOR_TP","GESTOR_CPF","GESTOR_DT","CNPJ_MANT","FINANC",
                       "FAEC_TP","REGCT","SEQUENCIA","REMESSA","AUD_JUST",
                       "SIS_JUST","VAL_SH_FED","VAL_SP_FED","VAL_SH_GES","VAL_SP_GES",
                       "VAL_UCI", "CNES","US_TOT","UF_ZI","DIA_SEC","DIA_SEC9","DIA_SEC8","TPDISEC8","TPDISEC9"
                       ,"N_AIH","CGC_HOSP","GESTAO","INFEHOSP","INSC_PN", "ETNIA","CID_MORTE")

# Excluindo as colunas 
df <- df[, !(names(df) %in% colunas_a_excluir), drop = FALSE]


#codigo que exclui apenas uma coluna
#df <- df[, -which(names(df) == "CNES")]

#Transformando relatorio em DF
mun_res <- read_excel("mun_res.xlsx")



#Transformando MUNIC_RES em integer para conseguir fazer o leftjoin
df$MUNIC_RES <- as.integer(as.character(df$MUNIC_RES))

# Enriquecendo com os nomes das regiões, 
mun_res$MUNIC_RES <- paste0(substr(mun_res$MUNIC_RES, 1, 6), substr(mun_res$MUNIC_RES, 8, nchar(mun_res$MUNIC_RES)))
df1_enriquecido <- merge(df, mun_res, by = 'MUNIC_RES', all.x = TRUE)
#rm(mun_res)

#Verificando quantos diferentes tem
region_dif <- df1_enriquecido %>%
  mutate(NOME_REG = as.character(NOME_REG)) %>%
  group_by(NOME_REG) %>%
  summarise(quantidade = n())

# Onde NOME_REG_RES é o Nome Região Geográfica Intermediária e NOME_UF_RES é o nome do estado
#Renomendo as duas colunas últimas colunas para não ficar igual ao outro df
df1_enriquecido <- df1_enriquecido %>%
  rename(NOME_REG_RES = NOME_REG, NOME_UF_RES = Nome_EST)
#OBS: os mun_res de MUNIC_RES são os codigos dos municipios de residencia dos pacientes


#Testando outra planilha do excel
#Esse dá 344k de NA
# df_enriquecido <- merge(df, mun_res, by = 'MUNIC_RES', all.x = TRUE)
# df_enriquecido <- df_enriquecido %>% rename(res_regiao = value)
# 
# regiao_dif <- df_enriquecido %>%
#   mutate(res_regiao = as.character(res_regiao)) %>%
#   group_by(res_regiao) %>%
#   summarise(quantidade = n())



#Agora é preciso enriquecer com os mun_res MUN_MOV esses são os CODIGOS DOS municipios 
#dos estabelecimentos

# 'MUNIC_MOV' é a chave no primeiro dataframe e 'MUNIC_RES' no segundo
df_enriq_mov <- merge(df1_enriquecido, mun_res, by.x = 'MUNIC_MOV', by.y = 'MUNIC_RES', all.x = FALSE)

# Excluindo as duas colunas a mais que foi criada

df_enriq_mov <- subset(df_enriq_mov, select = -c(NOME_REG, Nome_EST))

#Renomendo as duas colunas últimas colunas para não ficar igual ao outro df

df_enriq_mov <- df_enriq_mov %>%
  rename(NOME_REG_EST = NOME_REG_RES, NOME_UF_EST = NOME_UF_RES)

# Atualiza df1_enriquecido com o resultado do merge em df_enriq_mov/// EST = ESTABELECIMENTO
df1_enriquecido <- cbind(df1_enriquecido, df_enriq_mov[, c("NOME_REG_EST", "NOME_UF_EST")])


#------------------------------------------------------------------------------------------------------#
#Arrumando a coluna DT_INTER que está em factor para data no formato yyyyMMdd 

df1_enriquecido$DT_INTER <- as.Date(df1_enriquecido$DT_INTER, format = "%Y%m%d")

#Arrumando a coluna DT_SAIDA que está em factor para data no formato yyyyMMdd 

df1_enriquecido$DT_SAIDA <- as.Date(df1_enriquecido$DT_SAIDA, format = "%Y%m%d")

#Arrumando a coluna NASC que está em factor para data no formato yyyyMMdd 

df1_enriquecido$NASC <- as.Date(df1_enriquecido$NASC, format = "%Y%m%d")

#Arrumando a coluna "SEXO" que está com dois fatores "1" e "3" e vamos mudar para M e F, respectivamente

df1_enriquecido$SEXO <- factor(df1_enriquecido$SEXO, levels = c("1", "3"), labels = c("M", "F"))

#Arrumando a coluna "IDENT" que está com dois fatores "1 e 5", mudar para {1:"Normal", 5:"Longa permanência"}

df1_enriquecido$IDENT <- factor(df1_enriquecido$IDENT, levels = c("1", "5"), labels = c("Normal", "Longa permanência"))

#Arrumando a coluna "CEP" que está em factor para inteiro

df1_enriquecido$CEP <- as.integer(as.character(df1_enriquecido$CEP)) #olhar depois com calma

# Tamanho esperado: 8 digitos 
df1_enriquecido %>% filter(str_length(CEP)==8) %>% View() 
#Vimos que CEP está com 287,622 dados < 8 e ==8 423,097 dados
#Não sei se o que fazer para tratar -.-


#Arrumando a coluna "COMPLEX" que está com dois fatores "02 e 03", mudar para{01:"Atenção Básica", 02:"Média complexidade", 03:"Alta complexidade", 00:"", 99:""}

df1_enriquecido$COMPLEX <- factor(df1_enriquecido$COMPLEX, levels = c("02", "03"), labels = c("Média complexidade", "Alta complexidade"))


#Arrumando a coluna "NUM_FILHOS" que está como inteiro, precisa mudar para factor e mudar a legenda.


df1_enriquecido$NUM_FILHOS <- as.factor(df1_enriquecido$NUM_FILHOS)

df1_enriquecido$NUM_FILHOS <- factor(
  df1_enriquecido$NUM_FILHOS,
  levels = 0:10,
  labels = c(
    "Sem filhos/Não inform",
    "1 filho",
    "2 filhos",
    "3 filhos",
    "4 a 5 filhos",
    "4 a 5 filhos",
    "6 a 9 filhos",
    "6 a 9 filhos",
    "6 a 9 filhos",
    "6 a 9 filhos",
    "10 filhos ou mais"
  )
)


# A coluna "DIAS_PERM"  está como inteiro.... 

# Arrumando a coluna "CAR_INT" CARATER DA INTERNAÇÃO, está em factor e mudarei a legenda

df1_enriquecido$CAR_INT <- factor(
  df1_enriquecido$CAR_INT,
  levels = c("01", "02", "03", "04", "05", "06"),
  labels = c(
    "Eletivo",
    "Urgência",
    "Acidente no local de trabalho ou a serviço da empresa",
    "Acidente no trajeto para o trabalho",
    "Outros tipos de acidente de trânsito",
    "Outros tipos de lesões e envenenamento por agentes químicos e físicos"
  )
)


# Excluir N_AIH #Está só mostrando números que não servirão


#Arrumando a coluna "MARCA_UTI" INDICA QUAL UTI FOI UTILIZADA. Colocando legenda

# Supondo que 'df1_enriquecido' é o nome do seu dataframe
df1_enriquecido$MARCA_UTI <- factor(
  df1_enriquecido$MARCA_UTI,
  levels = c("00", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "85", "86", "99", "01"),
  labels = c(
    "Não utilizou UTI",
    "UTI adulto - tipo I",
    "UTI adulto - tipo II",
    "UTI adulto - tipo III",
    "UTI infantil - tipo I",
    "UTI infantil - tipo II",
    "UTI infantil - tipo III",
    "UTI neonatal - tipo I",
    "UTI neonatal - tipo II",
    "UTI neonatal - tipo III",
    "UTI de queimados",
    "UTI coronariana tipo II - UCO tipo II",
    "UTI coronariana tipo III - UCO tipo III",
    "UTI Doador",
    "Utilizou mais de um tipo de UTI"
  )
)

#Arrumando a coluna "COBRANCA" com suas legendas.

# Supondo que 'df1_enriquecido' é o nome do seu dataframe
df1_enriquecido$COBRANCA <- factor(
  df1_enriquecido$COBRANCA,
  levels = c(
    "11", "12", "14", "15", "16", "18", "19", "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "32", "31", "41", "42", "43", "51", "61", "17",
    "62", "13", "63", "64", "65", "66", "67"
  ),
  labels = c(
    "Alta curado", "Alta melhorado", "Alta a pedido",
    "Alta com previsão de retorno para acompanhamento do paciente",
    "Alta por evasão", "Alta por outros motivos",
    "Alta de paciente agudo em psiquiatria",
    "Permanência por características próprias da doença",
    "Permanência por intercorrência",
    "Permanência por impossibilidade sócio-familiar",
    "Permanência proc doação órg, tec, cél-doador vivo",
    "Permanência proc doação órg, tec, cél-doador morto",
    "Permanência por mudança de procedimento",
    "Permanência por reoperação", "Permanência por outros motivos",
    "Transferência para internação domiciliar", 
    "Transferência para internação domiciliar", 
    "Transferência para outro estabelecimento",
    "Óbito com DO fornecida pelo médico assistente",
    "Óbito com DO fornecida pelo IML",
    "Óbito com DO fornecida pelo SVO",
    "Encerramento administrativo",
    "Alta da mãe/puérpera e do recém-nascido",
    "Alta da mãe/puérpera e do recém-nascido",
    "Alta da mãe/puérpera e permanência recém-nascido",
    "Alta da mãe/puérpera e permanência recém-nascido",
    "Alta da mãe/puérpera e óbito do recém-nascido",
    "Alta da mãe/puérpera com óbito fetal",
    "Óbito da gestante e do concepto",
    "Óbito da mãe/puérpera e alta do recém-nascido",
    "Óbito da mãe/puérpera e permanência recém-nascido"
  )
)




####Excluir "CGC_HOSP"### EXCLUIIR GESTAO

# Arrumando a coluna "COD_IDADE" que indica se é meses, dias ou anos a idade

# Supondo que 'df1_enriquecido' é o nome do seu dataframe
df1_enriquecido$COD_IDADE <- factor(
  df1_enriquecido$COD_IDADE,
  levels = c("2", "3", "4", "0"),
  labels = c("Dias", "Meses", "Anos", "")
)


# Arrumando a coluna "MORTE" com dois fatores {0:"Sem óbito", 1:"Com óbito"}	

df1_enriquecido$MORTE <- factor(
  df1_enriquecido$MORTE,
  levels = c("0", "1"),
  labels = c("Sem óbito", "Com óbito")
)

#Arrumando a coluna "INSTRU"

df1_enriquecido$INSTRU <- factor(
  df1_enriquecido$INSTRU,
  levels = c("1", "2", "3", "4", "0", "9"),
  labels = c("Analfabeto", "1º grau", "2º grau", "3º grau", "Sem Info", "Sem Info")
)

#Arrumando as colunas CONTRACEP1 e CONTRACEP2

df1_enriquecido$CONTRACEP1 <- factor(
  df1_enriquecido$CONTRACEP1,
  levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "00", "99"),
  labels = c(
    "LAM", "Ogino Kaus", "Temperatura basal", "Billings", "Cinto térmico", "DIU",
    "Diafragma", "Preservativo", "Espermicida", "Hormônio oral", "Hormônio injetável",
    "Coito interrompido", "", ""
  )
)

df1_enriquecido$CONTRACEP2 <- factor(
  df1_enriquecido$CONTRACEP2,
  levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "00", "99"),
  labels = c(
    "LAM", "Ogino Kaus", "Temperatura basal", "Billings", "Cinto térmico", "DIU",
    "Diafragma", "Preservativo", "Espermicida", "Hormônio oral", "Hormônio injetável",
    "Coito interrompido", "", ""
  )
)


df1_enriquecido$GESTRISCO <- factor(
  df1_enriquecido$GESTRISCO,
  levels = c("0", "1"),
  labels = c("Não", "Sim")
)


#Excluir INFEHOSP pois só tem NA e INSC_PN POIS SÓ É UM MONTE DE NÚMERO

#ARRUMANDO A COLUNA "RACA_COR" 

df1_enriquecido$RACA_COR <- factor(
  df1_enriquecido$RACA_COR,
  levels = c("01", "02", "03", "04", "05", "99"),
  labels = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "Sem Informação")
)


#Arrumando a coluna "MARCA_UCI"

df1_enriquecido$MARCA_UCI <- factor(
  df1_enriquecido$MARCA_UCI,
  levels = c("00", "01", "02", "03"),
  labels = c(
    "Não utilizou UCI",
    "Unidade de cuidados intermediários neonatal convencional",
    "Unidade de cuidados intermediários neonatal canguru",
    "Unidade intermediária neonatal"
  )
)
# Enriquecendo as colunas de tipo de diagnostico sec


colunas_tipo <- c("TPDISEC1", "TPDISEC2", "TPDISEC3", "TPDISEC4", "TPDISEC5", "TPDISEC6", "TPDISEC7")

# Loop para percorrer as colunas e aplicar as transformações
for (coluna in colunas_tipo) {
  df1_enriquecido[[coluna]] <- factor(
    df1_enriquecido[[coluna]],
    levels = c("1", "2", "0"),
    labels = c("Pré-existente", "Adquirido", "Sem Informação")
  )
}

#IDADE Já ESTA TRATADA

################Pergntar se precisa enriquecer CBO, PROC_REA, ESPEC, PROC_SOLIC

############################## #perguntar tbm se precisa criar variável def_nome_da_indentificacao para cada um #
#exemplo SEXO TEM 3 e def_sexo teria FEMININO

###################################################################################################################

#------------------------------------------------------------------------------#
####------------------------- CRIAÇÃO DE VARIÁVEL --------------------------####
#------------------------------------------------------------------------------#

#Variavel Faixa Etaria

df1_enriquecido$faixa_idade_anos <- cut(
  df1_enriquecido$IDADE,
  breaks = c(0, 19, 31, 46, 61, 76, Inf),  # Novos intervalos de idade
  labels = c("0-18", "19-30", "31-45", "46-60", "61-75", "76+"),  # Novas faixas
  right = FALSE  # Define se o intervalo deve incluir o limite superior
)


#CRIAR VARIAVEL  tempo de internação em
#hospitais. Neste caso, a censura ocorre quando o indivíduo morre antes da alta



df1_enriquecido$tempo <- df1_enriquecido$DT_SAIDA - df1_enriquecido$DT_INTER



df1_enriquecido$censura <- ifelse(df1_enriquecido$MORTE == "Sem óbito", 1, 
                            ifelse(df1_enriquecido$MORTE == "Com óbito", 0, NA))


# Vamos trabalhar apenas com dados em que tempo menor que 100 porque é dificil que um paciente
#com neoplasia fique tantos dias internados

tempo_100 <- df1_enriquecido %>% filter(tempo<100)
tempo_00 <- subset(tempo_100, tempo > 0 & tempo < 100)
 
#LEMBRANDO QUE #{1:"Sem óbito", 0:"Com óbito"}

tempo_100$censura <- ifelse(tempo_100$MORTE == "Sem óbito", 1, 0)

cens <- tempo_100$censura

tempo <- tempo_100$tempo

table(cens)  # 0 é a censura que é a morte e 1 é a falha que é a alta

#cens
#    0      1 
#73536 636858  

#Criar variavel REGIAO

tempo_100 <- tempo_100 %>%
  mutate(
    REGIAO = case_when(
      substr(CEP, 1, 1) == "0" ~ "Sudeste",
      substr(CEP, 1, 1) == "1" ~ "Sudeste",
      substr(CEP, 1, 1) == "2" ~ "Sudeste",
      substr(CEP, 1, 1) == "3" ~ "Sudeste",
      substr(CEP, 1, 1) == "4" ~ "Nordeste",
      substr(CEP, 1, 1) == "5" ~ "Nordeste",
      substr(CEP, 1, 1) == "6" ~ "Norte",
      substr(CEP, 1, 1) == "7" ~ "Centro-Oeste",
      substr(CEP, 1, 1) == "8" ~ "Sul",
      substr(CEP, 1, 1) == "9" ~ "Sul",
      TRUE ~ "Desconhecido"
    )
  )




#Criar uma nova variavel motivo da internaca que 1 se for urgencia e 0 se for eletivo 

tempo_100 <- tempo_100 %>%
  mutate(motivo = ifelse(CAR_INT == "Urgência", 0, 
                         ifelse(CAR_INT == "Eletivo", 1, NA)))

#Criar uma nova variavel genero 

tempo_100 <- tempo_100 %>%
  mutate(genero = ifelse(SEXO == "F", 0, 
                         ifelse(SEXO == "M", 1, NA)))


# Contar o número de CIDs diferentes na coluna DIAG_PRINC
num_cids_diferentes <- tempo_100 %>%
  summarise(num_diferentes = n_distinct(DIAG_PRINC))

print(num_cids_diferentes)
# Temos 706

# Contando a frequência de cada CID
frequencia_cid <- table(tempo_100$DIAG_PRINC)

# Visualizando as contagens
print(frequencia_cid)


# Convertendo a coluna DIAG_PRINC em fator
tempo_100$DIAG_PRINC <- as.factor(tempo_100$DIAG_PRINC)
# Convertendo a coluna DIAG_PRINC em fator
tempo_100$CAR_INT <- as.factor(tempo_100$CAR_INT)

# Verificando a conversão
str(tempo_100$DIAG_PRINC)  # Ver o teipo
levels(tempo_100$DIAG_PRINC)  # Lista os níveis (diferentes CIDs)


#Analise Exploratoria

#Quero verificar a partir das variaveis que tenho sobre o carater da internacao que ficaram vivos apos os 40 dias
#e quero tbm ver se os que morrem possui carater de internacao grave


# Contagem de pacientes que sobreviveram mais de 40 dias
total_sobreviventes_40 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo > 40))

# Contagem de pacientes que morreram após 40 dias
total_obitos_apos_40 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo > 40))

# Contagem de pacientes que morreram após 40 dias e têm "car_int" Urgência
total_obitos_grave_apos_40 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo > 40 & CAR_INT == "Urgência"))

# Contagem total de pacientes no dataset
total_pacientes <- nrow(tempo_100)

# Calcular porcentagens
porcentagem_sobreviventes_40 <- (total_sobreviventes_40 / total_pacientes) * 100
porcentagem_obitos_apos_40 <- (total_obitos_apos_40 / total_pacientes) * 100
porcentagem_obitos_grave_apos_40 <- (total_obitos_grave_apos_40 / total_pacientes) * 100

# Criar uma tabela organizada com as contagens e porcentagens
tabela_resultados <- data.frame(
  Categoria = c("Sobreviventes > 40 dias", "Óbitos após 40 dias", "Óbitos após 40 dias com 'car_int' Urgência"),
  Contagem = c(total_sobreviventes_40, total_obitos_apos_40, total_obitos_grave_apos_40),
  Porcentagem = c(
    round(porcentagem_sobreviventes_40, 2), 
    round(porcentagem_obitos_apos_40, 2), 
    round(porcentagem_obitos_grave_apos_40, 2)
  )
)

# Exibir a tabela
print(tabela_resultados)
#--------------------------------------------------------------------------------------------------------------------
# Contagem de pacientes que viveram menos de 20 dias
total_sobreviventes_menor_20 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo < 20))

# Contagem de pacientes que morreram após 20 dias
total_obitos_apos_40 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20))

# Contagem de óbitos menores que 20 dias com "car_int" Urgência
total_obitos_urgencia_15 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Urgência"))

# Contagem de óbitos menores que 20 dias com "car_int" Eletivo
total_obitos_eletivo_15 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Eletivo"))

# Contagem total de pacientes no dataset
total_pacientes <- nrow(tempo_100)

# Calcular porcentagens
porcentagem_sobreviventes_menor_20 <- (total_sobreviventes_menor_20 / total_pacientes) * 100
porcentagem_obitos_apos_40 <- (total_obitos_apos_40 / total_pacientes) * 100
porcentagem_obitos_urgencia_15 <- (total_obitos_urgencia_15 / total_pacientes) * 100
porcentagem_obitos_eletivo_15 <- (total_obitos_eletivo_15 / total_pacientes) * 100

# Criar uma tabela organizada com as contagens e porcentagens
tabela_resultados2 <- data.frame(
  Categoria = c("Sobreviventes < 20 dias", 
                "Óbitos menos 20 dias", 
                "Óbitos < 20 dias com 'car_int' Urgência", 
                "Óbitos < 20 dias com 'car_int' Eletivo"),
  Contagem = c(total_sobreviventes_menor_20, 
               total_obitos_apos_40, 
               total_obitos_urgencia_15, 
               total_obitos_eletivo_15),
  Porcentagem = c(
    round(porcentagem_sobreviventes_menor_20, 2), 
    round(porcentagem_obitos_apos_40, 2), 
    round(porcentagem_obitos_urgencia_15, 2), 
    round(porcentagem_obitos_eletivo_15, 2)
  )
)

# Exibir a tabela
print(tabela_resultados2)
#-----------------------------------------------------------------------------------------------------------------
# Contagem de óbitos após 20 dias com "car_int" Urgência
total_obitos_urgencia_apos_20 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Urgência"))

# Contagem de óbitos após 20 dias com "car_int" Eletivo
total_obitos_eletivo_apos_20 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Eletivo"))

# Calcular porcentagens em relação ao total de óbitos após 20 dias
porcentagem_obitos_urgencia_apos_20 <- (total_obitos_urgencia_apos_20 / 65355) * 100
porcentagem_obitos_eletivo_apos_20 <- (total_obitos_eletivo_apos_20 / 65355) * 100

# Criar uma tabela organizada com os resultados
tabela_obitos_apos_20 <- data.frame(
  Categoria = c("Óbitos menor 20 dias com 'CAR_INT' Urgência", 
                "Óbitos menor 20 dias com 'CAR_INT' Eletivo"),
  Contagem = c(total_obitos_urgencia_apos_20, total_obitos_eletivo_apos_20),
  Porcentagem = c(
    round(porcentagem_obitos_urgencia_apos_20, 2), 
    round(porcentagem_obitos_eletivo_apos_20, 2)
  )
)

# Exibir a tabela
print(tabela_obitos_apos_20)
#-------------------------------------------------------------------------------------------------------------------#

# Criar uma tabela de contingência entre MORTE e car_int
tabela_morte_carint <- table(tempo_100$MORTE, tempo_100$motivo)

# Exibir a tabela de contingência
print(tabela_morte_carint)

# Realizar o teste de qui-quadrado de independência
teste_chi2 <- chisq.test(tabela_morte_carint)

# Exibir o resultado do teste
print(teste_chi2)

# Exibir os valores esperados (esperados se não houver associação)
print(teste_chi2$expected)
#-------------------------------------------------------------------------------
# Filtrar os sobreviventes (<20 dias) com car_int "Urgência"
sobreviventes_urgencia_menor_20 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo < 20 & CAR_INT == "Urgência"))

# Filtrar os sobreviventes (<20 dias) com car_int "Eletivo"
sobreviventes_eletivo_menor_20 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo < 20 & CAR_INT == "Eletivo"))

# Filtrar os sobreviventes (>40 dias) com car_int "Urgência"
sobreviventes_urgencia_maior_40 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo > 40 & CAR_INT == "Urgência"))

# Filtrar os sobreviventes (>40 dias) com car_int "Eletivo"
sobreviventes_eletivo_maior_40 <- nrow(subset(tempo_100, MORTE == "Sem óbito" & tempo > 40 & CAR_INT == "Eletivo"))

# Criar uma tabela organizada com os resultados
tabela_sobreviventes <- data.frame(
  Categoria = c(
    "Sobreviventes < 20 dias com 'car_int' Urgência", 
    "Sobreviventes < 20 dias com 'car_int' Eletivo",
    "Sobreviventes > 40 dias com 'car_int' Urgência", 
    "Sobreviventes > 40 dias com 'car_int' Eletivo"
  ),
  Contagem = c(
    sobreviventes_urgencia_menor_20, 
    sobreviventes_eletivo_menor_20,
    sobreviventes_urgencia_maior_40, 
    sobreviventes_eletivo_maior_40
  )
)

# Exibir a tabela
print(tabela_sobreviventes)
#---------------------------------------------------------------------------------

# Filtrar os óbitos (<20 dias) com car_int "Urgência"
obitos_urgencia_menor_20 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Urgência"))

# Filtrar os óbitos (<20 dias) com car_int "Eletivo"
obitos_eletivo_menor_20 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo < 20 & CAR_INT == "Eletivo"))

# Filtrar os óbitos (>40 dias) com car_int "Urgência"
obitos_urgencia_maior_40 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo > 40 & CAR_INT == "Urgência"))

# Filtrar os óbitos (>40 dias) com car_int "Eletivo"
obitos_eletivo_maior_40 <- nrow(subset(tempo_100, MORTE == "Com óbito" & tempo > 40 & CAR_INT == "Eletivo"))

# Criar uma tabela organizada com os resultados
tabela_obitos <- data.frame(
  Categoria = c(
    "Óbitos < 20 dias com 'car_int' Urgência", 
    "Óbitos < 20 dias com 'car_int' Eletivo",
    "Óbitos > 40 dias com 'car_int' Urgência", 
    "Óbitos > 40 dias com 'car_int' Eletivo"
  ),
  Contagem = c(
    obitos_urgencia_menor_20, 
    obitos_eletivo_menor_20,
    obitos_urgencia_maior_40, 
    obitos_eletivo_maior_40
  )
)

# Exibir a tabela
print(tabela_obitos)

#--------------------------------------------------------------------------------------------------------------------
#Kaplan Meier -- tempo e a censura 

ekm<-survfit(Surv(tempo,cens)~1)
summary(ekm) 
st<-ekm$surv
temp<-ekm$time
invst<-qnorm(st) 
plot(ekm, main="Curva de Kaplan-Meier", xlab="Tempo", ylab="Probabilidade de sobrevivência")


plot(temp, -log(st),pch=4,xlab="Tempos",ylab="-log(S(t))")

plot(log(temp), log(-log(st)),pch=4,xlab="log(tempo)",ylab="log(-log(S(t)))")
 
  
ekm<-survfit(Surv(tempo,cens)~1) 
summary(ekm) 
st<-ekm$surv 
temp<-ekm$time 
invst<-qnorm(st) 
par(mfrow=c(1,3)) 
plot(temp, -log(st),pch=16,xlab="Tempos",ylab="-log(S(t))") 
plot(log(temp),log(-log(st)),pch=16,xlab="log(tempos)",ylab="log(-log(S(t))") 
plot(log(temp),invst,pch=16,xlab="log(tempos)",ylab=expression(Phi^-1*(S(t))))

#Weibull e Exponencial parecem ser as melhores candidatas

#agregate para verificar o tempo medio entre grupo  

tempo_medio_motivo <- aggregate(tempo ~ motivo, data = tempo_100, FUN = mean)
tempo_medio_motivo

tempo_medio_sexo <- aggregate(tempo ~ genero, data = tempo_100, FUN = mean)
tempo_medio_sexo

#kaplan meier por sexo 
# Instalar e carregar pacotes necessários
install.packages("survminer")  # Se ainda não estiver instalado
library(survival)
library(survminer)

# Criar o objeto Survfit e gerar o gráfico Kaplan-Meier
ekm1 <- survfit(Surv(tempo_100$tempo, tempo_100$censura) ~ tempo_100$SEXO)
summary(ekm1)

# Extrair dados de sobrevivência
st1 <- ekm1$surv
temp1 <- ekm1$time
invst1 <- qnorm(st1)

# Plotar a curva Kaplan-Meier
plot(ekm1, main="Curva de Kaplan-Meier", xlab="Tempo", ylab="Probabilidade de sobrevivência")

# Gerar gráfico com ggsurvplot
ggsurvplot(
  ekm1,
  data = tempo_100,
  pval = FALSE,                # Valor-p
  conf.int = TRUE,             # Intervalo de confiança
  risk.table = FALSE,          # Tabela de risco
  ggtheme = theme_minimal(),   # Tema minimalista
  xlab = "Tempo (dias)",       # Eixo x
  ylab = "Probabilidade de Sobrevivência", # Eixo y
  title = "Curva de Sobrevivência por Sexo", # Título
  legend.title = "Sexo",       # Legenda
  legend.labs = c("Masculino", "Feminino"), # Labels
  palette = c("#E7B800", "#2E9FDF")         # Cores
)


#colocar uma tabela com descritivas



#-------------------------------------------------------------------------------
#                           Algumas descritivas

#Como é a contagem de pacientes por SEXO E OBITO???


tempo_100 <- tempo_100 %>%
  mutate(MORTE = factor(MORTE, levels = c("Sem óbito", "Com óbito")))

# Contagem por SEXO e morte
contagem <- tempo_100 %>%
  group_by(SEXO, MORTE) %>%
  summarise(contagem = n(), .groups = 'drop')

# Calcular a porcentagem 
contagem <- contagem %>%
  group_by(SEXO) %>%
  mutate(total = sum(contagem),
         porcentagem = (contagem / total) * 100)

# Exibir a contagem com porcentagens
print(contagem)

# Criar um gráfico de barras com porcentagens
ggplot(contagem, aes(x = SEXO, y = contagem, fill = MORTE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", porcentagem)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Sem óbito" = "#1f77b4", "Com óbito" = "#ff7f0e"), 
                    labels = c("Sem óbito", "Com óbito")) +
  labs(title = "Contagem de Pacientes por Sexo e Óbito",
       x = "Sexo",
       y = "Contagem",
       fill = "Óbito") +
  theme_minimal()


#Qual sexo é mais acometido por neoplasia? 

contagem_MF <- tempo_100 %>%
  group_by(SEXO) %>%
  summarise(contagem = n())
contagem_MF
#Pelos resultados, homens porem está bem dividido

#Em quais regioes do brasil mais morre com essa doenca ?

# Filtrar apenas os pacientes com "Com óbito"
df_com_obito <- tempo_100 %>%
  filter(MORTE == "Com óbito")

# Contagem de óbitos por SEXO e REGIAO
contagem_obitos <- df_com_obito %>%
  group_by(SEXO, REGIAO) %>%
  summarise(contagem = n(), .groups = 'drop')

# Total de pacientes por REGIAO
total_por_regiao <- tempo_100 %>%
  group_by(REGIAO) %>%
  summarise(total = n(), .groups = 'drop')

# Juntar os dados e calcular a porcentagem
contagem <- contagem_obitos %>%
  left_join(total_por_regiao, by = "REGIAO") %>%
  mutate(porcentagem = (contagem / total_pacientes) * 100)

# Exibir a contagem com porcentagens
print(contagem)

ggplot(contagem, aes(x = REGIAO, y = contagem, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # Adicionar contagem acima da barra
  geom_text(aes(label = contagem), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  
  # Adicionar porcentagem dentro da barra
  #geom_text(aes(label = sprintf("%.1f%%", porcentagem)), 
            #position = position_dodge(width = 0.9), vjust = 1.5, color = "white", size = 3.5) +
  
  labs(title = "Contagem de Pacientes com Óbito por Sexo e Região",
       x = "Região",
       y = "Contagem",
       fill = "Sexo") +
  
  theme_minimal()



#Qual o CID que mais apareceu??

# Ordenando a tabela de frequência em ordem decrescente
frequencia_cid_ordenada <- sort(frequencia_cid, decreasing = TRUE)

# Selecionando os 3 CIDs mais frequentes
top_3_cids <- head(frequencia_cid_ordenada, 3)

# Visualizando os 3 CIDs mais frequentes e suas contagens
print(top_3_cids)

# D259   C61   C20 
#42869 39332 30312 
#------------------------------------------------------------------

# Filtrar os diagnósticos principais e separar por CID, SEXO e MORTE
contagem_por_cid_sexo_morte <- tempo_100 %>%
  filter(DIAG_PRINC %in% c("D259", "C61", "C20")) %>%   # Filtrar os diagnósticos principais
  group_by(DIAG_PRINC, SEXO, MORTE) %>%                 # Agrupar por CID, sexo e óbito
  summarise(contagem = n())                             # Contar casos
arrange(DIAG_PRINC, SEXO, MORTE)                      # Ordenar por CID, sexo e óbito

# Visualizar o resultado
print(contagem_por_cid_sexo_morte)
#-----------------------------------------------
obitos_cids_especificos1 <- subset(df_com_obito, DIAG_PRINC %in% c("c17", "C50", "C20"))
obitos_cids_especificos1

# Verificar se há diagnósticos com os CIDs "C17" ou "C50" na variável DIAG_PRINC
# Contar registros com os CIDs "C17" ou "C50" na variável DIAG_PRINC
contagem_cid_c17_c50 <- sum(tempo_100$DIAG_PRINC %in% c("C17", "C50"))

# Exibir a contagem
contagem_cid_c17_c50

# Contar registros com CID "C17"
contagem_c17 <- sum(df_completo$DIAG_PRINC == "C17" & df_completo$MORTE == 1)

# Contar registros com CID "C50"
contagem_c50 <- sum(df_completo$DIAG_PRINC == "C50" & df_completo$MORTE == 1)
   (df_completo$DIAG_PRINC == "C50")

cancer_mama <- df_completo %>%
  filter(DIAG_PRINC == "C50"& df_completo$MORTE == 1)

media_cancermama <- summary(cancer_mama$IDADE)

# Criar uma tabela para exibir os resultados
tabela_cid <- data.frame(
  CID = c("C17", "C50"),
  Contagem = c(contagem_c17, contagem_c50)
)

# Exibir a tabela
print(tabela_cid)



  #-----------------------------------------------
#Desses tres que mais apareceram, quantos pacientes morreram?

# Filtrando para os CIDs específicos
obitos_cids_especificos <- subset(df_com_obito, DIAG_PRINC %in% c("D259", "C61", "C20"))

obitos_cids_c20 <- subset(df_com_obito, DIAG_PRINC %in% c("c20"))
media_idade = summary(obitos_cids_c61$IDADE)
media_idade
# Contando o número de óbitos para cada CID específico
contagem_obitos_por_cid <- table(obitos_cids_especificos$DIAG_PRINC)

# Exibindo o resultado
print(contagem_obitos_por_cid)

#Qual foi o CID que mais matou?

# Filtrar apenas os pacientes com "Com óbito"
df_com_obito <- tempo_100 %>%
  filter(MORTE == "Com óbito")

# Contando a frequência dos CIDs em casos com óbito
frequencia_cid_obito <- table(df_com_obito$DIAG_PRINC)

# Ordenando a tabela de frequência em ordem decrescente
frequencia_cid_obito_ordenada <- sort(frequencia_cid_obito, decreasing = TRUE)

# Selecionando os 3 CIDs mais frequentes entre os casos com óbito
top_3_cids_obito <- head(frequencia_cid_obito_ordenada, 3)

# Visualizando os 3 CIDs mais frequentes e suas contagens
print(top_3_cids_obito)

#Qual a faixa etaria que teve mais obito?

# Contando o número de óbitos por faixa etária
contagem_faixa_idade <- table(df_com_obito$faixa_idade_anos)

# Visualizando a contagem
print(contagem_faixa_idade)

# Plotando o gráfico de barras
contagem_faixa_idade <- data.frame(
  Faixa = c("0-18", "19-30", "31-45", "46-60", "61-75", "76+"),
  Quantidade = c(983, 1188, 4248, 18729, 32355, 16033)
)

# Plotando o gráfico com ggplot2
ggplot(contagem_faixa_idade, aes(x = Faixa, y = Quantidade)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Barras com contorno preto
  geom_text(aes(label = Quantidade), vjust = -0.5, size = 4) +      # Rótulos com a contagem em cima da barra
  labs(
    title = "Quantidade de Óbitos por Faixa Etária",                # Título do gráfico
    x = "Faixa Etária (Anos)",                                      # Título do eixo X
    y = "Quantidade de Óbitos"                                      # Título do eixo Y
  ) +
  theme_minimal(base_size = 15) +                                   # Tema minimalista e mais legível
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),          # Centralizar e destacar o título
    axis.text.x = element_text(angle = 45, hjust = 1)               # Rotacionar os rótulos no eixo X
  )


#Histograma da variavel tempo (tempo é a difference entre a data de entrada na internacao e a data de saida)
tempo <- as.numeric(tempo)
hist(tempo)

# Teve Morte no tempo=0? Questionar o prof se não é bom eliminar esses de tempo=0
#Dias de permanencia na UTI apenas 1.


#Filtrando o dataframe para tempo = 0 e com óbito
obitos_tempo_0 <- subset(tempo_100, tempo == 0 & MORTE == "Com óbito")

# Contando o número de óbitos no tempo 0
num_obitos_tempo_0 <- nrow(obitos_tempo_0)

# Exibindo o resultado
print(num_obitos_tempo_0)

#Maioria era urgencia

table(obitos_tempo_0$CAR_INT)
#---------------------------------------------------------------------------



#----------------------------------------------------------------------------------------
#ajuste de modelos

#Dividindo em treino e teste
proporcao_treino <- 0.7  # 70% para treino, 30% para teste

# Gerando índices aleatórios para divisão
set.seed(1308)  # Para garantir a reprodutibilidade
indices <- sample(seq_len(nrow(tempo_100)), size = floor(proporcao_treino * nrow(tempo_100)))

# Criando os conjuntos de treino e teste
treino <- tempo_100[indices, ]
teste <- tempo_100[-indices, ]

# Verificando o tamanho dos conjuntos
cat("Número de linhas no conjunto de treino:", nrow(treino), "\n")
cat("Número de linhas no conjunto de teste:", nrow(teste), "\n")


tempo <- tempo_100$tempo
censura <- tempo_100$censura
motivo <-  tempo_100$motivo
genero <- tempo_100$genero
tempo_100$tempo[tempo_100$tempo <= 0] <- 0.1  # Adiciona um pequeno valor positivo
# Convertendo a coluna DIAG_PRINC em fator
treino$CAR_INT <- as.factor(treino$CAR_INT)

df_urgencia <- treino %>%
  filter(CAR_INT == "Urgência" )

df_elet <- treino %>%
  filter(CAR_INT == "Eletivo" )

df_urgencia_elet <- rbind(df_urgencia,df_elet)

#MODELO DE REGRESSÃO EXPONENCIAL

modelo_exponencial <- survreg(Surv(tempo, censura) ~ IDADE + motivo + genero , data = tempo_100, dist = "exponential")
#modelo_exponencial1 <- survreg(Surv(tempo, censura) ~ motivo, data = treino, dist = "exponential")
# Resumo do modelo
summary(modelo_exponencial)
modelo_exponencial$loglik

#summary(modelo_exponencial1)
#modelo_exponencial1$loglik


#Avaliando o modelo

#modelo_exponencial$coefficients[1] = beta 0


xi<-modelo_exponencial$coefficients[1]+modelo_exponencial$coefficients[2]*IDADE+modelo_exponencial$coefficients[3]*tempo_100$motivo+modelo_exponencial$coefficients[4]*tempo_100$genero
res<- tempo*exp(-xi*(modelo_exponencial$coefficients[1]+modelo_exponencial$coefficients[2]+modelo_exponencial$coefficients[3]+modelo_exponencial$coefficients[4]))
# res ́ıduos padronizados # exponencial dos res ́ıduos padronizad

res<-(log(as.numeric(tempo))-(xi))/sigma 
ei<--log(1-pnorm(res)) # res ́ıduos de Cox-Snell
ekm1<-survfit(Surv(ei,censura)~1) 
t<-ekm1$time 
st<-ekm1$surv 
sexp<-exp(-t) 
par(mfrow=c(1,2)) 
plot(st,sexp,xlab="S(ei):Kaplan-Meier",ylab="S(ei):Exponencial padrao",pch=16)
plot(ekm1,conf.int=F,mark.time=F, xlab="Res ́ıduos de Cox-Snell", ylab="Sobrevivencia estimada")
lines(t,sexp,lty=4) > legend(1.0,0.8,lty=c(1,4),c("Kaplan-Meier","Exponencial padrao"),cex=0.8,bty="n")




 

#Modelo De Regressão Weibull
  
modelo_weibull<-survreg(Surv(tempo, censura)~ IDADE  + motivo + genero, dist="weibull", data= tempo_100) 
modelo_weibull 
modelo_weibull$loglik 
gama<-1/modelo_weibull$scale 
gama
summary(modelo_weibull)


#GRAFICOS
xi<-modelo_weibull$coefficients[1]+modelo_weibull$coefficients[2]*IDADE+modelo_weibull$coefficients[3]*tempo_100$motivo+modelo_weibull$coefficients[4]*tempo_100$genero
res<- tempo*exp(-xi*(modelo_weibull$coefficients[1]+modelo_weibull$coefficients[2]+modelo_weibull$coefficients[3]+modelo_weibull$coefficients[4]))
residuos <- (as.numeric(res))^gama
ekm2<-survfit(Surv(residuos,censura)~1) 
t<-ekm2$time 
st<-ekm2$surv 
sexp<-exp(-t) 
par(mfrow=c(1,2)) 
plot(st,sexp,xlab="S(ei):Kaplan-Meier",ylab="S(ei):Exponencial padrão",pch=16)
plot(ekm2,conf.int=F,mark.time=F, xlab="Residuos de Cox-Snell", ylab ="Sobrevivência estimada")
lines(t,sexp,lty=4) > legend(1.0,0.8,lty=c(1,4),c("Kaplan-Meier","Weibull"),cex=0.8,bty="n")

#Modelo de Regressão Log-Normal
modelo_lognormal<-survreg(Surv(tempo,censura)~IDADE  + motivo + genero , dist="lognorm",data = treino)  
modelo_lognormal  
summary(modelo_lognormal)

#GRAFICOS

xb<-modelo_lognormal$coefficients[1]+modelo_lognormal$coefficients[2]*IDADE+modelo_lognormal$coefficients[3]*treino$motivo+ modelo_lognormal$coefficients[4]*treino$genero
sigma<-modelo_lognormal$scale 
res<-(log(as.numeric(tempo))-(xb))/sigma # residuos padronizados
resid<-exp(res)  # exponencial dos res ́ıduos padronizado

ekm<-survfit(Surv(resid,censura)~1) 
resid<-ekm$time 
sln<-pnorm(-log(resid)) 
par(mfrow=c(1,2)) 
plot(ekm$surv,sln,xlab="S(ei*):Kaplan-Meier", ylab="S(ei*):Log-normal padrao", pch=16)
plot(ekm, conf.int=F,mark.time=F,xlab="Residuos (ei*)", ylab="Sobrevivencia estimada",pch=16)
lines(resid,sln,lty=2) > legend(1.3,0.8,lty=c(1,2),c("Kaplan-Meier","Log-normal padrao"),cex=0.8,bty="n")



# Fazendo o que o prof pediu

# Vetor de tempos para prever
tempo_pred <- seq(0, max(tempo), length.out = 100)

# Prever o tempo de sobrevivência com base no modelo Weibull
predicoes <- predict(modelo_weibull, type = "quantile", p = 0.5, newdata = treino)

# Extrair os parâmetros do modelo Weibull
escala <- 1 / modelo_weibull$scale
forma <- exp(modelo_weibull$linear.predictors)

# Calcular a função de sobrevivência Weibull
sobrevivencia_weibull <- exp(- (tempo_pred / forma) ^ escala)

sobrevivencia_weibull <- sobrevivencia_weibull[!is.na(sobrevivencia_weibull)]
tempo_pred <- tempo_pred[1:length(sobrevivencia_weibull)]



ekm <- survfit(Surv(tempo, censura) ~ 1, data = treino)
plot(ekm, lty = 1, xlab = "Tempo (dias)", ylab = "Função de Sobrevivência", main = "Comparação de Curvas de Sobrevivência")
lines(tempo_pred, sobrevivencia_weibull, col = "red", lty = 2)
legend("topright", legend = c("Kaplan-Meier", "Weibull"), col = c("black", "red"), lty = c(1, 2))


-----------------------------------------------------------------------------------------------------------
# Comparação de curvas de sobrevivencia sem covariveis weibull
motivo <- 0
genero <- 0
idade <- 0


t <- seq(0, max(tempo_numeric), by = 0.1)

alfa <- 1.95+idade*1.002940+motivo*0.451227+genero*1.151274
sobrevivencia_weibull <- exp(- (t / alfa) ^ escala)


ekm <- survfit(Surv(tempo, censura) ~ 1, data = tempo_100)


plot(ekm, lty = 1, xlab = "Tempo (dias)", ylab = "Função de Sobrevivência", main = "Comparação de Curvas de Sobrevivência")


lines(t, sobrevivencia_weibull, col = "red", lty = 2)


legend("topright", legend = c("Kaplan-Meier", "Weibull"), col = c("black", "red"), lty = c(1, 2))

summary(modelo_weibull)

## Comparação de curvas de sobrevivencia sem covariveis exponecial

t <- seq(0, max(tempo_numeric), by = 0.1)


sobrevivencia_exp <- exp(- (t / alfa))


ekm <- survfit(Surv(tempo, censura) ~ 1, data = tempo_100)


plot(ekm, lty = 1, xlab = "Tempo (dias)", ylab = "Função de Sobrevivência", main = "Comparação de Curvas de Sobrevivência")


lines(t, sobrevivencia_weibull, col = "red", lty = 2)


legend("topright", legend = c("Kaplan-Meier", "Exponencial"), col = c("black", "red"), lty = c(1, 2))
