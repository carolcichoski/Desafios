### nome <- "Caroline Cichoski"
### programa <- "Doutorado em Ciência Ambiental"
### n_usp <- 6996718
### data_entrega: "29/07/2019"

# INSTRUÇÕES:
#Para o desafio você precisará estar confortável com os seguintes tópicos: 
#aberturas de bases de dados; 
#manipulação de bases de dados com dplyr; 
#bases de dados relacionais com dplyr; 
#e, finalmente, visualização de dados com ggplot2 (caps 3, 7, 28). 

# Parte 0 - Carregar bibliotecas
library(tidyverse)
library(dplyr)
library(data.table)
library(readxl)   # To read .xls files with read_excel function
library(gdata)    # To read .xls files with read.xls function
install.packages("gdata")
library(fread)
install.packages("data.table")
getwd()
library(zoo)

#Parte 1 - Combinando fonte da dados diferentes
#1.1 carregando dados Base_MUNIC_2015.csv
library(readr)
Base_MUNIC_2015 <- read_delim("Base_MUNIC_2015.csv", 
                              ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                              trim_ws = TRUE)
View(Base_MUNIC_2015)
view(base_perfilmun)
view(base_perfilmun_sp)
view(base_fiscalmunsp)

#Renomeando colunas

base_perfilmun <- Base_MUNIC_2015 %>%
  rename(cod_ibge7 = A1, cod_uf = Codigouf, municipio = Nome)
base_perfilmun <- base_perfilmun %>%
  mutate(cod_ibge6 = as.character(substr(cod_ibge7, 1, 6))) 

#selecionando UF DE SP
base_perfilmun_sp <- base_perfilmun %>%
  filter(cod_uf == 35) %>%
   select(cod_ibge6, municipio, A155, A156, A157, A158)  %>%
  rename(consorciosaude = A155, intermunicipal = A156, estado = A157, uniao = A158)


### 1.2 carregando Base Fiscal dos Municípios - FINBRA

finbra <- read_delim("finbra.csv", ";", escape_double = FALSE, 
                     locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
View(finbra)
names(base_fiscalmun)

names(finbra)
base_fiscalmun <- finbra %>%
  rename(cod_ibge7 = 'Cod.IBGE', municipio = 'Instituição', uf = 'UF', 
         populacao = 'População', coluna = 'Coluna', conta = 'Conta', valor = 'Valor')%>%
  mutate(cod_ibge6 = as.numeric(substr(cod_ibge7, 1, 6))) %>%
  select(-cod_ibge7)  %>%
  #selecionando UF DE SP
   base_fiscalmunsp <- base_fiscalmun %>%
  filter(uf == "SP" ) %>%
  
  
   #separate("conta", into = c("item", "area"), sep = " - ")  %>%
 

base_fiscalmun <- base_fiscalmun %>%
  spread(key = area, value = valor) 

base_fiscalmun <- base_fiscalmun %>%
  do(na.locf(.))

names(base_fiscalmun)  


names(base_fiscalmun)  
base_fiscalmun <- base_fiscalmun %>%


head(base_fiscalmun)


### 1.1.3 Base de Saúde dos Municípios


### checar o 3 dado



## 1.2 Juntar bases de dados

baseunica <- inner_join(base_perfilmun_sp, base_fiscalmunsp, by = "cod_ibge6")  %>%
  inner_join(base_datasusmun, by = "cod_ibge6")

baseunica <- baseunica %>%
  mutate(menor1ano = gsub("-", "0", menor1ano)) %>%
  select(-municipio.y) %>%
  rename(municipio = municipio.x)

# Parte 2 - Análise exploratória e gráfica dos dados

library(readr)
library(ggplot2)




