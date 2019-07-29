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

# Parte 1 - Combinando fontes de dados diferentes

cod_ibge6 <- as.numeric(substr(cod_ibge7, 1, 6)) #dica

## 1.1 Carregando bases de dados
## Obs: origem: repositório do curso

### 1.1.1 Base Perfil dos Municípios
### Obs: formato .csv
url_perfilmun <- "https://raw.githubusercontent.com/leobarone/FLS6397/master/data/Base_MUNIC_2015_xls.zip"
download.file(url_perfilmun, "perfilmun.zip", quiet = F)
unzip("perfilmun.zip")
list.files()
file.remove("perfilmun.zip")

View(path_baseperfil)

#carregando dados base municipios

Base_MUNIC_2015 <- read_delim("Base_MUNIC_2015.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(Base_MUNIC_2015)

path_baseperfil <- read_delim("Base_MUNIC_2015.csv", ";", escape_double = FALSE, trim_ws = TRUE)
base_perfilmun <-fread(path_baseperfil) 

names(path_baseperfil)
base_perfilmun <- path_baseperfil %>%
  rename(cod_ibge7 = A1, cod_uf = Codigouf, municipio = Nome)  

base_perfilmun <- base_perfilmun %>%
  mutate(cod_ibge6 = as.character(substr(cod_ibge7, 1, 6))) 


view(base_perfilmun)

base_perfilmun_sp <- base_perfilmun %>%
  filter(cod_uf == 35) %>%
  select(cod_ibge6, municipio, A155, A156, A157, A158)  %>%
  rename(consorciosaude = A155, intermunicipal = A156, estado = A157, uniao = A158)

head(base_perfilmun_sp)

### 1.1.2 Base Fiscal dos Municípios


zipfile <- "finbra_MUNEST_DespesasporFuncao(AnexoI-E).zip"
unzip(zipfile)
file.remove(zipfile)


finbra <- read_delim("finbra.csv", ";", escape_double = FALSE, 
                     locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)

path_fiscal <- "finbra.csv"
base_fiscalmun <- fread(path_fiscal, quote = "")
names(base_fiscalmun)

base_fiscalmun <- finbra %>%
  rename(cod_ibge7 = 'Cod.IBGE', municipio = 'Instituição', uf = 'UF', 
         populacao = 'População', coluna = 'Coluna', conta = 'Conta', valor = 'Valor') %>%
  mutate(cod_ibge6 = as.numeric(substr(cod_ibge7, 1, 6))) %>%
  select(-cod_ibge7)  %>%
  separate("conta", into = c("item", "area"), sep = " - ")  %>%
  filter(coluna == 'Despesas Empenhadas', item == '10' | item == '10.301' | item == '10.302' | item == '10.304')

base_fiscalmun <- base_fiscalmun %>%
  spread(key = area, value = valor) 

library(zoo)  ## Função do(na.locf) -> para espalhar valores de uma variável por linhas vazias ## Fonte: https://stackoverflow.com/questions/27207162/fill-in-na-based-on-the-last-non-na-value-for-each-group-in-r
base_fiscalmun <- base_fiscalmun %>%
  do(na.locf(.))

names(base_fiscalmun)  
base_fiscalmun <- base_fiscalmun %>%
  rename(saude = 'Saúde', assistencia = 'Assistência Hospitalar e Ambulatorial', atencao = 'Atenção Básica', vigilancia = 'Vigilância Sanitária') %>%
  group_by(municipio, cod_ibge6) %>%   #para reduzir o número de observações (linhas) por município, preservando os valores máximos das variáveis
  summarise(contagem = n(), 
            populacao = max(populacao),
            saude = max(saude),
            assistencia = max(assistencia),
            atencao = max(atencao),
            vigilancia = max(vigilancia))

head(base_fiscalmun)

### 1.1.3 Base de Saúde dos Municípios
path_datasus <- "obitos_datasus.csv"  
base_datasusmun <- fread(path_datasus, quote = "")

base_datasusmun <- base_datasusmun %>%
  rename(cod_ibge6 = Município, menor1ano = `Menor 1 ano`) %>%
  separate("cod_ibge6", into = c("cod_ibge6", "mun")) %>%  #http://r4ds.had.co.nz/tidy-data.html#separating-and-uniting
  select(cod_ibge6, menor1ano)  %>%
  arrange(cod_ibge6)  %>%
  filter(cod_ibge6 >= 350010 & cod_ibge6 <= 355710) 

head(base_datasusmun)

## 1.2 Juntar bases de dados

baseunica <- inner_join(base_perfilmun_sp, base_fiscalmun, by = "cod_ibge6")  %>%
  inner_join(base_datasusmun, by = "cod_ibge6")

baseunica <- baseunica %>%
  mutate(menor1ano = gsub("-", "0", menor1ano)) %>%
  select(-municipio.y) %>%
  rename(municipio = municipio.x)

# Parte 2 - Análise exploratória e gráfica dos dados

library(readr)
library(ggplot2)

# Hip. : há uma relação positiva entre gastos com assistência primária e indicadores de mortalidade infantil
# Hip. : há uma relação positiva entre a participação do município em qualquer arranjo interinstitucional na área de saúde e os indicadores de mortalidade infantil
# Hip. 1: municípios que participam de consórcios interinstitucionais apresentam melhores indicadores de mortalidade infantil
# Hip. 2: municípios com maior investimento em assistência primária apresentam melhores indicadores de mortalidade infantil
# Hip. 3

## 2.1 Gráficos

library(ggthemes)

### 2.1.1 Gráfico para variável discreta

ggplot(baseunica) +
  geom_bar(aes(x=consorciosaude), colour = "white", fill = "dark blue", alpha=0.4) +
  ggtitle("Participação dos municípios paulistas em consórcios de saúde") +
  xlab("Se participa") +
  ylab("Número de participantes") +
  theme_economist()

# É maior o número de municípios paulsitas que não participam de consórcio na área de saúde em relação àqueles que participam 

### 2.1.2 Gráfico para variável contínua

baseunica <- baseunica %>%
  mutate(saude=as.numeric(gsub(",",".",saude)), assistencia=as.numeric(gsub(",",".",assistencia)),
         atencao=as.numeric(gsub(",",".",atencao)), vigilancia=as.numeric(gsub(",",".",vigilancia)),
         perc_assist=(assistencia/saude),
         format(perc_assist, nsmall = 2))

ggplot(baseunica) +
  geom_density(aes(x=perc_assist), colour = "white", fill = "red", alpha=0.2) +
  ggtitle("Distribuição do percentual de despesa em assistência primária em relação às despesas de saúde entre municípios paulistas") +
  xlab("Percentual despendido (em %)") +
  ylab("Densidade") +
  theme_economist()

# A proporção de despesa com assistência primária é muito pequena em relação ao total gasto com a área de saúde em municípios paulistas

### 2.1.3 Gráfico para duas variáveis, discreta e contínua

baseunica <- baseunica %>%
  mutate(menor1ano=as.numeric(menor1ano))

ggplot(baseunica) +
  geom_histogram(aes(x=menor1ano, fill="consorciosaude", color="consorciosaude"), bins=10, colour = "white", fill = "green", alpha=0.2, position = "dodge") +
  ggtitle("Número de óbitos infantis em relação à partipação em consórcio por município paulista") +
  xlab("Número de óbitos") +
  ylab("Densidade") +
  theme_economist()

# 

### 2.1.4 Gráfico para duas variáveis contínuas

ggplot(baseunica) +
  geom_jitter(aes(x=perc_assist, y=menor1ano)) +
  geom_smooth(aes(x=perc_assist, y=menor1ano), method = "lm") +
  ggtitle("Correlação entre despesas com assistência primária e número de óbitos infantis em municípios paulistas") +
  xlab("Percentual de despesas com assistência primária (em %)") +
  ylab("Número de óbitos infantis") +
  theme_economist()

# Há uma correlação negativa (fraca) entre o percentual de despesas com assistência primária e o número de óbitos infantis em municípios paulistas.
# Ou seja, quanto maior o percentual gasto com este tipo de despesa de saúde, menor o número de crianças que morrem até completarem 1 ano de vida. 

