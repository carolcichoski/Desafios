### nome <- "Ana Lucia Gerardi Spinola"
### programa <- "Doutorado em Ciência Ambiental"
### n_usp <- "5982030"
### data_entrega: "01/06/2018"

list.files()
## Carregando pacotes necessários
library(data.table)
library(dplyr)
library(readr)

##Carregando dataframes com fread
obitos_mun <- fread("DataSUS.csv", sep = ";", nrows = 5500, header = T, skip = 3)


##Fazendo download e unzip

download.file("ftp://ftp.ibge.gov.br/Perfil_Municipios/2015/Base_de_Dados/Base_MUNIC_2015_xls.zip", "Base_MUNIC_2015_xls.zip", quiet = F)

unzip("Base_MUNIC_2015_xls.zip")
list.files()

## Carregando com fread (limitei 5500 municipios para não ficar muito grande)
terc_mun <- fread("Base_MUNIC_2015.csv", sep = ";", nrows = 5500, header = T)
receita_mun <- fread("finbra.csv", sep = ";", nrows = 5500, header = T, skip = 3)

## No caso da receita_mun terei que agrupar por código de UF antes de selecionar linhas
receita_mun <- fread("finbra.csv", sep = ";", header = T, skip = 3)

head(receita_mun)
str(receita_mun)
receita_mun <- receita_mun %>%
  select("Cod.IBGE", "UF", "Conta", "Valor")
head(receita_mun) 

receita_mun <- receita_mun %>%
  group_by(UF)%>%
  arrange(Cod.IBGE)

receita_mun2 <- receita_mun %>%
  filter(UF=="RO")
receita_mun2 <- receita_mun2 %>%
  filter(Conta=="Total Receitas"|Conta=="1.6.0.0.41.00.00 - Serviços de Captação, Adução, Tratamento, Reserva e Distribuição de Água"|Conta=="1.6.0.0.43.00.00 - Serviços de Coleta, Transporte, Tratamento e Destino Final de Resíduos Sólidos")
##não rolou

##Juntando os bancos do DataSUS Perfil
names(terc_mun)

terc_mun <- terc_mun %>%
  rename(codMun=`A1`)
names(obitos_mun)
obitos_mun <- obitos_mun %>%
  rename(codMun = `Município`)
 
names(receita_mun)
receita_mun <- receita_mun %>%
  rename(codMun = `Cod.IBGE`)


obitos_mun <- obitos_mun %>%
  mutate(codMun = as.numeric(codMun))
receita_mun <- receita_mun %>%
  mutate(codMun = as.numeric(codMun))
terc_mun <- terc_mun %>%
  mutate(codMun = as.numeric(codMun))

comb_terc_receita <- inner_join(terc_mun,receita_mun, by = "codMun")

obitos_mun <- fread("DataSUS.csv", sep = ";", nrows = 5500, header = T, skip = 3)
names(obitos_mun)

obitos_mun$codMun <- as.numeric(gsub("[[:alpha:]]","",obitos_mun$Município))

obitos_mun2 <- obitos_mun %>%
  rename(obitosRes = `Óbitos_p/Residênc`) %>%
  select(codMun, obitosRes) %>%
  na.omit()

View(obitos_mun2)

comb_terc_receita2 <- comb_terc_receita %>%
  mutate(codMun = as.numeric(codMun)) %>%
  as.numeric(substr("codMun", 1, 6))

glimpse(comb_terc_receita)
glimpse(obitos_mun2)

comb_terc_receita2 <- comb_terc_receita %>%
  mutate(codMun2 = as.numeric(codMun))

comb_terc_receita2 <- comb_terc_receita %>%
  mutate(codMun = as.numeric(substr(codMun,1,6)))

glimpse(comb_terc_receita2)

comb_terc_receita2 <- comb_terc_receita2 %>%
  select(codMun, Nome, Conta, Valor, A126, A127, A128, A129, A130) %>%
  rename(resDom = A126, resSau = A127, resInd = A128, 
         LimpAd = A129, LimpUrb = A130)

View(comb_terc_receita2)

## Combinando tudo de novo
names(comb_terc_receita2)
names(obitos_mun2)

combTudo <- inner_join(comb_terc_receita2, obitos_mun2, by = "codMun")
View(combTudo)

#aeeeeee uhuuuuu!!

combTudo2 <- combTudo %>%
  mutate(Valor2 = as.numeric(substr(Valor,1,9))) %>%
  na.omit()
View(combTudo2)

glimpse(combTudo2)

combTudo2 <- combTudo2 %>%
  select(codMun, Nome, Conta, Valor2, resDom, resSau, resInd, LimpAd, LimpUrb, obitosRes)

combTudo3 <- combTudo2 %>%
  filter(Conta == "Total Receitas")

View(combTudo3)
glimpse(combTudo3)







somaValor2 <- sum(combTudo3$Valor2)
combTudo4 <- combTudo3 %>%
  mutate(PercValor2 = (Valor2/somaValor2)*100)

#PS: eu sei que não faz muito sentido essa nova variável, 
#já que escolhi aleatoriamente os municípios e tem que levar em conta população etc, 
#mas foi o que deu para fazer com esses dados do capiroto de finanças

##Parte2 - Análise Exploratória dos Dados

library(ggplot2)

combTudo5 <- top_n(combTudo4, 20, wt = obitosRes) %>%
  filter(Valor2>=1)

ggplot(data = combTudo5)  +
  geom_point(aes(x = PercValor2, y = obitosRes, colour = Nome))


ggplot(data = combTudo5)  +
  geom_point(aes(x = obitosRes, y = Nome, colour = resDom))

combTudo5%>%
  count(resDom)

ggplot(data = combTudo5) +
  geom_bar(aes(x = resDom, fill = resDom), width = 0.5, na.rm = T)


           