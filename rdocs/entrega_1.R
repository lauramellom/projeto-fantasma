source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

##Análise 1


#Carregando pacotes
library(tidyverse)
library(readxl)


#Importando planilhas do Exel
relatorio_vendas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")
info_vendas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                           sheet = "infos_vendas")
infos_produtos <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                             sheet = "infos_produtos")
infos_lojas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                          sheet = "infos_lojas")

#Adicionar coluna ItemID em relatório_vendas
relatorio_vendas$ItemID <-info_vendas$ItemID


#Padronizar nomes de colunas
names(infos_produtos)[names(infos_produtos) == "Ite3ID"] <- "ItemID"
names(infos_lojas)[names(infos_lojas) == "Stor3ID"] <- "StoreID"


#Adicionar coluna com o valor dos produtos vendidos em cada venda
relatorio_vendas$UnityPrice <- NULL
relatorio_vendas <- merge(relatorio_vendas, infos_produtos[ , c("ItemID", "UnityPrice")],
                          by = "ItemID",
                          all.x = TRUE)

#Acionar coluna com os nomes das lojas de cada venda
relatorio_vendas$NameStore <- NULL
relatorio_vendas <- merge(relatorio_vendas, infos_lojas[ , c("StoreID", "NameStore")],
                          by = "StoreID",
                          all.x = TRUE)


#Converter o valor de Dollar para Real
relatorio_vendas$UnityPrice_real <- relatorio_vendas$UnityPrice * 5.31


#Calcular o valor total de cada venda em real
relatorio_vendas$valor_venda_real <- round(
  relatorio_vendas$Quantity * relatorio_vendas$UnityPrice_real, 2)


#Separar datas só pelo ano
relatorio_vendas$Date <- as.Date(relatorio_vendas$Date)
relatorio_vendas$Ano <- format(relatorio_vendas$Date, "%Y")


#Somar a receita de cada loja por ano
receita_loja_ano <- aggregate(valor_venda_real ~ NameStore + Ano,
                              relatorio_vendas,
                              sum)


#Somar a receita de todas as lojas por ano
faturamento_total_ano <- aggregate(valor_venda_real ~ Ano,
                                   receita_loja_ano,
                                   sum)


#Calcular a média da receita total das lojas pela quantidade de loja
faturamento_total_ano$ReceitaMedia <- faturamento_total_ano$valor_venda_real / 18


#Gráfico da receita média das lojas por ano
grafico1 <- ggplot(faturamento_total_ano) +
  aes(x = Ano, y = ReceitaMedia, group=1) +
  geom_line(size=1, colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita Média Lojas(R$)") +
  theme_estat()

grafico1

