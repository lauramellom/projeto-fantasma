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

#Importando planilhas do Exel
library(readxl)
infos_cidades <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                            sheet = "infos_cidades")
infos_clientes <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                             sheet = "infos_clientes")
infos_lojas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                          sheet = "infos_lojas")
relatorio_vendas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                               sheet = "relatorio_vendas")

#Padronizar nomes de colunas
names(infos_lojas)[names(infos_lojas) == "Stor3ID"] <- "StoreID"
names(infos_clientes)[names(infos_clientes) == "Cli3ntID"] <- "ClientID"

#Adicionar coluna com a cidade em que cada venda foi realizada
relatorio_vendas$CityID <- NULL
relatorio_vendas <- merge(relatorio_vendas, infos_lojas[ , c("StoreID", "CityID")],
                          by = "StoreID",
                          all.x = TRUE)

#Criar outro data.frame só com a cidade desejada
AmbarSeco <- relatorio_vendas[relatorio_vendas$CityID == 2, ]

#Adicionar coluna com a idade correspondente a cada cliente
AmbarSeco$Age <- NULL
AmbarSeco <- merge(AmbarSeco, infos_clientes[ , c("ClientID", "Age")],
                   by = "ClientID",
                   all.x = TRUE)

#Adicionar coluna com o nome da loja da venda
AmbarSeco$NameStore <- NULL
AmbarSeco <- merge(AmbarSeco, infos_lojas[ , c("StoreID", "NameStore")],
                   by = "StoreID",
                   all.x = TRUE)

#Eliminar clientes que tem informações duplicadas
ambar_seco <- AmbarSeco[!duplicated(AmbarSeco$ClientID), ]

#Gráfico 
grafico3 <- ggplot(ambar_seco) +
  aes(x = Age) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  facet_grid(. ~ NameStore) +
  labs(x = "Idade dos Clientes", y = "Frequência") +
  theme_estat(
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white")
  )
grafico3
