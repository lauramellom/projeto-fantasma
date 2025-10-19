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
## Análise 2

#Carregando pacotes
library(tidyverse)
library(readxl)


#Importando planilhas do Exel
infos_clientes <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                             sheet = "infos_clientes")

#Transformando de dm para cm e de lbs para kg
infos_clientes$Height_cm <- infos_clientes$Height_dm * 10
infos_clientes$Weight_kg <- infos_clientes$Weight_lbs * 0.45359237

grafico2 <- ggplot(infos_clientes) +
  aes(x = Height_cm, y = Weight_kg) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "Altura(cm)",
    y = "Peso(kg)"
  ) +
  theme_estat()

grafico2
