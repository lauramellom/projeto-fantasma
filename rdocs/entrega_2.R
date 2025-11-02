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

infos_clientes <- infos_clientes %>%
  mutate(faixa_altura = cut(
    Height_cm,
    breaks = seq(150, 200, by = 10),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("150–159", "160–169", "170–179", "180–189", "190–199")
  )) 
print_quadro_resumo <- function(data, var_name, group_name,
                                title = "Medidas resumo do peso (kg) por faixas de altura (cm)",
                                label = "quad:peso_altura") {
  
  var_name <- substitute(Weight_kg)
  group_name <- substitute(faixa_altura)
  
  resumo <- data %>%
    group_by(!!group_name) %>%
    summarize(
      `Média` = round(mean(!!sym(var_name)), 2),
      `Desvio Padrão` = round(sd(!!sym(var_name)), 2),
      `Mínimo` = round(min(!!sym(var_name)), 2),
      `1º Quartil` = round(quantile(!!sym(var_name), 0.25), 2),
      `Mediana` = round(quantile(!!sym(var_name), 0.5), 2),
      `3º Quartil` = round(quantile(!!sym(var_name), 0.75), 2),
      `Máximo` = round(max(!!sym(var_name)), 2)
    ) %>%
    as.data.frame()
  
  latex <- str_c(
    "\\begin{quadro}[H]
\\caption{", title, "}
\\centering
\\begin{adjustbox}{max width=\\textwidth}
\\begin{tabular}{|l|", 
    str_dup("S[table-format=3.2]|", ncol(resumo) - 1), "}
\\toprule
\\textbf{", names(resumo)[1], "}"
  )
  
  for (col in names(resumo)[-1]) {
    latex <- str_c(latex, " & \\textbf{", col, "}")
  }
  latex <- str_c(latex, " \\\\ \n\\midrule\n")
  
  for (i in seq_len(nrow(resumo))) {
    linha <- str_c(resumo[i, 1], " & ", 
                   str_flatten(resumo[i, -1], collapse = " & "),
                   " \\\\")
    latex <- str_c(latex, linha, "\n")
  }
  
  latex <- str_c(latex,
                 "\\bottomrule
\\end{tabular}
\\label{", label, "}
\\end{adjustbox}
\\end{quadro}")
  
  writeLines(latex)
}
print_quadro_resumo(infos_clientes, Weight_kg, faixa_altura)
