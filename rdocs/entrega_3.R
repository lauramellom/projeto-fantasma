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
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white"
  ) +
  labs(
    x = "Loja",
    y = "Idade dos Clientes"
  ) +
  theme_estat()

ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
grafico3


print_quadro_resumo <- function(data, var_name, group_name,
                                title = "Medidas resumo da idade dos clientes por loja",
                                label = "quad:idade_lojas") {
  
  var_name <- substitute(Age)
  group_name <- substitute(NameStore)
  
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
print_quadro_resumo(ambar_seco, Age, NameStore)