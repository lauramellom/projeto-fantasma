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
infos_produtos <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                             sheet = "infos_produtos")
infos_vendas <- read_excel("C:/Users/laura/Downloads/relatorio_old_town_road.xlsx", 
                           sheet = "infos_vendas")

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


##Análise 4

relatorio_vendas$Date <- as.Date(relatorio_vendas$Date)
relatorio_vendas$Ano <- format(relatorio_vendas$Date, "%Y")

names(infos_produtos)[names(infos_produtos) == "Ite3ID"] <- "ItemID"

relatorio_vendas$ItemID <-infos_vendas$ItemID

relatorio_vendas$UnityPrice <- NULL
relatorio_vendas <- merge(relatorio_vendas, infos_produtos[ , c("ItemID", "UnityPrice")],
                          by = "ItemID",
                          all.x = TRUE)

relatorio_1889 <- subset(relatorio_vendas, Ano == 1889)

relatorio_1889$valor_venda <- round(
  relatorio_1889$Quantity * relatorio_1889$UnityPrice, 2)

receita_loja_1889 <- aggregate(valor_venda ~ StoreID,
                               relatorio_1889,
                               sum)

receita_loja_1889 <- receita_loja_1889[order(-receita_loja_1889$valor_venda), ]
top_lojas <- head(receita_loja_1889$StoreID, 3)

vendas_top_lojas <- relatorio_1889[relatorio_1889$StoreID %in% top_lojas, ]

produtos_por_loja <- aggregate(Quantity ~ StoreID + ItemID,
                               data = vendas_top_lojas,
                               sum)
names(produtos_por_loja)[3] <- "quantidade_vendida"


produtos_por_loja <- produtos_por_loja[order(produtos_por_loja$StoreID, 
                                             -produtos_por_loja$quantidade_vendida), ]

top3_produtos_por_loja <- produtos_por_loja[ave(produtos_por_loja$StoreID, 
                                                produtos_por_loja$StoreID, FUN = seq_along) <= 3, ]

top3_produtos_por_loja$NameStore <- NULL
top3_produtos_por_loja<- merge(top3_produtos_por_loja, infos_lojas[ , c("StoreID", "NameStore")],
                          by = "StoreID", all.x = TRUE)

top3_produtos_por_loja$NameProduct <- NULL
top3_produtos_por_loja<- merge(top3_produtos_por_loja, infos_produtos[ , c("ItemID", "NameProduct")],
                               by = "ItemID", all.x = TRUE)

receita_loja_1889<- receita_loja_1889[order(-receita_loja_1889$valor_venda), ]
top_lojas_ordenadas <- receita_loja_1889$StoreID

top3_produtos_por_loja <- top3_produtos_por_loja[order(match(top3_produtos_por_loja$StoreID, top_lojas_ordenadas),
                                                       -top3_produtos_por_loja$quantidade_vendida), ]
                             