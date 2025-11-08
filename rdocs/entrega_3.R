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
library(tidyverse)
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



print_quadro_resumo <- function(data, var_name, title="Medidas resumo
da idade dos clientes por loja", label="quad:quadro_idade_loja")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1o Quartil` = round(quantile(!!sym(var_name), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5)
                                ,2),
              `3o Quartil` = round(quantile(!!sym(var_name), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count)) {
    latex <- str_c(latex, "\t\t\tS[table-format = 3.2]\n", sep="")
  }
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule


\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
ambar_seco %>%
  group_by(NameStore) %>%
  print_quadro_resumo(var_name = Age)


##Análise 4
relatorio_vendas$Date <- as.Date(relatorio_vendas$Date)
relatorio_vendas$Ano <- format(relatorio_vendas$Date, "%Y")

relatorio_vendas$ItemID <-info_vendas$ItemID

names(infos_produtos)[names(infos_produtos) == "Ite3ID"] <- "ItemID"

relatorio_vendas$UnityPrice <- NULL
relatorio_vendas <- merge(relatorio_vendas, infos_produtos[ , c("ItemID", "UnityPrice")],
                          by = "ItemID",
                          all.x = TRUE)

relatorio_vendas$valor_venda <- relatorio_vendas$Quantity * relatorio_vendas$UnityPrice

relatorio1889 <- relatorio_vendas[relatorio_vendas$Ano == 1889, ]

receita_lojas <-
     group_by(relatorio1889,StoreID)%>%
     summarise(Total=sum(valor_venda))

ordem_receitas <- receita_lojas%>%arrange(desc(Total))

top3lojas <- ordem_receitas[1:3,]

produtos_vendidos <- 
  group_by(relatorio1889, StoreID, ItemID)%>%
  summarise(quantidade=sum(Quantity))

produtos_filtrados <- filter(produtos_vendidos, StoreID %in% c(7, 5, 17))

                        

trans_drv <- mpg %>%
  mutate(trans = case_when(
  )) %>%
  group_by(trans, drv) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")")
)
ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, freq, .desc = T), y = freq,
    fill = drv, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")