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

print_quadro_resumo <- function(data, var_name, title="Medidas resumo
do peso(kg)", label="quad:quadro_peso")
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
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      

    
    max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
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

33

\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
print_quadro_resumo(infos_clientes, Weight_kg)

print_quadro_resumo <- function(data, var_name, title="Medidas resumo
da altura(cm)", label="quad:quadro_altura")
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
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      
      
      
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
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

33

\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}
print_quadro_resumo(infos_clientes, Height_cm)