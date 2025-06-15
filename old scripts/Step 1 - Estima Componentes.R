#' Autor: Bruno Tebaldi Barbosa
#'
#' Data: 2021-05-28
#'
#' Calcula os PCAs de cada grupo de exposição do ICF.
#'
#' Processo: (i) Remoção da tendência das séries dos grupos 1 até 7; (ii)
#' Impressao dos graficos das variaveis; (iii) Exclui os dados faltantes; (iv)
#' Calcula os pca´s de cada grupo; (v) agrupa os PCA´s dos grupos em uma unica
#' tabela.
#'
#'
#' Input: Data inicial e final a serem processadas
#'
#' Output: Arquivos rds (individuais e agrupados)

# Setup -------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tseries)

cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 1 - Analise de PCA's"))

rm(list=setdiff(ls(), c("date.ini", "date.fim")))

# Arquivo que vai guardar os pesos.
PCA_tbl.filepath <- file.path("database", "database_ICF", "PCA_tbl.rds")
PCA.rotation.fileMask <- file.path("database", "database_ICF", "PCA_groupo_%d.rds")

# User defined functions --------------------------------------------------
source("./scripts/ICF/lib/function pDiff.r")

# Define a Monthly table ----------------------------------------------------
cat(sprintf("Entre com a data inicial e data final para a analise dos pesos e decomposição dos PCAs\n"))
date.ini <- as.Date(readline(prompt="Enter initial Date: "))
date.fim <- as.Date(readline(prompt="Enter final Date: "))

if(is.na(date.ini)){
  date.ini <- as.Date("2004-01-01")
  cat(sprintf("Ajustando data.ini para %s", date.ini), "\n")
} else if(date.ini <= "2004-01-01"){
  date.ini <- as.Date("2004-01-01")
  cat(sprintf("Ajustando data.ini para %s", date.ini), "\n")
}

if(is.na(date.fim)){
  date.fim <- Sys.Date()
  cat(sprintf("Ajustando data.fim para %s", date.fim), "\n")
}

# Set day as day 1
day(date.ini) = 1
day(date.fim) = 1
cat(sprintf("\nData inicial: %s\nData final: %s", date.ini, date.fim))


# Tabela com os PCAs de cada grupo
tbl.pca <- tibble(Date=seq(from=date.ini, to=date.fim, by="month"))
tbl.pca <- tbl.pca %>% mutate(Ano=year(Date), Mes=month(Date))


# Construcao dos PCAs -----------------------------------------------------

# Mascara do caminho do banco de dados
db.file.mask <- "./database/database_ICF/%s_Monthly.rds"

db.idx <- 1
for (db.idx in seq_len(7)) {
  
  db.name <- paste("Grupo", db.idx, sep="")
  
  cat(sprintf("\nCarregando banco: %s\n", db.name))
  db.file.path <- sprintf(db.file.mask, db.name)
  tbl <- readRDS(db.file.path) %>% arrange(Ano, Mes)
  
  # Verifica estabilidade da serie
  for (col in colnames(tbl)) {
    if(!(col %in% c("Ano", "Mes"))){
      
      # busca a serie na tabela atual
      y <- tbl %>% arrange(Ano, Mes) %>% pull(col)
      
      if(any(is.na(y))){
        cat(sprintf("\nA serie %s do grupo %d contem NA's\n", 
                    col, db.idx))
        cat("Posições:", sprintf("%d", which(is.na(y))), "\n")
      }
      
      # Realiza o teste adf (simples)
      test <- adf.test(y)
      
      # Imprime o resultado do teste
      if(test$p.value >= 0.05){
        cat(sprintf("Serie %s é integrada (ADF pvalue: %0.3f)\n", col, test$p.value))
      } else {
        cat(sprintf("Serie %s é estacionaria (ADF pvalue: %0.3f)\n", col, test$p.value))
      }
    }
  }
  
  # Informo qual as tabelas que vao ser estabilizadas por primeira diferença
  if(db.name %in% paste("Grupo", c(1, 2, 3, 4, 5, 6, 7), sep="")){
    
    cat(sprintf("Retirando tendencia pela 1a Diff do %s\n", db.name))
    tbl <- tbl %>% 
      arrange(Ano, Mes) %>% 
      mutate_at(.vars = vars(-Ano, -Mes), .funs = p.Diff) %>%
      filter(row_number() > 1) #%>% mutate_at(.vars = vars(-Ano, -Mes), .funs = rollmean, k=12,  na.pad = TRUE, align = "right")
    
  }
  
  
  cat(sprintf("Gerando graficos das variaveis do %s padronizadas (mu=0; sd=1)\n", db.name))
  
  
  # Faz um grafico de cada serie antes da extração do PCA
  g <- tbl %>%
    mutate_at(.vars = vars(-Ano, -Mes), .funs = scale) %>% 
    pivot_longer(cols = -c("Ano", "Mes")) %>% 
    mutate(Date=ymd(Ano*100+Mes, truncated = 1)) %>% 
    ggplot() +
    geom_line(aes(x=Date, y=scale(value), colour=name)) +
    facet_wrap(~name) + 
    labs(title = db.name,
         subtitle = "Series padronizadas", 
         colour= NULL)
  
  print(g)
  
  ggsave(filename = sprintf("Series_%s.png", db.name),
         path = "./Export/ICF",
         plot = g,
         scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)
  
  
  cat(sprintf("Excluindo dados faltantes para calculo do PCA do %s", db.name))
  size.preExclusion <- nrow(tbl)
  tbl <- tbl %>% na.omit() 
  cat(sprintf("\t(Total de exclusões: %d)\n", size.preExclusion-nrow(tbl)))
  
  
  cat(sprintf("Calculando PCAs do %s\n", db.name))
  pca <- tbl %>% 
    dplyr::select(-Ano, -Mes) %>%
    prcomp(retx = TRUE,
           center = TRUE, scale. = TRUE,
           rank=1)
  
  # tbl$PCA <- pca$x*get_eig(pca)[1,1]
  tbl$PCA <- pca$x
  
  cat(sprintf("Salvando Rotacao do %s\n", db.name))
  saveRDS(pca, file = sprintf(PCA.rotation.fileMask, db.idx))
  
  #  Adiciona o resultado na tabela final
  tbl.pca <- tbl %>% 
    dplyr::select(Ano, Mes, PCA) %>% 
    right_join(tbl.pca, by=c("Ano"="Ano", "Mes"="Mes"))
  
  #  Copia o resultado do PCA para a coluna do grupo
  tbl.pca[,sprintf("PCA_%s", db.name)] <- tbl.pca$PCA
  
  # Apaga a coluna PCA (just in case)
  tbl.pca$PCA <- NULL
}

# Rearanjo os dados da tabela
tbl.pca <- tbl.pca %>%  arrange(Ano, Mes)

saveRDS(tbl.pca, file = PCA_tbl.filepath)