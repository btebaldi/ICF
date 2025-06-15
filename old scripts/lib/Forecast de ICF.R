
# Setup -------------------------------------------------------------------
rm(list=ls())

library(dplyr)

cat(sprintf("Entre com a data inicial e data final para a analise dos pesos e decomposição dos PCAs\n"))
if(!exists("date.ini")) {date.ini <- as.Date(readline(prompt="Enter initial Date: "))}
if(!exists("date.fim")) {date.fim <- as.Date(readline(prompt="Enter final Date: "))}

# Set day as day 1
day(date.ini) = 1
day(date.fim) = 1

# User defined functions --------------------------------------------------

source("./lib/function pDiff.r")

db.file.mask <- "./database/%s_Monthly.rds"
PCA.rotation.fileMask <- file.path("database", "PCA_%d_rotation.rds")

tbl.pca <- tibble(Date=seq(from=date.ini, to=date.fim, by="month"))
tbl.pca <- tbl.pca %>% mutate(Ano=year(Date), Mes=month(Date))

for (db.idx in seq_len(7)) {
  
  db.name <- paste("Grupo", db.idx, sep="")
  
  cat(sprintf("\nCarregando banco: %s\n", db.name))
  db.file.path <- sprintf(db.file.mask, db.name)
  tbl <- readRDS(db.file.path) %>% arrange(Ano, Mes)
  
# Informo qual as tabelas que vao ser estabilizadas por primeira diferença
  if(db.name %in% paste("Grupo", c(1, 2, 3, 4, 5, 6, 7), sep="")){
    
    cat(sprintf("Retirando tendencia pela 1a Diff do %s\n", db.name))
    tbl <- tbl %>% 
      arrange(Ano, Mes) %>% 
      mutate_at(.vars = vars(-Ano, -Mes), .funs = p.Diff) %>%
      filter(row_number() > 1)
  }
  
  cat(sprintf("Padronizando variaveis do %s (mu=0; sd=1)\n", db.name))
  for (col in colnames(tbl)) {
    if(col %in% c("Ano", "Mes")) {
      next()
    }
    
    tbl[col] <- scale(tbl[col], center = TRUE, scale = TRUE)
  }
  
  data <- tbl %>% dplyr::select(-Ano, -Mes) %>% data.matrix()
  
  rotation <- readRDS(sprintf(PCA.rotation.fileMask, db.idx))
  
  tbl[, sprintf("PCA_%s", db.name)] <- data %*% rotation
  
  #  Copia o resultado do PCA para a coluna do grupo
  tbl.pca <- tbl.pca %>% left_join(tbl, by =c("Ano"="Ano", "Mes"="Mes")) %>%
    dplyr::select(Ano, Mes, starts_with("PCA"))

}


tbl.pca


wights.filepath <- file.path("database", "ICF_Weights.rds")
weights <- readRDS(wights.filepath)

tbl.pca[, psate("PCA_Grupo", 1:7, sep = "")] %*%

