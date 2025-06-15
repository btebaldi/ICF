# Main script - (i) remoção da tendência das séries dos grupos 4 até 7;
# 
# Processos:
#
# Autor: Bruno Tebaldi Barbosa
# Data: 2021-05-28
# 
# Input: Data inicial e final a serem processadas
# 
# Output: Arquivos rds (individuais e agrupados)

if(!exists("date.ini")) {date.ini <- as.Date(readline(prompt="Enter initial Date: "))}
if(!exists("date.fim")) {date.fim <- as.Date(readline(prompt="Enter final Date: "))}

# Setup -------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(seasonal)
library(factoextra)
library(zoo)
library(tseries)


rm(list=setdiff(ls(), c("date.ini", "date.fim")))

# User defined functions --------------------------------------------------

p.Diff <- function(x){
  ret <- x - lag(x)
  return(ret)
}

# Define a Monthly table ----------------------------------------------------

# Tabela com os PCAs de cada grupo
tbl.pca <- tibble(Date=seq(from=date.ini, to=date.fim, by="month"))

tbl.pca <- tbl.pca %>% mutate(Ano=year(Date), Mes=month(Date))


# Dataload e Processo logico ----------------------------------------------

# Mascara do caminho do banco de dados
db.file.mask <- "./database/%s_Monthly.rds"

db.list <- paste("Grupo", 1:7, sep="")
for (db.name in db.list) {
  
  cat(sprintf("\nCarregando banco: %s\n", db.name))
  db.file.path <- sprintf(db.file.mask, db.name)
  tbl <- readRDS(db.file.path)
  
  for (col in colnames(tbl)) {
    if(!(col %in% c("Ano", "Mes"))){
      y <- tbl %>% arrange(Ano, Mes) %>% pull(col)
      test <- adf.test(y)
      cat(sprintf("%0.3f ", test$p.value))
      
      if(test$p.value >= 0.05){
        cat(sprintf("serie %s é integrada\n", col))
      } else {
        cat(sprintf("serie %s é estacionaria\n", col))
      }
    }
  }
  
  
  if(db.name %in% paste("Grupo", c(4,5,6,7), sep="")){
    
    cat(sprintf("Retirando tendencia pela 1a Diff do banco: %s\n", db.name))
    tbl <- tbl %>% 
      arrange(Ano, Mes) %>% 
      mutate_at(.vars = vars(-Ano, -Mes), .funs = p.Diff) %>%
      filter(row_number() > 1)  %>% 
      mutate_at(.vars = vars(-Ano, -Mes), .funs = rollmean, k=12,  na.pad = TRUE, align = "right")
  }
  
  
  cat(sprintf("Padronizando variaveis do banco: %s\n", db.name))
  # tbl <- tbl %>% 
  #   arrange(Ano, Mes) %>% 
  #   mutate_at(.vars = vars(-Ano, -Mes), .funs = scale)
  
  
  g <- tbl %>%
    mutate_at(.vars = vars(-Ano, -Mes), .funs = scale) %>% 
    pivot_longer(cols = -c("Ano", "Mes")) %>% 
    mutate(Date=ymd(Ano*100+Mes, truncated = 1)) %>% 
    ggplot() +
    geom_line(aes(x=Date, y=scale(value), colour=name)) +
    facet_wrap(~name) + 
    labs(title = db.name)
  
  print(g)
  
  cat(sprintf("Excluindo dados faltantes para calculo do PCA do banco: %s", db.name))
  size.preExclusion <- nrow(tbl)
  tbl <- tbl %>% na.omit() 
  cat(sprintf("\tTotal de exclusões: %d\n", size.preExclusion-nrow(tbl)))
  
  
  cat(sprintf("Calculando PCA do banco: %s\n", db.name))
  pca <- tbl %>% 
    dplyr::select(-Ano, -Mes) %>%
    prcomp(retx = TRUE,
           center = TRUE, scale. = TRUE,
           rank=1)
  
  
  # tbl$PCA <- pca$x*get_eig(pca)[1,1]
  tbl$PCA <- pca$x
  
  #  Adiciona o resultado na tabela final
  tbl.pca <- tbl %>% 
    dplyr::select(Ano, Mes, PCA) %>% 
    right_join(tbl.pca, by=c("Ano"="Ano", "Mes"="Mes"))
  
  #  Copia o resultado do PCA para a coluna do grupo
  tbl.pca[,sprintf("PCA_%s", db.name)] <- tbl.pca$PCA
  
  # Apaga a coluna PCA (just in case)
  tbl.pca$PCA <- NULL
}

tbl.pca <- tbl.pca %>% arrange(Date)




load("../GDP_Forecast/database/IBCBR.RData")

IBCBR.tbl <- IBCBR.tbl %>% mutate(IBCBR = log(Valor_adj) - lag(log(Valor_adj)))

dic.tbl <- tibble(PCA=paste("PCA_Grupo", 1:7, sep = ""), desc=c("Juros Brasil", "Juros exterior", "Risco", "Moedas", "Petroleo", "Commodities",  "Capital"))
dic.tbl$desc <- factor(dic.tbl$desc)
levels(dic.tbl$desc) <- c(
  "Juros Brasil",
  "Juros exterior",
  "Risco",
  "Moedas",
  "Petroleo",
  "Commodities",
  "Capital"
)

tbl <- tbl.pca %>% inner_join(IBCBR.tbl, by=c("Date"="Data")) %>%
  arrange(Date) %>% 
  dplyr::select(PCA_Grupo1, PCA_Grupo2, PCA_Grupo3,
                PCA_Grupo4, PCA_Grupo5,
                PCA_Grupo6, PCA_Grupo7, IBCBR) %>%
  # filter(row_number()> 1)
  na.omit()

library(vars)

infocrit = VARselect(tbl, lag.max = 24, type = "const")
var = VAR(tbl, p = 2, type = "const")
summary(var)


for(col in paste("PCA_Grupo", 1:7, sep = "")){
  irf.a = irf(var,
              impulse = col,
              response="IBCBR",
              n.ahead = 16,
              ortho = FALSE,
              cumulative = FALSE,
              boot = TRUE)
  
  plot(irf.a)
  print(sum(unlist(irf.a$irf)[1:12]))
}



library(tidyr)
g <- tbl.pca %>%
  na.omit() %>% 
  pivot_longer(cols=-c("Ano", "Mes", "Date")) %>% 
  # left_join(dic.tbl, by=c("name"="PCA")) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=value, colour=name)) +
  facet_wrap(~name)+
  labs(title = "Indica de condições financeiras")

print(g)

tbl.pca %>%na.omit() %>% 
  mutate(ICF=PCA_Grupo1 * 0.34 +
           PCA_Grupo2*0.33+
           PCA_Grupo3*0.18+
           PCA_Grupo4*0.20+
           PCA_Grupo5*0.23+ 
           PCA_Grupo6*(-0.13)+
           PCA_Grupo7*(-0.15),
  ) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=ICF), size=1, colour = "blue", alpha= 0.5) +
  geom_hline(yintercept = 0, colour= "red") +
  labs(title = "Índice de condições financeiras")


tbl.pca %>%na.omit() %>% 
  mutate(ICF=PCA_Grupo1  +
           PCA_Grupo2 +
           PCA_Grupo3 +
           PCA_Grupo4 +
           PCA_Grupo5 + 
           PCA_Grupo6 +
           PCA_Grupo7 
  ) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=ICF), size=1, colour = "red", alpha= 0.5) +
  geom_hline(yintercept = 0, colour= "red") +
  labs(title = "Índice de condições financeiras")




