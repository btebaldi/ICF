#' Autor: Bruno Tebaldi Barbosa
#'
#' Data: 2021-05-28
#'
#' Calcula os pesos de influencia de cada PCA por meio do efeito total dos
#' choques em cada PCA.
#'
#'
#' Input: Data inicial e final a serem processadas
#'
#' Output: Arquivos rds (individuais e agrupados)

# Setup -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(vars)

cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 2 - Calculo dos pesos do ICF"))

rm(list=setdiff(ls(), c("date.ini", "date.fim")))

# Arquivo que vai guardar os pesos.
PCA_tbl.filepath <- file.path("database", "database_ICF", "PCA_tbl.rds")
wights.filepath <- file.path("database", "database_ICF", "ICF_Weights.rds")

# User defined functions --------------------------------------------------

source("./lib/Get_BCB_SGS.R")


D.ln <- function(x){
  ret <- log(x) - lag(log(x))
  return(ret)
}



# Load PCA factors --------------------------------------------------------

cat(sprintf("Carregando PCAs\n"))
tbl.pca <- readRDS(file = PCA_tbl.filepath)


# Amarra de PCA com IBCBR -------------------------------------------------

cat(sprintf("Cruzando PCAs com IBCBR (D.ln)\n"))

IBCBR.tbl <- Get_IBCBR()

IBCBR.tbl <- IBCBR.tbl %>% mutate(IBCBR = D.ln(Valor))

tbl.pca <- tbl.pca %>% inner_join(IBCBR.tbl, by = c("Date"="Data"))



# Construcao de VAR para IRF ----------------------------------------------

#  limpeza de variaveis para controle.
# rm(list=setdiff(ls(), c("date.ini", "date.fim", "tbl.pca", )))


# seleciono apenas as variaveis do VAR e excluo linhas com NA. ATENCAO: A
# exclusao de NA´s assume que nâo existe buracos nas series, os NA´s estao
# apenas nas pontas;
tbl <- tbl.pca %>%
  arrange(Date) %>% 
  dplyr::select(PCA_Grupo1, PCA_Grupo2, PCA_Grupo3,
                PCA_Grupo4, PCA_Grupo5,
                PCA_Grupo6, PCA_Grupo7, IBCBR) %>%
  na.omit()

#  Criterio de informação (apenas para informar o lag.)
infocrit = VARselect(tbl, lag.max = 12, type = "const")

lagSelection <- infocrit$selection["SC(n)"]

cat(sprintf("VAR Information Criteria [SC(n)]: %d\n", lagSelection))

# Calcula o VAR 
var = VAR(tbl, p = lagSelection, type = "const")

weights <- rep(NA, 7)
names(weights) <- paste("PCA_Grupo", 1:7, sep = "")

# Para cada PCA faz a simulacao do IRF. Choque no PCA e resposta do IBCBR
irf.analise = irf(var,
                  # impulse = col,
                  response="IBCBR",
                  n.ahead = 16,
                  ortho = FALSE,
                  cumulative = FALSE,
                  boot = TRUE)

for(col in paste("PCA_Grupo", 1:7, sep = "")){
  # Imprime a resposta just as information
  IBCBR_IRF <- as.vector(irf.analise$irf[[col]])
  IBCBR_IRF.ub <- as.vector(irf.analise$Upper[[col]])
  IBCBR_IRF.lb <- as.vector(irf.analise$Lower[[col]])
  
  tbl.IRF <- tibble(IBCBR_IRF, IBCBR_IRF.ub, IBCBR_IRF.lb) %>% 
    mutate(Id = row_number())
  
  g <- ggplot(tbl.IRF) + 
    geom_line(aes(x = Id, IBCBR_IRF)) + 
    geom_line(aes(x = Id, IBCBR_IRF.ub), linetype = "dashed", colour = "blue") + 
    geom_line(aes(x = Id, IBCBR_IRF.lb), linetype = "dashed", colour = "blue") + 
    geom_hline(yintercept = 0, colour="red") +
    labs(title = sprintf("Choque em %s", col),
         y="IBC-BR",
         x = NULL) + 
    theme_bw()
  
  ggsave(filename = sprintf("IRF_%s.png", col),
         path = "./Export/ICF",
         plot = g,
         scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)  
  
  weights[col] <- sum(IBCBR_IRF[1:12])
}


# Salva os pesos do ICF
saveRDS(weights, file = wights.filepath)

cat(sprintf("%s: %f\n", names(weights), weights))
