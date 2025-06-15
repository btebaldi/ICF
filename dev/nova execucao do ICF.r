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

#' https://en.wikipedia.org/wiki/Hodrick%E2%80%93Prescott_filter ' ' Hodrick and
#Prescott suggest 1600 as a value for lambda ' for quarterly data. Ravn and
#Uhlig (2002) state that λ {\displaystyle \lambda } should vary by the fourth
#power of the frequency observation ratio; thus, λ {\displaystyle \lambda }
#should equal 6.25 (1600/4^4) for annual data and 129,600 (1600*3^4) for monthly
#data;[5] in practice, λ = 100 {\displaystyle \lambda =100} for yearly data and
#λ = 14 , 400 {\displaystyle \lambda =14,400} for monthly data are commonly
#used, however.

# Setup ---------------------------------------------------------

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vars)

# User defined functions --------------------------------------------------

source("./TestaRaizUnitaria.R")

D.ln <- function(x){
  ret <- log(x) - lag(log(x))
  return(ret)
}

Delta <- function(x){
  ret <- x - dplyr::lag(x)
  return(ret)
}


detrend <- function(x){
  # x <- database[[4]][[1]]
  # hp.filter <- mFilter::hpfilter(x = x, freq = 129600, type = "lambda")
  hp.filter <- mFilter::hpfilter(x = x, freq = 14400, type = "lambda")
  # plot(hp.filter, ask = FALSE)
  
  # class(hp.filter$cycle)
  return(hp.filter$cycle)
  
}

# Data load ---------------------------------------------------------------


# janeiro/2006 até dezembro/2014

tbl <- read_xlsx("./Database/FCI data.xlsx", sheet = "FCI")
tbl <- tbl %>% arrange(Date)
# tbl$dxydm <- 1/tbl$dxydm
tbl$ibov <- log(tbl$ibov)

# Lista de colunas que devem ser buscadas na base de dados
database <- list( G1 = c("pre1ano", "pre5ano"),
                  G2 = c("ge2y", "ge10y", "ge3m", "jp10y", "jp2y", "jp3m", "uk10y", "uk2y", "uk3m", "us10y", "us2y", "us3m"),
                  G3 = c("vix", "cds"),
                  G4 = c("brl", "dxydm"),
                  G5 = c("brent", "wti"),
                  G6 = c("crbfood", "crbmetals"),
                  G7 = c("mscidm", "msciem", "ibov") )

# Constroi a base de dados de cada grupo 
i <- 1
for(i in seq_along(database)){
  database[[i]] <- tbl %>%
    dplyr::select(all_of(database[[i]]))
  
  g <- database[[i]] %>%
    # scale() %>% 
    as_tibble() %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(cols = -id) %>% 
    ggplot() + geom_line(aes(x = id, y = value, colour = name)) + 
    facet_wrap(~name)
  
  print(g)
}

for(i in seq_along(database)){
  database[[i]] %>% summary() %>% print()
}

# Verificar integridade do banco de dados
# Verificar se existe dados faltantes
# Completar dados faltantes.

pca.tbl <- tibble(G1 = NA,
                  G2 = NA,
                  G3 = NA,
                  G4 = NA,
                  G5 = NA,
                  G6 = NA,
                  G7 = NA,
                  Date = as.Date(tbl$Date2) )


# Calculo de PCA ----------------------------------------------------------

i <- 1
for (i in seq_along(database)) {
  
  cat("Processando grupo", i, "\n")
  
  db <- database[[i]]
  
  if(any(!complete.cases(db))){
    cat("O Grupo", i, "contem NA's\n")
    cat("Posições:", sprintf("%d", which(!complete.cases(db))), "\n")
  }
  
  
  test <- Testa.RaizUnitaria(dados = db, critical = "1pct", N.lags = 2)
  print(test)
  
  
  # Retira a primeira diferenca da serie
  if(i %in% c(1,2,4,5,6,7,3) ){
    cat(sprintf("Retirando tendencia pela 1a Diff do Grupo %s\n", i))
    
    db <- db %>%
      mutate_all(.funs = detrend)
      # mutate_all(.funs = Delta)
  
    
    index.PCA <- which(!is.na(db[[1]]))
  }
  
  cat(sprintf("Calculando PCAs do Grupo %d\n", i))
  pca <- db[index.PCA, ] %>% 
    prcomp(retx = TRUE,
           center = TRUE,
           scale. = TRUE,
           rank=1)
  
  pca.tbl[index.PCA, i] <- pca$x
  
}


col <- 3
for(col in 1:7){
  g <- ggplot(pca.tbl) + 
    geom_line(mapping = aes(x = Date, y = !!sym(sprintf("G%d",col)))) + 
    labs(title = sprintf("PCA Grupo %s", col))
  print(g)
}

# ggplot(pca.tbl) + 
#   geom_line(mapping = aes(x = Date, y = !!sym(sprintf("G%d",col)))) + 
#   geom_line(mapping = aes(x = as.Date(Date), y = vix/5), colour = "red", data = tbl) +
#   labs(title = sprintf("PCA Grupo %s", col))


cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 2 - Calculo dos pesos do ICF"))


pca.tbl$Dln.IbcBr <- D.ln(tbl$ibcbr)

# Construcao de VAR para IRF ----------------------------------------------

# seleciono apenas as variaveis do VAR e excluo linhas com NA. ATENCAO: A
# exclusao de NA´s assume que nâo existe buracos nas series, os NA´s estao
# apenas nas pontas;
tbl.var <- pca.tbl %>%
  # pivot_longer(cols = c(paste0("G", 1:7), "ln.IbcBr"))
  # arrange(Date) %>% 
  dplyr::select(G1, G2, G3, G4, G5, G6, G7, Dln.IbcBr, Date)




which(pca.tbl$Date == "2020-04-01")
which(pca.tbl$Date == "2018-05-01")

tbl.var$D_Pan = 0
tbl.var$D_Pan[137] = 1

tbl.var$D_Cam = 0
tbl.var$D_Cam[114] = 1

tbl.var %>% 
  mutate(id = row_number()) %>% 
  ggplot() + 
  geom_line(aes(x =id, y = Dln.IbcBr)) + 
  geom_point(aes(x =id, y = Dln.IbcBr), alpha = 0.4, colour = "blue") + 
  geom_point(aes(x =id, y = D_Pan*(-0.13))) + 
  geom_vline(xintercept = 137) + 
  labs()

tbl.var2 <- tbl.var

mm <- matrix(NA, nrow = 192, ncol = 7)

for(j in seq(191, 192, 1)){
  if(!is.na(mm[j, 1])){
    print("skip")
    next()
  }
  
  
  tbl.var <- tbl.var2 %>% filter(row_number()<j) %>% na.omit()
  
  
  #  Criterio de informação (apenas para informar o lag.)
  infocrit = VARselect(tbl.var[, c(1:8)], lag.max = 12, type = "const",
                       # season = 12,
                       exogen = tbl.var[, c("D_Pan", "D_Cam")] )

  infocrit
  lagSelection <- infocrit$selection["SC(n)"]
  lagSelection <- 2
  # 
  # cat(sprintf("VAR Information Criteria [SC(n)]: %d\n", lagSelection))
  
  
  # Calcula o VAR 
  if(j>=137){
    var = VAR(tbl.var[, 1:8],
              p = lagSelection, 
              type = "const",
              # season = 12,
              exogen = tbl.var[, c("D_Pan", "D_Cam")])
  } else {
    
    var = VAR(tbl.var[, 1:8],
              p = lagSelection, 
              type = "const")
  }
  
  weights <- rep(NA, 7)
  names(weights) <- paste("G", 1:7, sep = "")
  
  # Para cada PCA faz a simulacao do IRF. Choque no PCA e resposta do IBCBR
  irf.analise = irf(var,
                    # impulse = col,
                    response="Dln.IbcBr",
                    n.ahead = 36,
                    ortho = FALSE,
                    cumulative = TRUE,
                    boot = TRUE)
  
  for(col in paste("G", 1:7, sep = "")){
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
      labs(title = sprintf("Choque em %s com lag %d", col, lagSelection),
           y="IBC-BR",
           x = NULL) +
      theme_bw()
    
    print(g)
    
    # ggsave(filename = sprintf("IRF_%s.png", col),
    #        path = "./Export/ICF",
    #        plot = g,
    #        scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)  
    
    weights[col] <- IBCBR_IRF[12]
  }
  # weights/sum(weights)
  print(j)
  mm[j, ] <- weights
}
# Salva os pesos do ICF
# saveRDS(weights, file = wights.filepath)

cat(sprintf("SUM = %0.2f\n", sum(weights)), sprintf("%s: %5.1f\n", names(weights), 100*weights/sum(weights)))




mm %>% 
  as_tibble() %>% 
  mutate(Total = V1+V2+V3+V4+V5+V6+V7, id = row_number()) %>% 
  mutate(V1=V1/Total,V2=V2/Total,V3=V3/Total,V4=V4/Total,V5=V5/Total,V6=V6/Total,V7=V7/Total, Total = 1*sign(Total)) %>% 
  na.omit() %>% 
  pivot_longer(cols = -id) %>% 
  ggplot() + 
  geom_line(aes(x=id, y = value)) + 
  geom_col(aes(x=id, y = ln.IbcBr), data = tbl.var %>% mutate(id= row_number()), fill="blue", alpha =0.3) + 
  geom_vline(xintercept = 136)+
  geom_vline(xintercept = 73, colour = "blue")+
  geom_vline(xintercept = 99, colour = "blue")+
  geom_hline(yintercept = 0, colour = "red")+
  # ylim(-2,2) +
  facet_wrap(~name)

# library(seasonal)
# y = ts(pca.tbl$ln.IbcBr, start = c(2008,12), frequency = 12)
# jj <- seas(x = y)
# outlier(jj)
# which(!is.na(outlier(jj)))
# 
# plot(jj)
tbl[136,]
