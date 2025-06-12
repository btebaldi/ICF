rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)

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


# Data load ---------------------------------------------------------------

tbl <- read_xlsx("./Database/FCI data.xlsx", sheet = "FCI")
tbl <- tbl %>% arrange(Date)
# tbl$dxydm <- 1/tbl$dxydm
tbl$ibov <- log(tbl$ibov)


Moedas <- read_xlsx("./Database/Dados Teo.xlsx",
                    sheet = 1,
                    range = cell_limits(ul = c(6, 2), lr = c(NA, NA)),
                    col_names = c("Data", "DXY", "USDCAD", "USDEUR", "USDJPY",
                                  "USDGBP", "USDSEK", "USDCHF", "DOLOF"), 
                    col_types = c("date", rep("numeric", 8)))

Moedas$EURUSD <- 1/Moedas$USDEUR

Moedas <- Moedas %>% mutate(myDXY = (49.86985) * USDEUR^(+0.576) * (USDJPY)^(+0.136) * USDGBP^(+0.119) *
                              USDCAD^(+0.091) * USDSEK^(+0.042) * USDCHF^(+0.036))

tail(Moedas)

ggplot(Moedas) + geom_point(aes(x = myDXY, y = DXY))

summary(Moedas$myDXY/Moedas$DXY)

Moedas %>% 
  na.omit() %>% 
  mutate(Data = as.Date(Data)) %>% 
  inner_join(tbl, by = c("Data" = "Date")) -> zz

ggplot(zz) + geom_point(aes(x = dxydm, y = DXY))
ggplot(zz) + geom_point(aes(x = dxydm, y = myDXY))

Moedas %>% 
  mutate_at(.vars = c("USDCAD", "USDEUR", "USDJPY",
                      "USDGBP", "USDSEK", "USDCHF", "DOLOF", "EURUSD", "DXY"),
            .funs = log) %>%
  # mutate_if(.predicate = is.numeric, .funs = D.ln) %>% 
  na.omit() %>% 
  lm(DXY ~ 1 + USDCAD + USDEUR + USDJPY + USDGBP + USDSEK + USDCHF, data = .) -> mdl

mdl %>% 
  summary()

exp(mdl$coefficients[1])
a <- mdl$coefficients[-1]
a/sum(a)


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
    dplyr::select(all_of(database[[i]])) %>% scale()
  
  g <- database[[i]] %>% 
    as_tibble() %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(cols = -id) %>% 
    ggplot() + geom_line(aes(x = id, y = value, colour = name))
  
  print(g)
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
                  Date = as.Date(tbl$Date) )


# Calculo de PCA ----------------------------------------------------------
