#' Main script - Construcao do banco de dados mensal.
#'
#' Processos:
#'
#' Autor: Bruno Tebaldi Barbosa Data: 2021-05-26
#'
#' Input: Data inicial e final a serem processadas
#'
#' Output: Arquivos rds (Grupo1_Monthly.rds, Grupo2_Monthly.rds,
#' Grupo3_Monthly.rds, Grupo4_Monthly.rds, Grupo5_Monthly.rds,
#' Grupo6_Monthly.rds, Grupo7_Monthly.rds)


cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 0B"))

cat("Definir as datas para analise do ICF\n")
date.ini <- as.Date(readline(prompt="Enter initial Date: "))
date.fim <- as.Date(readline(prompt="Enter final Date: "))


# Setup -------------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

rm(list=setdiff(ls(), c("date.ini", "date.fim")))

# User defined functions --------------------------------------------------

source("./lib/Local level.R")

log_retorno <- function(retorno){
  ret <- log(1+(retorno/100))
  return(ret)
}


# User defined variables --------------------------------------------------

MainDirPath <- file.path("database", "database_ICF")

# Dataload ----------------------------------------------------------------

file_path <- file.path("P:", "Economia", "Base_ICF2.xlsx")

db <- read_excel(file_path,
                 sheet = "Dados", 
                 na = c("#N/A N/A", "#N/A", "N/A"),
                 range = cell_limits(ul=c(2,1), lr=c(NA,27)), 
                 col_types = c("date",      # Date
                               "numeric",   # USGG3M Index
                               "numeric",   # GTGBP3MO Corp
                               "numeric",   # GTDEM3M Govt
                               "numeric",   # GTJPY3MO Corp
                               "numeric",   # USGG2YR Index
                               "numeric",   # GTGBP2YR Corp
                               "numeric",   # GTDEM2Y Govt
                               "numeric",   # GTJPY2YR Corp
                               "numeric",   # USGG10YR Index
                               "numeric",   # GTGBP10YR Corp
                               "numeric",   # GTDEM10YR Corp
                               "numeric",   # GTJPY10YR Corp
                               "numeric",   # VIX Index
                               "numeric",   # BRAZIL CDS USD SR 5Y D14 Corp
                               "numeric",   # DXY Curncy
                               "numeric",   # USTWEME Index
                               "numeric",   # BRL REGN Curncy
                               "numeric",   # CL1 COMB Comdty
                               "numeric",   # CO1 Comdty
                               "numeric",   # CRB FOOD Index
                               "numeric",   # CRB METL Index
                               "numeric",   # MXWO Index
                               "numeric",   # MXEF Index
                               "numeric",   # IBOV Index
                               "numeric",   # BCSFLPDV Curncy
                               "numeric"   	# BCSFTPDV CMPN Curncy
                 ))


#  Leitura de dados de 'Fontes para o ICF'
file_path <- file.path(MainDirPath, "fontes para o ICF.xlsx")

db_sources <- read_excel(file_path, sheet = "Dados")

# Ajusta o nome das colunas na tabela
newColNames <- NULL
for (col in colnames(db)) {
  if(col == "d"){
    newName <- "Date"
  } else {
    newName <- db_sources %>% filter(Bloomberg == col) %>% pull(ColCode)
  }
  newColNames <- c(newColNames, newName)
}

colnames(db) <- newColNames

db$Date <- as.Date(db$Date)

# Define a Daily table ----------------------------------------------------

if(is.na(date.ini)){
  date.ini <- max(as.Date("2003-08-01"), min(db$Date))
}
if(is.na(date.fim)){
  date.fim <- max(db$Date)
}


# Define a Daily table
tbl.period <- tibble(Date = seq(from=date.ini, to=date.fim, by="month"))
tbl.period <- tbl.period %>% mutate(Ano=year(Date), Mes=month(Date))

# Grupo 1 -----------------------------------------------------------------

SwapPreDi <- readRDS(file = file.path(MainDirPath, "Grupo1_Swap_Pre_DI.rds"))

tbl <- tbl.period %>% left_join(SwapPreDi, by = c("Date"="Date")) %>% arrange(Date)

g <- tbl %>% 
  pivot_longer(cols = starts_with("Tx"), names_to = "Rate", values_to = "value") %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=value, colour=Rate)) +
  labs(title = "Swap pre-Di", subtitle = "Indice de condições financeiras",
       colour = NULL, y=NULL, x=NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(filename = sprintf("Swap Pre-Di.png"),
       path = "./Export/ICF",
       plot = g,
       scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)

# Passo 3 - Determinar datas de fim do mes
tbl <- tbl %>% dplyr::select(Ano, Mes, starts_with("Tx"))

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo1_Monthly.rds"))


# Grupo 2 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 2])

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Determinar taxas medias no mes
tbl.grouped <- tbl %>%
  mutate_at(vars(matches('Bond_')), .funs = log_retorno) %>% 
  group_by(Ano, Mes) %>%
  summarise(
    GER_10Y=mean(Bond_10Y_GER, na.rm=TRUE),
    GER_02Y=mean(Bond_2Y_GER, na.rm=TRUE),
    GER_03M=mean(Bond_3M_GER, na.rm=TRUE),
    USA_10Y=mean(Bond_10Y_USA, na.rm=TRUE),
    USA_02Y=mean(Bond_2Y_USA, na.rm=TRUE),
    USA_03M=mean(Bond_3M_USA, na.rm=TRUE),
    JAP_10Y=mean(Bond_10Y_JAP, na.rm=TRUE),
    JAP_02Y=mean(Bond_2Y_JAP, na.rm=TRUE),
    JAP_03M=mean(Bond_3M_JAP, na.rm=TRUE),
    GBR_10Y=mean(Bond_10Y_GBR, na.rm=TRUE),
    GBR_02Y=mean(Bond_2Y_GBR, na.rm=TRUE),
    # GBR_03M=mean(Bond_3M_GBR, na.rm=TRUE), # Retirado devido a muitos dados faltantes
    .groups="drop")

# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo2_Monthly.rds"))

# Grupo 3 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 3] )

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 3 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(CDS = mean(CDB_5Y, na.rm=TRUE),
            VIX = mean(VIX, na.rm=TRUE),
            .groups = "drop")

tbl.grouped[is.na(tbl.grouped)] <- NA

# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo3_Monthly.rds"))


# Grupo 4 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 4])

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(DXY = mean(DXY, na.rm=TRUE),
            BRL_USD = mean(USD_BRL, na.rm=TRUE), 
            UDS_Emerging = mean(UDS_Emerging , na.rm=TRUE),
            .groups = "drop")

# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo4_Monthly.rds"))

# Grupo 5 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 5])

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(Date=max(Date), .groups = "drop") %>% 
  inner_join(tbl, by = c("Date"="Date", "Ano"="Ano", "Mes"="Mes")) %>% 
  dplyr::select(Ano, Mes, WTI, Brent)


# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo5_Monthly.rds"))

# Grupo 6 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 6])

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(Food = mean(CRB_Food, na.rm=TRUE),
            Metal = mean(CRB_Metals, na.rm=TRUE), 
            .groups = "drop")

# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo6_Monthly.rds"))

# Grupo 7 -----------------------------------------------------------------

tbl <- db %>% dplyr::select(Date, db_sources$ColCode[db_sources$Grupo_Id == 7])

# Group by Ano, Mes
tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date), wday = wday(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(Date=max(Date), .groups = "drop") %>% 
  inner_join(tbl, by = c("Date"="Date", "Ano"="Ano", "Mes"="Mes")) %>% 
  dplyr::select(Ano, Mes, MSCI_Develop, MSCI_Emergentes, Ibov)

# Passo 3 - Determinar taxas medias no mes
tbl <- tbl.period %>% left_join(tbl.grouped, by = c("Ano"="Ano", "Mes"="Mes")) %>%
  arrange(Date) %>% 
  dplyr::select(-Date)

saveRDS(object = tbl, file = file.path(MainDirPath, "Grupo7_Monthly.rds"))