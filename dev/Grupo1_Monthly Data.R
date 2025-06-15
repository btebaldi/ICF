#' Main script - Construcao do banco de dados mensal de Swap PreDi.
#' 
#' Processos:
#'
#' Autor: Bruno Tebaldi Barbosa
#' Data: 2021-05-26
#' 
#' Input: Data inicial e final a serem processadas
#' 
#' Output: Arquivos rds (individuais e agrupados)

if(!exists("date.ini")) {date.ini <- as.Date(readline(prompt="Enter initial Date: "))}
if(!exists("date.fim")) {date.fim <- as.Date(readline(prompt="Enter final Date: "))}

# Setup -------------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)

rm(list=setdiff(ls(), c("date.ini", "date.fim")))

# User defined functions --------------------------------------------------

source("../Common Functions/Local level.R")

# Define a Daily table ----------------------------------------------------

# Define a Daily table
tbl <- tibble(Date = seq(from=date.ini,
                         to=date.fim,
                         by="month"))
tbl$wday <- wday(tbl$Date)



# Dataload ----------------------------------------------------------------

SwapPreDi <- readRDS(file = "./database/Grupo1_Swap_Pre_DI.rds")

tbl <- tbl %>% left_join(SwapPreDi, by = c("Date"="Date"))

# Group by Ano Mes --------------------------------------------------------

tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date))

# Passo 2 - Preencher Buracos
tbl <- Local.Level(tbl, cols = c("Tx_1ano", "Tx_5ano"))


# Passo 3 - Determinar datas de fim do mes
tbl <- tbl %>% 
  mutate(Spread = Tx_5ano - Tx_1ano) %>% 
  select(Ano, Mes, Tx_1ano, Tx_5ano)

# tbl %>%
#   ggplot() +
#   geom_line(aes(x=Date, y=Tx_1ano)) + 
#   geom_line(aes(x=Date, y=Spread), colour = "red")

saveRDS(object = tbl, file = "./database/Grupo1_Monthly.rds")



