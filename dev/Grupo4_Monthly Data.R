# Main script - Construcao do banco de dados mensal do grupo 5.
# 
# Processos:
#
# Autor: Bruno Tebaldi Barbosa
# Data: 2021-05-27
# 
# Input: Data inicial e final a serem processadas
# 
# Output: Arquivos rds (individuais e agrupados)

if(!exists("date.ini")) {date.ini <- as.Date(readline(prompt="Enter initial Date: "))}
if(!exists("date.fim")) {date.fim <- as.Date(readline(prompt="Enter final Date: "))}

# Setup -------------------------------------------------------------------
library(readxl)
library(dplyr)
# library(tidyr)
# library(ggplot2)
library(lubridate)
library(fredr)

rm(list=setdiff(ls(), c("date.ini", "date.fim")))



# User defined functions --------------------------------------------------

source("../Common Functions/Local level.R")

# Define a Daily table ----------------------------------------------------

# Define a Daily table
tbl <- tibble(Date = seq(from=date.ini,
                         to=date.fim,
                         by="day"))
tbl$wday <- wday(tbl$Date)



# Dataload ----------------------------------------------------------------

# readxl::excel_sheets("./database/Grupo 4 - Currency.xlsx")

DXY <- read_excel("./database/Grupo 4 - Currency.xlsx", sheet = "DXY")
DXY$Date <- as.Date(DXY$Date)

BRL_USD <- read_excel("./database/Grupo 4 - Currency.xlsx", sheet = "BRL_USD")
BRL_USD$Date <- as.Date(BRL_USD$Date)

tbl <- tbl %>% left_join(DXY, by = c("Date"="Date"))

tbl <- tbl %>% left_join(BRL_USD, by = c("Date"="Date"))
colnames(tbl) <- c("Date", "wday", "DXY", "BRL_USD")

fredr_set_key("e5bb5eb00c5bdbdb3dbc4277217ac3ef")

USD_Emerging <- fred.data <- fredr(series_id = "DTWEXEMEGS",
                                   observation_start = date.ini,
                                   observation_end = date.fim)

tbl <- tbl %>% left_join(USD_Emerging, by = c("Date"="date")) %>% 
  select(Date, wday, DXY, BRL_USD, value)
colnames(tbl) <- c("Date", "wday", "DXY", "BRL_USD", "USD_emerg")

rm(list = c("USD_Emerging", "BRL_USD", "DXY"))

# Group by Ano Mes --------------------------------------------------------

tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Preencher Buracos
# tbl <- Local.Level(tbl, cols = c("CDS", "VIX"))

# Passo 3 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(DXY = mean(DXY, na.rm=TRUE),
            BRL_USD = mean(BRL_USD, na.rm=TRUE), 
            # USD_emerg = mean(USD_emerg, na.rm=TRUE),
            .groups = "drop")

saveRDS(object = tbl.grouped, file = "./database/Grupo4_Monthly.rds")
