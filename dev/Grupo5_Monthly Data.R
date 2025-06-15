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

WTI <- read_excel("./database/Grupo 5 - Oil.xlsx", sheet = "WTI")
WTI$Date <- as.Date(WTI$Date)

Brent <- read_excel("./database/Grupo 5 - Oil.xlsx", sheet = "Brent")
Brent$Date <- as.Date(Brent$Date)

tbl <- tbl %>% left_join(WTI, by = c("Date"="Date"))

tbl <- tbl %>% left_join(Brent, by = c("Date"="Date"))
colnames(tbl) <- c("Date", "wday", "WTI", "Brent")


rm(list = c("WTI", "Brent"))

# Group by Ano Mes --------------------------------------------------------

tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Preencher Buracos
tbl <- Local.Level(tbl, cols = c("WTI", "Brent"))

# Passo 3 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(Date=max(Date), .groups = "drop") %>% 
  inner_join(tbl, by = c("Date"="Date", "Ano"="Ano", "Mes"="Mes")) %>% 
  select(Ano, Mes, WTI, Brent)


saveRDS(object = tbl.grouped, file = "./database/Grupo5_Monthly.rds")




