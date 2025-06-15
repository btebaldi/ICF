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

# readxl::excel_sheets("./database/Grupo 7 - Capital Market.xlsx")

IBOV <- read_excel("./database/Grupo 7 - Capital Market.xlsx", sheet = "IBOV")
IBOV$Date <- as.Date(IBOV$Date)

MXEF <- read_excel("./database/Grupo 7 - Capital Market.xlsx", sheet = "MXEF Index")
MXEF$Date <- as.Date(MXEF$Date)

MXWO <- read_excel("./database/Grupo 7 - Capital Market.xlsx", sheet = "MXWO Index")
MXWO$Date <- as.Date(MXWO$Date)

EWZ <- read_excel("./database/Grupo 7 - Capital Market.xlsx", sheet = "EWZ")
EWZ$Date <- as.Date(EWZ$Date)

tbl <- tbl %>%
  left_join(IBOV, by = c("Date"="Date")) %>% 
  left_join(MXEF, by = c("Date"="Date")) %>% 
  left_join(MXWO, by = c("Date"="Date"))

colnames(tbl) <- c("Date", "wday",
                   "IBOV", "Vol_IB",
                   "MXEF", "Vol_EF",
                   "MXWO", "Vol_WO")

tbl <- select(tbl, c("Date", "wday", "IBOV", "MXEF", "MXWO"))

rm(list = c("IBOV", "MXEF", "MXWO", "EWZ"))

# Group by Ano Mes --------------------------------------------------------

tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)

# Passo 2 - Preencher Buracos
tbl <- Local.Level(tbl, cols = c("IBOV", "MXEF", "MXWO"))

# Passo 3 - Determinar datas de fim do mes
tbl.grouped <- tbl %>%
  group_by(Ano, Mes) %>%
  summarise(Date=max(Date), .groups = "drop") %>% 
  inner_join(tbl, by = c("Date"="Date", "Ano"="Ano", "Mes"="Mes")) %>% 
  select(Ano, Mes, IBOV, MXEF, MXWO)

tbl.grouped <- tbl.grouped %>%
  mutate_at(.vars = c("IBOV", "MXEF", "MXWO"),
            .funs = ts,
            start=c(tbl.grouped$Ano[1],tbl.grouped$Mes[1]),
            frequency=12)


saveRDS(object = tbl.grouped, file = "./database/Grupo7_Monthly.rds")
