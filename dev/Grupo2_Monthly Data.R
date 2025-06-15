# Main script - Construcao do banco de dados mensal de Swap PreDi.
# 
# Processos:
#
# Autor: Bruno Tebaldi Barbosa
# Data: 2021-05-26
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



# Dataload German bonds ---------------------------------------------------

GER_10Y <- read_excel("./database/Grupo 2 - Ger Bonds.xlsx", sheet = "Germany TB 10y")
GER_10Y$Date <- as.Date(GER_10Y$Date)

GER_02Y <- read_excel("./database/Grupo 2 - Ger Bonds.xlsx", sheet = "Germany TB 2y")
GER_02Y$Date <- as.Date(GER_02Y$Date)

GER_03M <- read_excel("./database/Grupo 2 - Ger Bonds.xlsx", sheet = "Germany TB 3m")
GER_03M$Date <- as.Date(GER_03M$Date)

tbl <- tbl %>%
  left_join(GER_10Y, by=c("Date"="Date")) %>% 
  left_join(GER_02Y, by=c("Date"="Date")) %>% 
  left_join(GER_03M, by=c("Date"="Date"))

colnames(tbl) <- c("Date", "wday", "GER_10Y", "GER_02Y", "GER_03M")

rm(list = c("GER_10Y", "GER_02Y", "GER_03M"))

# Dataload USA bonds ------------------------------------------------------

USA_10Y <- read_excel("./database/Grupo 2 - US bonds.xlsx", sheet = "US TB 10y")
USA_10Y$Date <- as.Date(USA_10Y$Date)

USA_02Y <- read_excel("./database/Grupo 2 - US bonds.xlsx", sheet = "US TB 2y")
USA_02Y$Date <- as.Date(USA_02Y$Date)

USA_03M <- read_excel("./database/Grupo 2 - US bonds.xlsx", sheet = "US TB 3m")
USA_03M$Date <- as.Date(USA_03M$Date)

tbl <- tbl %>%
  left_join(USA_10Y, by=c("Date"="Date")) %>% 
  left_join(USA_02Y, by=c("Date"="Date")) %>% 
  left_join(USA_03M, by=c("Date"="Date"))

colnames(tbl) <- c("Date", "wday", "GER_10Y", "GER_02Y", "GER_03M", "USA_10Y", "USA_02Y", "USA_03M")

rm(list = c("USA_10Y", "USA_02Y", "USA_03M"))

# Dataload JAP bonds ------------------------------------------------------

JAP_10Y <- read_excel("./database/Grupo 2 - Japan bonds.xlsx", sheet = "Japan TB 10y")
JAP_10Y$Date <- as.Date(JAP_10Y$Date)

JAP_02Y <- read_excel("./database/Grupo 2 - Japan bonds.xlsx", sheet = "Japan TB 2y")
JAP_02Y$Date <- as.Date(JAP_02Y$Date)

JAP_03M <- read_excel("./database/Grupo 2 - Japan bonds.xlsx", sheet = "Japan TB 3m")
JAP_03M$Date <- as.Date(JAP_03M$Date)

tbl <- tbl %>%
  left_join(JAP_10Y, by=c("Date"="Date")) %>% 
  left_join(JAP_02Y, by=c("Date"="Date")) %>% 
  left_join(JAP_03M, by=c("Date"="Date"))

colnames(tbl) <- c("Date", "wday",
                   "GER_10Y", "GER_02Y", "GER_03M",
                   "USA_10Y", "USA_02Y", "USA_03M",
                   "JAP_10Y", "JAP_02Y", "JAP_03M"
)

rm(list = c("JAP_10Y", "JAP_02Y", "JAP_03M"))

# Dataload UK bonds ------------------------------------------------------

GBR_10Y <- read_excel("./database/Grupo 2 - UK bonds.xlsx", sheet = "GB TB 10y")
GBR_10Y$Date <- as.Date(GBR_10Y$Date)

GBR_02Y <- read_excel("./database/Grupo 2 - UK bonds.xlsx", sheet = "GB TB 2y")
GBR_02Y$Date <- as.Date(GBR_02Y$Date)

GBR_03M <- read_excel("./database/Grupo 2 - UK bonds.xlsx", sheet = "GB TB 3m")
GBR_03M$Date <- as.Date(GBR_03M$Date)

tbl <- tbl %>%
  left_join(GBR_10Y, by=c("Date"="Date")) %>% 
  left_join(GBR_02Y, by=c("Date"="Date")) %>% 
  left_join(GBR_03M, by=c("Date"="Date"))

colnames(tbl) <- c("Date", "wday",
                   "GER_10Y", "GER_02Y", "GER_03M",
                   "USA_10Y", "USA_02Y", "USA_03M",
                   "JAP_10Y", "JAP_02Y", "JAP_03M",
                   "GBR_10Y", "GBR_02Y", "GBR_03M"
)

rm(list = c("GBR_10Y", "GBR_02Y", "GBR_03M"))

# Group by Ano Mes --------------------------------------------------------

tbl <- tbl %>% mutate(Ano = year(Date), Mes = month(Date))

# Passo 1 - Retirar sabdado e domingo
tbl <- tbl %>% filter(wday != 7 & wday != 1)



# Passo 2 - Determinar taxas medias no mes
fn <- function(x){
  ret <- log(1+(x/100))
  return(ret)
}

tbl.grouped <- tbl %>%
  mutate_at(vars(matches('(10Y)|(02Y)|(03M)')), .funs = fn) %>% 
  group_by(Ano, Mes) %>%
  summarise(
    GER_10Y=mean(GER_10Y, na.rm=TRUE),
    GER_02Y=mean(GER_02Y, na.rm=TRUE),
    GER_03M=mean(GER_03M, na.rm=TRUE),
    USA_10Y=mean(USA_10Y, na.rm=TRUE),
    USA_02Y=mean(USA_02Y, na.rm=TRUE),
    USA_03M=mean(USA_03M, na.rm=TRUE),
    JAP_10Y=mean(JAP_10Y, na.rm=TRUE),
    JAP_02Y=mean(JAP_02Y, na.rm=TRUE),
    JAP_03M=mean(JAP_03M, na.rm=TRUE),
    GBR_10Y=mean(GBR_10Y, na.rm=TRUE),
    GBR_02Y=mean(GBR_02Y, na.rm=TRUE),
    # GBR_03M=mean(GBR_03M, na.rm=TRUE),
    .groups="drop")



saveRDS(object = tbl.grouped, file = "./database/Grupo2_Monthly.rds")




