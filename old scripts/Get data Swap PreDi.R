# Processo de unificacao dos arquivos de Swap Pre-Di que foram baixados da
# bovespa em arquivos anuais
#
# Autor: Bruno Tebaldi Barbosa

# Data: 2021-05-19
# 
# Input: Arquivos rds baixados da b3 em periodicidade diaria
# 
# Output: Arquivos rds aagrupados por ano

# Setup -------------------------------------------------------------------
cat(sprintf("[%s] Inicio de Script\n", Sys.time()))
#  Data inicial e final do processo 
if(!exists("date.ini")){
  date.ini <- Sys.Date()-1000 }
if(!exists("date.fim")){
  date.fim <- Sys.Date() }

library(dplyr)
library(progress)
library(readr)
library(lubridate)

rm(list=setdiff(ls(), c("date.ini", "date.fim")))

source("./lib/function Download (web scrapping) Swap Di_Pre.R", encoding = "UTF-8")


# Configuracoes da camada logica ------------------------------------------

# Flag de impressao de status
verbose <- FALSE
cat(sprintf("Avaliando datas de %s ate %s\n", date.ini, date.fim))


# Lista de datas
date.list <- seq(from = date.ini,to = date.fim,by = "day")

# Mascara dos caminhos dos arquivos
database.mask <- "./database/SwapDiPre_daily/%s.rds"
destination.mask <- "./database/SwapDiPre_yearly/Swap PreDI_Yearly.rds"



# download das informacoes ------------------------------------------------

# Path do arquivo de destino
destination.path <- sprintf(destination.mask)
yearly.tbl <- readRDS(file = destination.path)

# barra de progresso do processo
pb <- progress_bar$new(total = length(date.list))

if(TRUE){
  
i <- 3
for(i in 1:length(date.list)) {
  # busca a data dos dados
  date <- date.list[i]
  
  # Determina o arquivo a ser aberto
  database.path <- sprintf(database.mask, date)
  
  
  #  Se nao existe arquivo para aquela data pula para a proxima data de
  #  processamento
  if(file.exists(database.path)) {
    #  Carrega o banco de dados
    tbl <- readRDS(file = database.path)
    if(verbose){ cat(sprintf("Dados diarios da data %s encontrados.\n", date)) }
  } else {
    if(verbose){ cat(sprintf("Fazendo donwload da data %s do site da B3\n", date)) }
    
    tbl <- Download.B3.SwapDi(date, verbose=FALSE)
    if(nrow(tbl) > 0){
      #  salva o banco de dados para nao ter que fazer novo download
      if(verbose){ cat(sprintf("Salvando dados diarios em disco.\n", date)) }
      saveRDS(tbl, file = database.path)
    } else {
      if(verbose){ cat(sprintf("A busca na B3 não retornou dados para a data %s\n", date)) }
    }
  }
  
  # Apenas toma alguma acao se ouver dados para serem acrescentados
  if(nrow(tbl) > 0){
    if(any(tbl$Data %in% yearly.tbl$Data)) {
      if(verbose){
        cat(sprintf("\nData %s já esta presente no banco de dados. Nenhuma ação foi feita\n", date))
      }
    } else {
      yearly.tbl <- dplyr::bind_rows(yearly.tbl, tbl)
    }
  }
  
  # incrementa a barra de progresso
  pb$tick()
}

# Termina com a barra de progresso
pb$terminate()

# Organiza o banco de dados por data
yearly.tbl <- yearly.tbl %>% arrange(Data)

# Salva o banco de dados
saveRDS(yearly.tbl, file = destination.path)
}

cat(sprintf("[%s] Fim de Script\n", Sys.time()))


