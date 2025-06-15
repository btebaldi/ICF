# Main script - Construcao do banco de dados Mensal de Swap PreDi.
# 
# Processos:
#   (1) Download de informacaoes da B3
#   (2) Construcao das medias mensais
#
# Autor: Bruno Tebaldi Barbosa
# Data: 2021-05-19
# 
# Input: Data inicial e final a serem processadas
# 
# Output: Arquivos rds (individuais e agrupados)
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 0A"))

cat("Entre com as datas para download de dados da B3 de swap Pre-DI\n")
date.ini <- as.Date(readline(prompt="Enter initial Date: "))
if(is.na(date.ini)){
  date.ini <- as.Date("2003-01-01")
}
date.fim <- as.Date(readline(prompt="Enter final Date: "))
if(is.na(date.fim)){
  date.fim <- Sys.Date()
}

script <- "./scripts/ICF/Handle Swap Di Pre/Get data Swap PreDi.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./scripts/ICF/Handle Swap Di Pre/Construcao e update de serie Swap PreDi.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)
