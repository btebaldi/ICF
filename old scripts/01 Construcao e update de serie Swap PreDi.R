# Processo de construcao do banco de dados de Swap Pre-Di que foram baixados da
# bovespa. Aquivos construidos a partir dos arquivos dados diários
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2021-05-20
# Revisao: 2025-06-12

# Setup -------------------------------------------------------------------
# Limpa o ambiente de trabalho para evitar conflitos de variáveis antigas
rm(list = ls())

# Imprime mensagem de início do script com timestamp
cat(sprintf("[%s] Inicio de Script\n", Sys.time()))

# Carrega as bibliotecas necessárias para manipulação de dados, progresso, datas e paralelização
library(dplyr)      # Manipulação de dados
library(readr)      # Leitura e escrita de arquivos CSV
library(progress)   # Barra de progresso
library(lubridate)  # Manipulação de datas
library(furrr)      # Paralelização com purrr
library(ggplot2)    # para graficos


# user-defined functions ---------------------------------------------------
custom.mean <- function(x, na.rm = TRUE) {
  y <- log(1 + x)
  ret <- mean(y, na.rm = na.rm)
  ret <- exp(ret) - 1  # Converte de log para taxa
  return(ret)
}


# Configurações iniciais --------------------------------------------------

# Define o plano de execução paralela usando todos os núcleos disponíveis menos um
# 'multisession' permite execução paralela em múltiplos processos
future::plan(future::multisession, workers = future::availableCores() - 1)

# Carrega os dados diários de Swap Pre-DI e ordena por data e dias corridos
SwapDiPre_daily <- readr::read_rds("./Database/SwapDiPre_daily.rds") %>%
  arrange(Data, DiasCorridos)

# Função para processar cada data individualmente
process_date <- function(date, data_tbl) {
  
  # Filtra os dados para a data específica
  day.tbl <- data_tbl %>% dplyr::filter(Data == date)
  
  # Cria uma função spline para interpolação das taxas
  spfun <- splinefun(day.tbl$DiasCorridos, y = day.tbl$TX_252 / 100)
  
  # Retorna um tibble com as taxas interpoladas para diferentes prazos
  tibble(
    Data = date,
    wday = lubridate::wday(date),  # Dia da semana da data
    Tx_1ano = spfun(252),          # 1 ano (252 dias úteis)
    Tx_2ano = spfun(252 * 2),      # 2 anos
    Tx_3ano = spfun(252 * 3),      # 3 anos
    Tx_4ano = spfun(252 * 4),      # 4 anos
    Tx_5ano = spfun(252 * 5),      # 5 anos
    Tx_10ano = spfun(252 * 10)     # 10 anos
  )
}

# Processa todas as datas em paralelo, aplicando a função acima
tbl <- furrr::future_map_dfr( unique(SwapDiPre_daily$Data),       # Vetor de datas únicas
                              ~process_date(.x, SwapDiPre_daily), # Função a ser aplicada
                              .progress = TRUE )                  # Mostra barra de progresso


summary(tbl)

ggplot(tbl) +
  geom_line(aes(x = Data, y = Tx_1ano, colour = "1 Ano")) +
  geom_line(aes(x = Data, y = Tx_5ano, colour = "5 Ano")) +
  geom_line(aes(x = Data, y = Tx_10ano, colour = "10 Ano")) +
  labs(title = "Taxa de Swap Pre-DI - 1 Ano", x = "Data", y = "Taxa (%)") +
  theme_minimal()

# Salva o banco de dados diário em CSV
cat("\nSalvando os dados diarios de swap Pre-Di\n")
readr::write_csv(tbl, "./Database/Swap_Pre_DI_diario.csv")


# Agrupamento mensal ------------------------------------------------------

cat("Agrupando mensalmente serie de swap Pre-Di\n")


tbl.m <- tbl %>% 
  mutate(year_month = lubridate::floor_date(Data, "month")) %>%
  group_by(year_month) %>%
  summarise(
    Tx_1ano = custom.mean(Tx_1ano),
    Tx_2ano = custom.mean(Tx_2ano),
    Tx_3ano = custom.mean(Tx_3ano),
    Tx_4ano = custom.mean(Tx_4ano),
    Tx_5ano = custom.mean(Tx_5ano),
    Tx_10ano = custom.mean(Tx_10ano), .groups = "drop"
  )

ggplot(tbl.m) +
  geom_line(aes(x = year_month, y = Tx_1ano, colour = "1 Ano")) +
  geom_line(aes(x = year_month, y = Tx_5ano, colour = "5 Ano")) +
  geom_line(aes(x = year_month, y = Tx_10ano, colour = "10 Ano")) +
  labs(title = "Taxa de Swap Pre-DI - Mensal", x = "Data", y = "Taxa (%)") +
  theme_minimal()


tbl.m %>%
  readr::write_csv("./Database/Swap_Pre_DI_mensal.csv")

cat(sprintf("[%s] Fim de Script\n", Sys.time()))

