# Funcoa que faz web scrapping dos dados de Swap Pre-Di da bovespa/B3
# 
# Autor: Bruno Tebaldi Barbosa
# Data: 2021-05-19

library(rvest)
library(dplyr)
library(lubridate)
library(progress)


Download.B3.SwapDi <- function(date, verbose=TRUE){
  
  # Site de busca
  URL.mask <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp?Data=%s&Data1=20210514&slcTaxa=PRE"
               
  # Busca qual a data a ser processada
  date <- as.Date(date)
  
  # variavel de retorno dos dados
  ret <- as.data.frame(NULL)
  
  if((wday(date) == 7) | (wday(date) == 1)){
    #  data é um fim de semana. Não tento nem processar
    if(verbose){
      cat(sprintf("\nFim de semana - data: %s\n", date))
    }
  } else{
    #  Determina a URL
    url.string <- sprintf(URL.mask, format(date, "%d/%m/%Y")  ) 
    
    # Faz leitura da URL
    html <-read_html(url.string)
    
    # Busca as informacoes das tabelas.
    td.nodes <- html %>%
      html_nodes(xpath='//table[@id="tb_principal1"]/td')
    
    # Leio os textos nas divisoes das tabelas
    td.text <- td.nodes %>% html_text()
    
    if (length(td.text) == 0){
      # Pode ser um feriado entao informo que nao encontrei dados.
      if(verbose){
        cat(sprintf("\nNão encontrado nenhum dado para a data: %s\n", date))
      }
    } else {
      
      # Comeco o processamento
      cat(sprintf("\nProcessando a data: %s\n", date))
      
      #  tabela que vai guardar as informacoes destes dados.
      tbl <- tibble(Data = rep(date, length(td.text)/3),
                    DiasCorridos = as.integer(NA),
                    TX_252 = as.numeric(NA),
                    TX_360 = as.numeric(NA))
      
      # Barra de progresso do sistema
      pb <- progress_bar$new(total = length(td.text))
      
      # Processamento: Para cada elemento converte o mesmo em numero e guarda na tabela.
      for (i in 1:length(td.text)) {
        mod <- i %% 3
        linha <- ceiling(i/3)
        if(mod == 0) {col <- 3} else {col <- mod}
        
        tbl[linha, col+1] <- parse_number(td.text[i], na = c("", "NA"), locale = locale(decimal_mark = ",", grouping_mark = "."))
        
        # incrementa a barra de progresso
        pb$tick()
      }
      
      # Informa que terminou de processar a data
      pb$terminate()
      
      ret <- tbl
    }
    
  }
  
  return(ret)
}

