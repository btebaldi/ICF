#' Fun??o que testa a raiz unitaria para variaveis, de uma base de dados
#'
#' @param dados Base de dados a ser analizada
#' @param colunas Colunas a serem analizadas 
#' @param critical valor critico da analise
#' @param InfoCriteria Criterio de selecao das variaveis 
#' @param N.lags Numero de lags do ADF
#'
#' @return Tabela informando se existe ou nao raiz unitaria
#' @export 
#'
#' @examples Testa.RaizUnitaria(data, c("col1", "col2"))
browser()
Testa.RaizUnitaria <- function(dados,
                               colunas=NULL,
                               critical = c("5pct", "1pct", "10pct"),
                               InfoCriteria = c("Fixed", "BIC", "AIC"),
                               N.lags = 1){
  
  
  critical <- match.arg(critical, choices = c("5pct", "1pct", "10pct"))  # forces the value to match one of the options
  InfoCriteria = match.arg(InfoCriteria, choices = c("Fixed", "BIC", "AIC"))
  
  
  # switch(method,
  #        mean = mean(c(1, 2, 3)),
  #        median = median(c(1, 2, 3)),
  #        trimmed = mean(c(1, 2, 3), trim = 0.2))
  
  
  # # Load arguments
  # if (missing(critical) | length(critical)>1) {
  #   print("SELECIONADO 5pct")
  #   critical <- critical[1]
  # }
  p.critical <- c("5pct" = 0.05, "1pct"=0.01, "10pct"=0.1)
  
  
  if(is.null(colunas)){
    colunas <- colnames(dados)
  }
  
  # biblioteca requerida
  require(urca)
  
  dados.ret <- data.frame(variavel=colunas,
                          Critical.Value = critical,
                          Unit.Root = as.logical(NA),
                          Criterio = as.numeric(NA),
                          trend.tau3 = as.numeric(NA),
                          trend.phi3 = as.numeric(NA),
                          trend.phi2 = as.numeric(NA),
                          drift.tau2 = as.numeric(NA),
                          drift.phi1 = as.numeric(NA),
                          none.tau1 = as.numeric(NA)
  )
  
  for(i in 1:nrow(dados.ret)){
    # Retira os dados da tabela
    x <- dados[[dados.ret$variavel[i]]]
    
    # se existe NA na serie, considero como erro zerado.
    if(sum(is.na(x)) > 0){
      cat(sprintf("\tSerie %d contem NA\n", i));
      stop("Encontrado NA")
    }
    
    if(!is.numeric(x)) {
      cat(sprintf("\tSerie %s nao e numerica. Skip!\n", colunas[i]));
      next()
    }
    
    if(sum(abs(x - mean(x))) == 0) {
      cat(sprintf("\tSerie %s nao tem variacao. Skip!\n", colunas[i]));
      next()
    }
    
    # Estima os modelos com tendencia, drift, e none
    ADF.none <- ur.df(x, type = "none", lags = N.lags, selectlags = InfoCriteria)
    ADF.drift <- ur.df(x, type = "drift", lags = N.lags, selectlags = InfoCriteria)
    ADF.trend <- ur.df(x, type = "trend", lags = N.lags, selectlags = InfoCriteria)
    
    # Guarda as estatisiticas
    dados.ret[i, c("trend.tau3", "trend.phi3", "trend.phi2")] <- ADF.trend@teststat["statistic", c("tau3", "phi3", "phi2")]
    dados.ret[i, c("drift.tau2", "drift.phi1")] <- ADF.drift@teststat["statistic", c("tau2", "phi1")]
    dados.ret[i, c("none.tau1")] <- ADF.none@teststat["statistic", c("tau1")]
    
    if(ADF.trend@teststat["statistic", "tau3"] < ADF.trend@cval["tau3", critical]){
      # se temos rejeicao da nula entao nao tem os unit root
      dados.ret[i, "Unit.Root"] <- FALSE
      dados.ret[i, "Criterio"] <- 1
    } else {
      if(ADF.trend@teststat["statistic", "phi3"] < ADF.trend@cval["phi3", critical]) {
        # aqui a trend nao ? significante, entao devemos estimar o modelo drift
        
        if(ADF.drift@teststat["statistic", "tau2"] < ADF.drift@cval["tau2", critical]){
          #  temos rejeicao da nula e com isso nao temos raiz unit?ria
          dados.ret[i, "Unit.Root"] <- FALSE
          dados.ret[i, "Criterio"] <- 2
        } else {
          # temos de testar a presenca de drift nao significante
          if(ADF.drift@teststat["statistic", "phi1"] < ADF.drift@cval["phi1", critical]){
            # aqui nao temos drift, temos de estimar o modelo sem nada
            if(ADF.none@teststat["statistic", "tau1"] < ADF.none@cval["tau1", critical]){
              #  temos rejeicao da nula e com isso nao temos raiz unitaria
              dados.ret[i, "Unit.Root"] <- FALSE
              dados.ret[i, "Criterio"] <- 3
            } else {
              dados.ret[i, "Unit.Root"] <- TRUE
              dados.ret[i, "Criterio"] <- 4
            }
          } else {
            # temos presenca de drift. Realizamos o teste utilizando a distribuicao normal.
            if(ADF.drift@teststat["statistic", "tau2"] < qnorm(p=p.critical[critical])){
              # o coeficiente de y_t-1 ? diferente de zero, logo nao tem unit root
              dados.ret[i, "Unit.Root"] <- FALSE
              dados.ret[i, "Criterio"] <- 5
            } else {
              dados.ret[i, "Unit.Root"] <- TRUE
              dados.ret[i, "Criterio"] <- 6
            }
          }
        }
      } else {
        # aqui a trend ? significante, entao devemos estimar gamma com distrib normal
        if(ADF.trend@teststat["statistic", "tau3"] < qnorm(p=p.critical[critical])){
          dados.ret[i, "Unit.Root"] <- FALSE
          dados.ret[i, "Criterio"] <- 7
        }else{
          dados.ret[i, "Unit.Root"] <- TRUE  
          dados.ret[i, "Criterio"] <- 8
        }
      }
    }
    
    # Remove the models and variable x to avoid trash
    rm(list = c("x", "ADF.none", "ADF.drift", "ADF.trend"))
  }
  
  # Retorno dos dados
  return(dados.ret)
}
