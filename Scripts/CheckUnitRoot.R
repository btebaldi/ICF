#' Teste ADF (ur.df/urca) para raiz unitária em múltiplas séries
#'
#' Executa a bateria padrão do ADF com três especificações ("none", "drift", "trend")
#' e aplica uma árvore de decisão simples com base em testes tau/phi e significância
#' da tendência/constante, retornando uma tabela resumida.
#'
#' @param dados data.frame ou tibble com as séries em colunas.
#' @param colunas character (opcional). Quais colunas testar. Default: todas as colunas numéricas.
#' @param critical character. Nível crítico usado nas tabelas do `urca`: "1pct","5pct","10pct".
#' @param InfoCriteria character. Critério de seleção de defasagens para `ur.df`:
#'   "Fixed", "AIC" ou "BIC". (Passado para `selectlags`.)
#' @param N.lags integer. Número máximo de defasagens (ou fixo se InfoCriteria="Fixed").
#' @param verbose logical. Imprimir mensagens de progresso/avisos por série?
#'
#' @return data.frame with: variavel, unit_root (TRUE/FALSE), criterion (1..8),
#'   spec_chosen, n_effective, stats_* and cval_*.
#' @export
#'
#' @examples
#' # Exemplo simples:
#' # library(urca)
#' # set.seed(1); y <- cumsum(rnorm(200))
#' # df <- data.frame(y = y, dy = diff(c(NA,y)))
#' # Testa.RaizUnitaria(df, c("y","dy"), critical = "5pct", InfoCriteria="AIC", N.lags=8)
Check.UnitRoot <- function(data,
                           columns = NULL,
                           critical = c("5pct", "1pct", "10pct"),
                           InfoCriteria = c("Fixed", "BIC", "AIC"),
                           N.lags = 1,
                           verbose = TRUE) {
  
  ## --------- Validações e preparação ----------
  critical     <- match.arg(critical, c("5pct", "1pct", "10pct"))
  InfoCriteria <- match.arg(InfoCriteria, c("Fixed", "BIC", "AIC"))
  
  p.critical <- c(0.05, 0.01, 0.10)[which(critical == c("5pct", "1pct", "10pct"))]
  
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("Pacote 'urca' não está instalado. Instale com install.packages('urca').")
  }
  
  if (!is.data.frame(data)) {
    stop("'dados' deve ser um data.frame/tibble com séries nas colunas.")
  }
  
  # Se colunas não for informado, pega todas as numéricas
  if( is.null(columns) ) {
    columns <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(columns) == 0L){
      stop("Nenhuma coluna numérica encontrada em 'dados'.") 
    }
  } else {
    missing_col <- setdiff(columns, names(data))
    if (length(missing_col) > 0){
      stop("Columns not found in 'data': ", paste(faltantes, collapse = ", "))
    }    
  }
  
  # Internal helper: runs ur.df safely
  # run_urdf <- function(x, type, lags, selectlags) {
  #   urca::ur.df(y = x, type = type, lags = lags, selectlags = selectlags)
  # }
  
  # Helper: extracts statistics and critics from the ur.df object 
  # Note: in urca::ur.df, @teststat is a named vector; @cval is a matrix with
  # rows ("tau1","tau2","tau3","phi1","phi2","phi3")
  get_stat <- function(obj, name){ 
    idx <- which(colnames(obj@teststat) == name)
    return(as.numeric(obj@teststat[idx]))
  }
  
  get_cval <- function(obj, name, crit){ 
    c_idx <- which(colnames(obj@cval) == crit)
    r_idx <- which(rownames(obj@cval) == name)
    return( as.numeric(obj@cval[r_idx, c_idx]) )
  }
  
  get_modelLags <- function(obj){ 
    return( sum(grepl(pattern = "z.diff.lag", x = rownames(obj@testreg$coefficients) )) )
  }
  
  
  # Initial result
  out <- data.frame(
    variavel       = columns,
    unit_root      = NA,          # TRUE se há raiz unitária (não rejeita H0)
    criterion      = NA_integer_, # índice do caminho na árvore (1..8)
    spec_chosen    = NA_character_,
    n_effective    = NA_integer_,
    trend.tau3     = NA_real_,
    trend.phi3     = NA_real_,
    trend.phi2     = NA_real_,
    drift.tau2     = NA_real_,
    drift.phi1     = NA_real_,
    none.tau1      = NA_real_,
    cval.tau3      = NA_real_,
    cval.phi3      = NA_real_,
    cval.phi2      = NA_real_,
    cval.tau2      = NA_real_,
    cval.phi1      = NA_real_,
    cval.tau1      = NA_real_,
    trend.lags     = NA_integer_,
    drift.lags     = NA_integer_,
    none.lags      = NA_integer_,
    stringsAsFactors = FALSE )
  
  ## --------- Loop through variables ----------
  for (i in seq_along(columns)) {
    nm <- columns[i]
    x  <- data[[nm]]
    
    # Basic checks
    if (!is.numeric(x)) {
      if (verbose) {
        message(sprintf("Series '%s' is not numeric. Ignoring.", nm))
      }
      next()
    }
    
    # Remove NAs with warning
    nas <- sum(is.na(x))
    if (nas > 0L) {
      if (verbose) {
        message(sprintf("Series '%s' contains %d NA(s). Applying na.omit().", nm, nas))
      }
      x <- stats::na.omit(x)
    }
    
    n_eff <- length(x)
    out$n_effective[i] <- n_eff
    
    if (n_eff < (N.lags + 10L)) {
      if (verbose) {
        message(sprintf("Series '%s' too short (n=%d) for N.lags=%d. Ignoring.", nm, n_eff, N.lags))
      }
      next()
    }
    
    # Constant series?
    if (all(abs(x - mean(x)) < .Machine$double.eps^0.5)) {
      if (verbose) {
        message(sprintf("Series '%s' without variation (constant). Ignoring.", nm))
      }
      next()
    }
    
    # Estima modelos: none, drift, trend
    ADF.none  <- urca::ur.df(y = x, type = "none",  lags = N.lags, selectlags = InfoCriteria)
    ADF.drift <- urca::ur.df(y = x, type = "drift", lags = N.lags, selectlags = InfoCriteria)
    ADF.trend <- urca::ur.df(y = x, type = "trend", lags = N.lags, selectlags = InfoCriteria)
    
    out$trend.lags[i] <- get_modelLags(ADF.trend)
    out$drift.lags[i] <- get_modelLags(ADF.drift)
    out$none.lags[i]  <- get_modelLags(ADF.none)
    
    # Estatísticas tau/phi
    tt_tau3 <- get_stat(ADF.trend, "tau3")
    tt_phi3 <- get_stat(ADF.trend, "phi3")
    tt_phi2 <- get_stat(ADF.trend, "phi2")
    dd_tau2 <- get_stat(ADF.drift, "tau2")
    dd_phi1 <- get_stat(ADF.drift, "phi1")
    nn_tau1 <- get_stat(ADF.none,  "tau1")
    
    # Critical values
    cv_tau3 <- get_cval(ADF.trend, "tau3", critical)
    cv_phi3 <- get_cval(ADF.trend, "phi3", critical)
    cv_phi2 <- get_cval(ADF.trend, "phi2", critical)
    cv_tau2 <- get_cval(ADF.drift, "tau2", critical)
    cv_phi1 <- get_cval(ADF.drift, "phi1", critical)
    cv_tau1 <- get_cval(ADF.none,  "tau1", critical)
    
    # Save output critical values and statistics
    out[i, c("trend.tau3","trend.phi3","trend.phi2",
             "drift.tau2","drift.phi1","none.tau1")] <- c(tt_tau3, tt_phi3, tt_phi2, dd_tau2, dd_phi1, nn_tau1)
    
    out[i, c("cval.tau3","cval.phi3","cval.phi2",
             "cval.tau2","cval.phi1","cval.tau1")] <- c(cv_tau3, cv_phi3, cv_phi2, cv_tau2, cv_phi1, cv_tau1)
    
    ## --------- Decision tree (light version) ----------
    # Rule 1: Test with bias (tau3)
    if (tt_tau3 < cv_tau3) {
      # Rejects H0 of unit root with trend -> stationary (no UR)
      out$unit_root[i]      <- FALSE
      out$criterion[i]       <- 1L
      out$spec_chosen[i] <- "No UR; +trend; +drift"
    }
    else {
      # Trend does not reject UR.
      # Check trend significance via phi3
      if (tt_phi3 < cv_phi3) {
        # Trend is NOT significant we must use DRIFT model
        # Rule 2: drift (tau2)
        if (dd_tau2 < cv_tau2) {
          # Rejects H0 of unit root with drift -> stationary (no UR)
          out$unit_root[i]     <- FALSE
          out$criterion[i]     <- 2L
          out$spec_chosen[i] <- "No UR; No trend; +drift"
        }
        else {
          # Fail to reject with tau2: check if drift is non-significant (phi1)
          if (dd_phi1 < cv_phi1) {
            # Drift is NOT significant we must use NONE model
            if (nn_tau1 < cv_tau1) {
              # Rejects H0 of unit root with none -> stationary (no UR)
              out$unit_root[i]      <- FALSE
              out$criterion[i]      <- 3L
              out$spec_chosen[i] <- "No UR; No Drift; No Trend"
            } else {
              # Accept H0 of unit root with none
              out$unit_root[i]      <- TRUE
              out$criterion[i]       <- 4L
              out$spec_chosen[i] <- "+UR; No Drift; No Trend"
            }
          } 
          else {
            # Drift present but tau2 did not reject UR
            
            # we have the presence of drift. We performed the test using the
            # normal distribution.
            if(dd_tau2 < qnorm(p=p.critical)){
              # The coefficient of y_{t-1} is different from zero, so it has no unit root
              out$unit_root[i]    <- FALSE
              out$criterion[i]    <- 5L
              out$spec_chosen[i]  <- "No UR; No trend; +drift"
            }
            else {
              out$unit_root[i]    <- TRUE
              out$criterion[i]    <- 6L
              out$spec_chosen[i]  <- "+UR; No trend; +drift"
            }
          }
        }
      } 
      else {
        # Tendência é significativa (phi3 não rejeita ausência de trend) -> manter TREND
        # Mas tau3 não rejeitou RU: mantém RU
        
        # TREND But tau3 did not reject UR
        # Trend is significant (phi3 does not reject absence of trend)
        # we must estimate gamma with normal distribution
        if(tt_tau3 < qnorm(p=p.critical)){
          out$unit_root[i]      <- FALSE
          out$criterion[i]       <- 7L
          out$spec_chosen[i] <- "No UR;  +trend; +drift"
        }
        else{
          out$unit_root[i]      <- TRUE
          out$criterion[i]       <- 8L
          out$spec_chosen[i] <- "+UR; +trend; +drift"
        }
      }
    }
  }
  
  return(out)
}
