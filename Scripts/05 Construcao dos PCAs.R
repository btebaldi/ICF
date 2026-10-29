# Single base construction to be analyzed in the ICF
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2021-05-20
# Revisao: 2025-06-12

# Setup -------------------------------------------------------------------
# Cleans up the workspace to avoid conflicts from old variables
rm(list = ls())

library(tidyverse)
library(readxl)


# User defined functions --------------------------------------------------

source("./Scripts/CheckUnitRoot.R")


d.ln <- function(x, n.lag = 1){
  return( log(x) - log(dplyr::lag(x, n = n.lag)) )
}

d.nn <- function(x, n.lag = 1){
  return( x - dplyr::lag(x, n = n.lag) )
}

# Data Load ---------------------------------------------------------------

tbl <- readxl::read_xlsx(path = "./Database/FCI Selected Data.xlsx")

tbl$Date <-  as.Date(tbl$Date)


# Group setup -------------------------------------------------------------

# Lista tem a realcao de qual variavel esta em cada grupo.


m_groups <- list( 
  # Produto = c("IBCBR", "PIM"),
  Juros_exterior = c(
    "Bond_USA_3M", 
    # "Bond_GRB_3M", # serie nao muito boa
    "Bond_DEM_3M",
    "Bond_JAP_3M",
    "Bond_USA_2Y",
    "Bond_GRB_2Y", 
    "Bond_DEM_2Y", 
    "Bond_JAP_2Y",
    "Bond_USA_10Y",
    "Bond_GRB_10Y",
    "Bond_DEM_10Y",
    "Bond_JAP_10Y"),
  Risco = c("VIX",
            "BRA_CDS_5Y"),
  Juros_Brazil = c(# "SWAP_10Y",  # BRL SWAP PRE-DI (2520d) = 10 anos
    # "SWAP_6Y", # BRL SWAP PRE-DI (1512d) =  6 anos
    # "SWAP_5Y",  # BRL SWAP PRE-DI (1260d) =  5 anos
    # JUROS ACIMA DE 5 ANOS NAO PARECEM ESTAREM MUITO BOM 
    "SWAP_3Y",  # BRL SWAP PRE-DI (756d)  =  3 anos
    "SWAP_2Y",  # BRL SWAP PRE-DI (504d)  =  2 anos
    "SWAP_1Y"  # BRL SWAP PRE-DI (252d)   =  1 anos
  ),
  Commoditires = c("CRB_FOOD",
                   "CRB_METL"),
  Mercado_capitais = c("MSCI_Desenvolvidos",
                       "MSCI_Emergentes",
                       "IBOV"),
  Moedas = c("DXY",
    "DXY_EME",
    # "FX_EUR_USD",
    # "FX_JPY_USD",
    # "FX_GBP_USD",
    # "FX_CAD_USD",
    # "FX_SEK_USD",
    # "FX_CHF_USD",
    "FX_BRL_USD" ),
  Petroleo = c("WTI",
               "Brent") )




#   # Grupo Produto,
#   "IBCBR_SA" = "BZEASA_Index",   # IBCBR (SA)
#   "IBCBR_NSA" = "BZEANSA_Index",  # IBCBR (NSA)
#   "PIM_NSA" = "BZIPTLYO_Index", # Producao Industrial Bens de Capital (NSA 2012=100)
#   "PIM_SA" = "BZIPTLSA_Index"  # Producao Industrial Bens de Capital (SA 2012=100)
# )


Check.UnitRoot(tbl, N.lags = 4, InfoCriteria = "BIC", critical = "1pct")

tbl <- na.omit(tbl)



# Function ----------------------------------------------------------------

Extrair_PCA <- function(tbl, func = NULL){
  
  if(is.null(func)) {
    func = function(c){return(c)}
  }
  
  tbl.aux <- tbl |> 
    mutate_if(.predicate = is.numeric, .funs = func) |> 
    na.omit()
  
  g1 <- tbl.aux %>%
    mutate_if(.predicate = is.numeric, .funs = scale) |> 
    pivot_longer(cols = -Date) |>  
    ggplot() + geom_line(aes(x =Date, y = value)) + 
    facet_wrap(~name)
  
  print(g1)
  
  pca.aux <- tbl.aux |>
    dplyr::select(-Date )  |>
    prcomp(center = TRUE, scale. = TRUE)
  
  print(summary(pca.aux))
  
  tbl_return <- tbl.aux |>
    dplyr::select("Date")
  
  tbl_return$PC1 <- pca.aux$x[,"PC1"]
  
  return(tbl_return)
}



# Juros_exterior ----------------------------------------------------------

tbl_PCA_JE <- tbl |> 
  dplyr::select(c("Date", all_of(m_groups$Juros_exterior))) |>
  Extrair_PCA(func = NULL)

tbl_PCA_JE <- tbl_PCA_JE |> rename(JE_PC1 = PC1)

# Risco -------------------------------------------------------------------

tbl_PCA_RK <- tbl %>% 
  dplyr::select(c(m_groups$Risco, "Date")) %>%
  Extrair_PCA(func = NULL)

tbl_PCA_RK <- tbl_PCA_RK |> rename(RK_PC1 = PC1)


# Juros_Brazil ------------------------------------------------------------

tbl_PCA_JB <- tbl |> 
  dplyr::select(c(m_groups$Juros_Brazil, "Date")) |> 
  Extrair_PCA(func = NULL)

tbl_PCA_JB <- tbl_PCA_JB |> rename(JB_PC1 = PC1)

# Commoditires ------------------------------------------------------------

tbl_PCA_CM <- tbl |> 
  dplyr::select(c(m_groups$Commoditires, "Date")) |> 
  Extrair_PCA(func = d.ln)

tbl_PCA_CM <- tbl_PCA_CM |> rename(CM_PC1 = PC1)


# Mercado_capitais --------------------------------------------------------

tbl_PCA_MK <- tbl %>% 
  dplyr::select(c(m_groups$Mercado_capitais, "Date")) %>%
  Extrair_PCA(func = d.ln)

tbl_PCA_MK <- tbl_PCA_MK |> rename(MK_PC1 = PC1)

# Moedas ------------------------------------------------------------------

tbl_PCA_MM <- tbl %>% 
  dplyr::select(c(m_groups$Moedas, "Date")) %>%
  Extrair_PCA(func = d.ln)

tbl_PCA_MM <- tbl_PCA_MM |> rename(MM_PC1 = PC1)

# Petroleo ----------------------------------------------------------------

tbl_PCA_OL <- tbl %>% 
  dplyr::select(c(m_groups$Petroleo, "Date")) %>%
  Extrair_PCA(func = d.ln)

tbl_PCA_OL <- tbl_PCA_OL |> rename(OL_PC1 = PC1)

# Produto -----------------------------------------------------------------

nlags <- 1

tbl.YY <- tbl %>% 
  dplyr::select(c("IBCBR_SA", "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.ln, n.lag=nlags) %>%
  dplyr::slice(-(1:nlags)) %>%  
  mutate(DUM_202004 = if_else(condition = Date == as.Date("2020-04-01"),true = 1, false = 0),
         DUM_202003 = if_else(condition = Date == as.Date("2021-03-01"),true = 1, false = 0),
         DUM_201805 = if_else(condition = Date == as.Date("2018-05-01"),true = 1, false = 0),
         DUM_202302 = if_else(condition = Date == as.Date("2023-02-01"),true = 1, false = 0))


tbl.YX <- tbl.YY %>%
  dplyr::left_join(tbl_PCA_CM, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_JB, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_JE, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_MK, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_MM, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_OL, by = c("Date" = "Date")) %>% 
  dplyr::left_join(tbl_PCA_RK, by = c("Date" = "Date"))


tbl.YX |> 
  writexl::write_xlsx(path = "./Database/FCI PCAs Data.xlsx")

