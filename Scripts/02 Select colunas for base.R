# Single base construction to be analyzed in the ICF
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2026-08-08

# script fara a selecao das colunas que vao ser utilizadas no estudo e cria uma
# base unica.


# Data Load ---------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# load variable data from Bloomberg
tbl.colnames <- readxl::read_excel("Database/BBG FCI Data_asValues.xlsx",
                                   sheet = "Dados",
                                   # range = "A4:AN4",
                                   na = "#N/A N/A")

tbl.ICF <- readxl::read_excel("Database/BBG FCI Data_asValues.xlsx",
                              sheet = "Mensal_BBG",
                              range = "A4:AQ281",
                              na = c("#N/A N/A", "PX_LAST", "Dates") )


# Adjust column names
for(col in colnames(tbl.ICF)){
  
  idx <- which(tbl.colnames$Bloomberg == col)
  
  if(length(idx) > 1){ stop("Erro. Mais de uma coluna com esse nome!")}
  
  if(length(idx) > 0){
    colnames(tbl.ICF)[colnames(tbl.ICF) == col] <- tbl.colnames$ColCode[idx]
  }
}

rm(list = base::setdiff(ls(), c("tbl.ICF") ) )

# Database merge ----------------------------------------------------------

tbl.ICF$Date2 <- as.Date(tbl.ICF$Date)

day(tbl.ICF$Date2) <- 1

ColunasSelecionadas <-   c("Date"="Date2",
                           # Grupo de Juros Internacional
                           "Bond_USA_3M" = "USGG3M_Index", 
                           "Bond_GRB_3M" = "GTGBP3MO_Corp",
                           "Bond_DEM_3M" = "GTDEM3M_Govt",
                           "Bond_JAP_3M" = "GTJPY3MO_Corp",
                           "Bond_USA_2Y" = "USGG2YR_Index",
                           "Bond_GRB_2Y" = "GTGBP2YR_Corp", 
                           "Bond_DEM_2Y" = "GTDEM2Y_Govt", 
                           "Bond_JAP_2Y" = "GTJPY2YR_Corp",
                           "Bond_USA_10Y" = "USGG10YR_Index",
                           "Bond_GRB_10Y" = "GTGBP10YR_Corp",
                           "Bond_DEM_10Y" = "GTDEM10YR_Corp",
                           "Bond_JAP_10Y" = "GTJPY10YR_Corp",
                           # Grupo de Risco
                           "VIX" = "VIX_Index",
                           "BRA_CDS_5Y" = "BRAZIL_CDS_USD_SR_5Y_D14_Corp",
                           # Grupo Moedas
                           "DXY" = "DXY_Curncy",
                           "DXY_EME" = "USTWEME_Index",
                           "FX_BRL_USD" = "BRL_REGN_Curncy",
                           "FX_EUR_USD" = "EUR_BGN_Curncy",
                           "FX_JPY_USD" = "JPY_BGN_Curncy",
                           "FX_GBP_USD" = "GBP_BGN_Curncy",
                           "FX_CAD_USD" = "CAD_BGN_Curncy",
                           "FX_SEK_USD" = "SEK_BGN_Curncy",
                           "FX_CHF_USD" = "CHF_BGN_Curncy",
                           # Grupo Petroleo 
                           "WTI" = "CL1_COMB_Comdty",
                           "Brent" = "CO1_Comdty",
                           # Grupo Commoditires
                           "CRB_FOOD" = "CRB_FOOD_Index",
                           "CRB_METL" = "CRB_METL_Index",
                           # Mercado de capitais
                           "MSCI_Desenvolvidos" = "MXWO_Index",
                           "MSCI_Emergentes" = "MXEF_Index",
                           "IBOV" = "IBOV_Index",
                           # Grupo Juros Brazil 
                          "SWAP_10Y" = "BCSFXPDV_BLP_Curncy",  # BRL SWAP PRE-DI (2520d) = 10 anos
                          "SWAP_6Y" = "BCSFTPDV_CMPN_Curncy", # BRL SWAP PRE-DI (1512d) =  6 anos
                          "SWAP_5Y" = "BCSFSPDV_BLP_Curncy",  # BRL SWAP PRE-DI (1260d) =  5 anos
                          "SWAP_3Y" = "BCSFQPDV_BLP_Curncy",  # BRL SWAP PRE-DI (756d)  =  3 anos
                          "SWAP_2Y" = "BCSFPPDV_BLP_Curncy",  # BRL SWAP PRE-DI (504d)  =  2 anos
                          "SWAP_1Y" = "BCSFLPDV_BLP_Curncy",  # BRL SWAP PRE-DI (252d)   =  1 anos
                           # Grupo Produto,
                          "IBCBR_SA" = "BZEASA_Index",   # IBCBR (SA)
                          "IBCBR_NSA" = "BZEANSA_Index",  # IBCBR (NSA)
                          "PIM_NSA" = "BZIPTLYO_Index", # Producao Industrial Bens de Capital (NSA 2012=100)
                          "PIM_SA" = "BZIPTLSA_Index"  # Producao Industrial Bens de Capital (SA 2012=100)
)

# Seleciono as colunasque vamos trabalhar.
tbl <- tbl.ICF |> 
  dplyr::select(all_of(ColunasSelecionadas)) |> 
  slice(-1)

# Retiro a primeira linha pois ela nao esta na base de dados (problema da
# Bloomberg)
tbl |>
  summary()


tbl |>
  writexl::write_xlsx(path = "./Database/FCI Selected Data.xlsx")
