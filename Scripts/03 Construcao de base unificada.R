# Single base construction to be analyzed in the ICF
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2021-05-20
# Revisao: 2025-06-12

# Setup -------------------------------------------------------------------
# Cleans up the workspace to avoid conflicts from old variables
rm(list = ls())

library(readxl)

# readxl::excel_sheets("Database/BBG FCI Data.xlsx")


# Data Load ---------------------------------------------------------------

# Loads the database with Swap Pre-DI data on a monthly basis
tbl.m <- readr::read_csv("./Database/Swap_Pre_DI_mensal.csv")

# load variable data from Bloomberg
tbl.colnames <- readxl::read_excel("Database/BBG FCI Data.xlsx",
                                   sheet = "Dados",
                                   # range = "A4:AN4",
                                   na = "#N/A N/A")

tbl.ICF <- readxl::read_excel("Database/BBG FCI Data.xlsx",
                              sheet = "Mensal_BBG",
                              range = "A4:AL273",
                              na = c("#N/A N/A", "PX_LAST", "Dates") )


# Adjust column names
for(col in colnames(tbl.ICF)){
  
  idx <- which(tbl.colnames$Bloomberg == col)
  
  if(length(idx) > 1){ stop("Erro")}
  
  if(length(idx) > 0){
    colnames(tbl.ICF)[colnames(tbl.ICF) == col] <- tbl.colnames$ColCode[idx]
  }
}

rm(list = base::setdiff(ls(), c("tbl.ICF", "tbl.m")) )

# Database merge ----------------------------------------------------------

tbl.ICF$Date2 <- as.Date(tbl.ICF$Date)

day(tbl.ICF$Date2) <- 1

tbl <- tbl.ICF %>% 
  left_join(tbl.m, by = c("Date2" = "year_month"))


tbl %>% 
  dplyr::select(-Date, -Date2) %>% 
  dplyr::select(starts_with("TX"), starts_with("BRL_SWP")) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot( hc.order = FALSE, lab = TRUE)



# Select columns to build the single base ---------------------------------

tbl <- tbl %>% 
  dplyr::select(
    c("Date"="Date2",
      "Bond_USA_3M", "Bond_GRB_3M", "Bond_DEM_3M", "Bond_JAP_3M", "Bond_USA_2Y",
      "Bond_GRB_2Y", "Bond_DEM_2Y", "Bond_JAP_2Y", "Bond_USA_10Y",
      "Bond_GRB_10Y", "Bond_DEM_10Y", "Bond_JAP_10Y",
      "VIX", "BRA_CDS_5Y",
      "DXY",
      "DXY_EME",
      "FX_BRL_USD",
      "WTI",
      "Brent",
      "CRB_FOOD",
      "CRB_METL",
      "MSCI_Dev_Index",
      "MSCI_Emerg_Index",
      "IBOV",
      # "BRL_SWP_PRE_DI_252",
      # "BRL_SWP_PRE_DI_1512",
      "IBCBR",
      # "BRL_SWP_PRE_DI_5Y",
      # "BRL_SWP_PRE_DI_2Y",
      # "BRL_SWP_PRE_DI_1Y",
      "FX_USD_EUR",
      "FX_JPY_USD",
      "FX_USD_GBP",
      "FX_CAD_USD",
      "FX_SEK_USD",
      "FX_CHF_USD",
      "Tx_1ano",
      "Tx_2ano",
      "Tx_3ano",
      "Tx_4ano",
      "Tx_5ano",
      "Tx_10ano") )

summary(tbl)


tbl[-1,] %>% 
  writexl::write_xlsx(path = "./Database/FCI Final Data.xlsx")







