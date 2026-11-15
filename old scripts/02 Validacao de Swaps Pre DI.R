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
library(dplyr)
library(ggplot2)
library(corrplot)


# Data Load ---------------------------------------------------------------

# Loads the database with Swap Pre-DI data on a monthly basis
tbl.m <- readr::read_csv("./Database/Swap_Pre_DI_mensal.csv")

# load variable data from Bloomberg
tbl.colnames <- readxl::read_excel("Database/BBG FCI Data_asValues.xlsx",
                                   sheet = "Dados",
                                   # range = "A4:AN4",
                                   na = "#N/A N/A")

tbl.ICF <- readxl::read_excel("Database/BBG FCI Data_asValues.xlsx",
                              sheet = "Mensal_BBG",
                              range = "A4:AL273",
                              na = c("#N/A N/A", "PX_LAST", "Dates") )


# Adjust column names
for(col in colnames(tbl.ICF)){
  
  idx <- which(tbl.colnames$Bloomberg == col)
  
  if(length(idx) > 1){ stop("Erro. Mais de uma coluna com esse nome!")}
  
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



# Comparacao Grafica do Swap Pre-Di ---------------------------------------

relacoesSwapPreDi = c("Tx_1ano" = "BCSFLPDV_BLP_Curncy",
                      "Tx_2ano" = "BCSFPPDV_BLP_Curncy",
                      "Tx_3ano" = "BCSFQPDV_BLP_Curncy",
                      "Tx_5ano" = "BCSFSPDV_BLP_Curncy",
                      "Tx_10ano" = "BCSFXPDV_BLP_Curncy")

for(i in seq_along(relacoesSwapPreDi)){
  col1 <- names(relacoesSwapPreDi)[i]
  col2 <- as.character(relacoesSwapPreDi[i])
  
  g1 <- tbl %>% 
    dplyr::select(Date2, 
                  Serie1 = !!col1,
                  Serie2 = !!col2) %>% 
    ggplot() + 
    geom_line(aes(x = Date2, y = Serie1 *100, colour = "calculada")) +
    geom_line(aes(x = Date2, y = Serie2, colour = "Bloomberg")) + 
    labs(title = sprintf("%s vs %s", col1, col2))
  
  print(g1)
}




# Comparacao por correlacao -----------------------------------------------

# Colunas do Swap pre-di
colSwapPreDiBloomberg <- c(
  "BCSFXPDV_BLP_Curncy",  # BRL SWAP PRE-DI (2520d) = 10 anos
  "BCSFTPDV_CMPN_Curncy", # BRL SWAP PRE-DI (1512d) =  6 anos
  "BCSFSPDV_BLP_Curncy",  # BRL SWAP PRE-DI (1260d) =  5 anos
  "BCSFQPDV_BLP_Curncy",  # BRL SWAP PRE-DI (756d)  =  3 anos
  "BCSFPPDV_BLP_Curncy",  # BRL SWAP PRE-DI (504d)  =  2 anos
  "BCSFLPDV_BLP_Curncy"  # BRL SWAP PRE-DI (252d)   =  1 anos
)

for(i in c("Tx_1ano",
           "Tx_2ano",
           "Tx_3ano",
           "Tx_5ano",
           "Tx_10ano")){
  
  tbl |> 
    dplyr::select(!!i, colSwapPreDiBloomberg) |>
    cor(use = "pairwise.complete.obs") |>
    corrplot(method = "color",
             addCoef.col = "white",
             # mar = c(0, 0, 0, 0),
             title = sprintf("%s", i)) |>
    print()
  
}









