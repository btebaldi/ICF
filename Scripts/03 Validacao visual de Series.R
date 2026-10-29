# Single base construction to be analyzed in the ICF
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2026-08-08

# script para validacao (visial) das series.

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


# Validacao Grafica -------------------------------------------------------


for(i in seq_along(tbl.ICF)){
  
  col <- colnames(tbl.ICF)[i]
  
  if(col == "Date"){
    next
  }
  
   
  g1 <- tbl.ICF %>%
    dplyr::select(Date,
                  Serie1 = !!col) %>%
    ggplot() +
    geom_line(aes(x = Date, y = Serie1)) +
    labs(title = sprintf("%s", col))

   print(g1)
}
