# Processo de construcao do banco de dados de Swap Pre-Di que foram baixados da
# bovespa. Aquivos construidos a partir dos arquivos dados diários
#
# Autor: Bruno Tebaldi Barbosa
#
# Data: 2021-05-20
# Revisao: 2025-10-17

# Setup -------------------------------------------------------------------

# Clear the workspace to remove all existing objects
rm(list = ls())

# Load required packages
library(tidyverse)

# Load parallel processing packages
library(future)
library(furrr)


Force_Reconstruction = FALSE


# Data Load --------------------------------------------

# List all CSV files in the specified directory, returning full file paths
mlistFiles <- list.files(
  path = "./Database/SwapDiPre_daily_raw", # Directory containing raw data files
  pattern = "*.csv",                       # Only files ending with .csv
  full.names = TRUE                        # Return full file paths
)

# Data Processing --------------------------------------------

# Get the total number of files for progress tracking
total_files <- length(mlistFiles)

# Loop over each file in the list
i <- 1

# Set up parallel plan (use available cores minus one)
future::plan(multisession, workers = future::availableCores() - 1)




# Leitura dos dados ja processados
if(!Force_Reconstruction){
  banco_atual <- readr::read_rds("./Database/SwapDiPre_daily.rds")
  Matrix_dates <- str_match(string = basename(mlistFiles),
                            pattern = "(?<=swapdi_)\\d{4}-\\d{2}-\\d{2}(?=\\.csv)")
  
  mlistFiles <- mlistFiles[!(as.Date(Matrix_dates[,1]) %in% banco_atual$Data)]
}

# function to read CSV Files
openFile <- function(currentFile) {
  # Print progress message (will appear after all processing due to parallelism)
  cat("Processing file:", basename(currentFile), "\n")
  
  # check if the file exists in the database
  
  return(readr::read_csv(currentFile, show_col_types = FALSE))
}


# Read all CSV files in parallel and combine them
SwapDiPre_New <- furrr::future_map_dfr( 
  mlistFiles,
  openFile)

# Clean up parallel workers
plan(sequential)


SwapDiPre_New %>%  summary()

if(!Force_Reconstruction){
  banco_atual <- readr::read_rds("./Database/SwapDiPre_daily.rds")
  SwapDiPre_New <- dplyr::bind_rows(banco_atual, SwapDiPre_New)
  
  # sanity check. If duplicated rows, stop
  sanityCheck <- SwapDiPre_New %>%
    count(Data, DiasCorridos) %>%
    filter(n >1) %>% nrow()
  if(sanityCheck > 0){
    stop("DUPLICATED DATA")
  }
}

SwapDiPre_New %>%
  arrange(Data, DiasCorridos) %>% 
  saveRDS(file = "./Database/SwapDiPre_daily.rds")











