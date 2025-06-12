# Clear the workspace to remove all existing objects
rm(list = ls())

# Load required packages
library(tidyverse)

# Load parallel processing packages
library(future)
library(furrr)

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
plan(multisession, workers = availableCores() - 1)

# Read all CSV files in parallel and combine them
SwapDiPre_Full <- future_map_dfr(
    mlistFiles,
    function(currentFile) {
        # Print progress message (will appear after all processing due to parallelism)
        cat("Processing file:", basename(currentFile), "\n")
        readr::read_csv(currentFile, show_col_types = FALSE)
    }
)

# Clean up parallel workers
plan(sequential)


SwapDiPre_Full %>%  summary()

SwapDiPre_Full %>%
 arrange(Data, DiasCorridos) %>% 
 saveRDS(file = "./Database/SwapDiPre_daily.rds")
