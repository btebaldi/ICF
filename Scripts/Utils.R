library(dplyr)
library(tidyr)


get_mode <- function(x) {
  # get non NA items
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0)
  {
    return(NA_character_)
  }
  
  ux <- unique(x_clean)
  tab <- tabulate(match(x_clean, ux))
  
  idx_max_tab <- which.max(tab)
  if(sum(tab == tab[idx_max_tab]) > 1){
    warning("Multiple Mode found. Returning the first one found.")
  }
  
  # If there is a tie, returns the first mode encountered
  return( ux[which.max(tab)] )
  
}


single_summary <- function(df, col_name) {
  x <- df[[col_name]]
  n_total <- length(x)
  n_na    <- sum(is.na(x))
  n_valid <- n_total - n_na
  
  # Default empty template (ensures consistent data types across all rows)
  out <- data.frame(
    Variable = col_name,
    Type     = class(x)[1],
    n        = n_total,
    Min      = NA_real_,
    Q1       = NA_real_,
    Median   = NA_real_,
    Mean     = NA_real_,
    Mode     = get_mode(x),
    Q3       = NA_real_,
    Max      = NA_real_,
    SD       = NA_real_,
    NAs      = n_na,
    stringsAsFactors = FALSE
  )
  
  # 1. Numeric & Logical columns (logicals treat TRUE=1, FALSE=0)
  if ((is.numeric(x) || is.logical(x)) && n_valid > 0) {
    x_num <- as.numeric(x)
    out$Min    <- min(x_num, na.rm = TRUE)
    out$Q1     <- as.numeric(quantile(x_num, 0.25, na.rm = TRUE))
    out$Median <- median(x_num, na.rm = TRUE)
    out$Mean   <- mean(x_num, na.rm = TRUE)
    out$Q3     <- as.numeric(quantile(x_num, 0.75, na.rm = TRUE))
    out$Max    <- max(x_num, na.rm = TRUE)
    out$SD     <- sd(x_num, na.rm = TRUE) # Returns NA if n_valid == 1
  }
  
  # 2. Date & Date-Time columns (SD is calculated in days/seconds)
  if (inherits(x, c("Date", "POSIXt")) && n_valid > 0) {
    # x_num <- as.numeric(x)
    out$Min  <- as.character(min(x, na.rm = TRUE))
    out$Q1     <- as.character(NA)
    out$Median <- as.character(median(x, na.rm = TRUE))
    out$Mean   <- as.character(mean(x, na.rm = TRUE))
    out$Q3     <- as.character(NA)
    out$Max    <- as.character(max(x, na.rm = TRUE))
    out$SD     <- as.character(NA)
  }
  
  # 3. Factor & Character columns:
  # Min, Q1, Median, Mean, Q3, Max, SD remain NA; n, Mode, and NAs are populated.
  
  return(out)
}


df_summary <- function(df) {
  
  # Process each column and build a 1-row data.frame per variable
  
  df_Num <- df |> select_if(.predicate = is.numeric)
  df_NotNum <- df |> select_if(.predicate = function(x){!(is.numeric(x))})
  
  res_list_num <- lapply(names(df_Num), single_summary, df=df_Num)
  
  res_list_notNum <- lapply(names(df_NotNum), single_summary, df=df_NotNum)
  
  # Combine all rows into a single data.frame
  final_df_Num <- do.call(rbind, res_list_num)
  final_df_NotNum <- do.call(rbind, res_list_notNum)

  return(list("Numeric" = final_df_Num,
              "NonNumeric" = final_df_NotNum))
}
