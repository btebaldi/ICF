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


# Data Load ---------------------------------------------------------------

tbl <- readxl::read_xlsx(path = here::here("./Database/FCI PCAs Data.xlsx"))

tbl$Date <-  as.Date(tbl$Date)


arimaMdl <- forecast::auto.arima(y = tbl$IBCBR_SA, trace = TRUE, stepwise = FALSE)
arimaMdl$residuals %>% forecast::Acf()

model <- lm(IBCBR_SA ~ lag(IBCBR_SA, 1) +
              lag(CM_PC1, 1) +
              lag(JB_PC1, 1) +
              lag(JE_PC1, 1) +
              lag(MK_PC1, 1) +
              lag(MM_PC1, 1) +
              lag(OL_PC1, 1) +
              lag(RK_PC1, 1) +
              DUM_202004 + DUM_202003 + DUM_201805 + DUM_202302 + 
              1, data = tbl)
summary(model)

model$residuals %>% acf()

lmtest::dwtest(model)


forecast::Acf(model$residuals)
forecast::Pacf(model$residuals)
forecast::checkresiduals(model)
summary(model)


coef_FCI <- names(model$coefficients)[str_detect(string = names(model$coefficients), pattern = "PC1")]

soma <- sum(model$coefficients[coef_FCI])
soma
model$coefficients[coef_FCI] / soma


# VAR Model ---------------------------------------------------------------


library(vars)

# 2. Define endogenous variables (the ones you want the VAR to explain)
# Adjust this list based on your actual modeling goal
endo_vars <- tbl |>  
  dplyr::select(IBCBR_SA, ends_with("PC1")) %>%   # example: pick a manageable subset
  as.matrix()

# 3. Define exogenous dummies
exo_vars <- tbl %>%
  dplyr::select(starts_with("DUM_")) %>%
  as.matrix()

# 4. Select lag order using information criteria
lag_select <- VARselect(endo_vars, lag.max = 8, type = "const", exogen = exo_vars)
print(lag_select$selection)

# 5. Estimate the VAR with dummies as exogenous
p <- lag_select$selection["AIC(n)"]  # or choose manually based on SC/BIC too

var_model <- VAR(endo_vars, p = p, type = "const", exogen = exo_vars)
summary(var_model)

var_model_restricted <- restrict(var_model, method = "ser", thresh = 2)
summary(var_model_restricted)

# 6. Check stability (roots should be < 1)
roots(var_model)
roots(var_model_restricted)

arch.test(var_model)
arch.test(var_model_restricted)

normality.test(var_model)
normality.test(var_model_restricted)


acf(var_model$varresult$IBCBR_SA$residuals)
acf(var_model$varresult$CM_PC1$residuals)
acf(var_model$varresult$JB_PC1$residuals)
acf(var_model$varresult$JE_PC1$residuals)
acf(var_model$varresult$JE_PC1$residuals)
acf(var_model$varresult$MK_PC1$residuals)
acf(var_model$varresult$MM_PC1$residuals)
acf(var_model$varresult$OL_PC1$residuals)
acf(var_model$varresult$RK_PC1$residuals)

# 7. Impulse Response Functions
irf_result <- irf(var_model,
                  # impulse = c("CM_PC1","JE_PC1","JB_PC1")     # shock variable
                  response = "IBCBR_SA",  # variable of interest
                  n.ahead = 12,
                  ortho = TRUE,           # Cholesky orthogonalization
                  boot = TRUE,
                  ci = 0.95,
                  runs = 500)

plot(irf_result, plot.type = "single")


# 7. Impulse Response Functions
irf_restrict_result <- irf(var_model_restricted,
                           # impulse = c("CM_PC1","JE_PC1","JB_PC1")     # shock variable
                           response = "IBCBR_SA",  # variable of interest
                           n.ahead = 12,
                           ortho = TRUE,           # Cholesky orthogonalization
                           boot = TRUE,
                           ci = 0.95,
                           runs = 500)

plot(irf_restrict_result, plot.type = "single")

