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

tbl <- readxl::read_xlsx(path = "./Database/FCI Final Data.xlsx")

tbl$Date <-  as.Date(tbl$Date)


# Group setup -------------------------------------------------------------

m_groups <- list( 
  # Produto = c("IBCBR", "PIM"),
  Juros_exterior = c("Bond_USA_3M", "Bond_GRB_3M", "Bond_DEM_3M", "Bond_JAP_3M",
                     "Bond_USA_2Y", "Bond_GRB_2Y", "Bond_DEM_2Y", "Bond_JAP_2Y",
                     "Bond_USA_10Y", "Bond_GRB_10Y", "Bond_DEM_10Y", "Bond_JAP_10Y"),
  Risco = c("VIX", "BRA_CDS_5Y"),
  Juros_Brazil = c("Tx_1ano", "Tx_2ano", "Tx_3ano", "Tx_4ano",
                   "Tx_5ano", "Tx_10ano"),
  Commoditires = c("CRB_FOOD", "CRB_METL"),
  Mercado_capitais = c("MSCI_Dev_Index", "MSCI_Emerg_Index", "IBOV"),
  Moedas = c("DXY", "DXY_EME",
             "FX_BRL_USD", "FX_USD_EUR", "FX_JPY_USD",
             "FX_USD_GBP", "FX_CAD_USD", "FX_SEK_USD", "FX_CHF_USD"),
  Petroleo = c("WTI", "Brent") )



m_groups


Check.UnitRoot(tbl, N.lags = 5, InfoCriteria = "BIC", critical = "5pct")

tbl <- na.omit(tbl)

# Juros_exterior ----------------------------------------------------------

tbl.JE <- tbl %>% 
  dplyr::select(c(m_groups$Juros_exterior, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.nn) %>% 
  na.omit()

tbl.JE %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.JE <- tbl.JE %>% 
  select(-Date, Bond_GRB_3M ) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.JE)

# Risco -------------------------------------------------------------------

tbl.RK <- tbl %>% 
  dplyr::select(c(m_groups$Risco, "Date")) %>%
  # mutate_if(.predicate = is.numeric, .funs = d.nn) %>% 
  na.omit()

tbl.RK %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.RK <- tbl.RK %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.RK)


# Juros_Brazil ------------------------------------------------------------

tbl.JB <- tbl %>% 
  dplyr::select(c(m_groups$Juros_Brazil, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.nn) %>%
  na.omit()

tbl.JB %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.JB <- tbl.JB %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.JB)


# Commoditires ------------------------------------------------------------

tbl.CM <- tbl %>% 
  dplyr::select(c(m_groups$Commoditires, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.ln) %>%
  na.omit()


tbl.CM %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.CM <- tbl.CM %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.CM)


# Mercado_capitais --------------------------------------------------------

tbl.MK <- tbl %>% 
  dplyr::select(c(m_groups$Mercado_capitais, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.ln) %>%
  na.omit()

tbl.MK %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.MK <- tbl.MK %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.CM)



# Moedas ------------------------------------------------------------------

tbl.MM <- tbl %>% 
  dplyr::select(c(m_groups$Moedas, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.ln) %>%
  na.omit()

tbl.MM %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.MM <- tbl.MM %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.MM)

# Petroleo ----------------------------------------------------------------

tbl.OL <- tbl %>% 
  dplyr::select(c(m_groups$Petroleo, "Date")) %>%
  mutate_if(.predicate = is.numeric, .funs = d.ln) %>%
  na.omit()

tbl.OL %>% 
  pivot_longer(cols = -Date) %>% 
  ggplot() + geom_line(aes(x =Date, y = value)) + 
  facet_wrap(~name)

pca.OL <- tbl.OL %>% 
  select(-Date) %>% 
  # scale() %>%
  # boxplot()
  prcomp(center = TRUE, scale. = TRUE)

summary(pca.OL)


# Produto -----------------------------------------------------------------

tbl.YY <- tbl %>% 
  dplyr::select(c("IBCBR", "Date")) %>%
  # dplyr::select(c("IBCBR")) %>%
  # mutate_if(.predicate = is.numeric, .funs = d.ln) %>%
  mutate(DUM_202004 = if_else(condition = Date == as.Date("2020-04-01"),true = 1, false = 0),
         DUM_202003 = if_else(condition = Date == as.Date("2021-03-01"),true = 1, false = 0),
         DUM_201805 = if_else(condition = Date == as.Date("2018-05-01"),true = 1, false = 0),
         DUM_202302 = if_else(condition = Date == as.Date("2023-02-01"),true = 1, false = 0)) %>% 
  na.omit()



tbl.YX <- tbl.YY[-1,] %>% 
  dplyr::bind_cols(pca.CM$x) %>% 
  rename(Commod.PC1 = PC1, 
         CM.PC2 = PC2) %>% 
  dplyr::bind_cols(pca.JB$x) %>% 
  rename(JurosBRA.PC1 = PC1, 
         JB.PC2 = PC2,
         JB.PC3 = PC3, 
         JB.PC4 = PC4,
         JB.PC5 = PC5, 
         JB.PC6 = PC6
  ) %>% 
  dplyr::bind_cols(pca.JE$x) %>% 
  rename(JurosExt.PC1 = PC1, 
         JE.PC2 = PC2,
         JE.PC3 = PC3, 
         JE.PC4 = PC4,
         JE.PC5 = PC5, 
         JE.PC6 = PC6,
         JE.PC7 = PC7,
         JE.PC8 = PC8,
         JE.PC9 = PC9,
         JE.PC10 = PC10,
         JE.PC11 = PC11,
  ) %>% 
  dplyr::bind_cols(pca.MK$x) %>% 
  rename(MercCapitais.PC1 = PC1, 
         MK.PC2 = PC2,
         MK.PC3 = PC3, 
  ) %>%
  dplyr::bind_cols(pca.MM$x) %>% 
  rename(Moedas.PC1 = PC1, 
         MM.PC2 = PC2,
         MM.PC3 = PC3,
         MM.PC4 = PC4,
         MM.PC5 = PC5,
         MM.PC6 = PC6,
         MM.PC7 = PC7,
         MM.PC8 = PC8,
         MM.PC9 = PC9
  ) %>%   
  dplyr::bind_cols(pca.OL$x) %>% 
  rename(Oil.PC1 = PC1, 
         OL.PC2 = PC2
  ) %>%   
  dplyr::bind_cols(pca.RK$x[-1,]) %>% 
  rename(Risco.PC1 = PC1, 
         RK.PC2 = PC2
  ) %>%
  mutate(IBCBR_1 = lag(IBCBR),
         IBCBR_0 = lead(IBCBR))
# scale() %>% 
# boxplot()
# lm(IBCBR ~ ., data = .) -> model
# lm(IBCBR ~ lag(IBCBR), data = .) -> model


forecast::auto.arima(y = tbl$IBCBR, trace = TRUE, stepwise = FALSE)

model <- lm(IBCBR ~ lag(IBCBR, 1) +
              lag(JurosBRA.PC1,     2) +
              lag(Commod.PC1,       2) +
              lag(JurosExt.PC1,     2) +
              lag(MercCapitais.PC1, 2) +
              lag(Moedas.PC1,       2) +
              lag(Oil.PC1,          2) +
              lag(Risco.PC1,        2) +
              DUM_202004 + DUM_202003 + DUM_201805 + DUM_202302 + 
              1, data = tbl.YX)
summary(model)

lmtest::dwtest(model)

tbl.YX[c(-1, -2),] %>% 
  mutate(aaa = model$residuals) %>% 
  ggplot() + 
  geom_line(aes(x = Date, y = aaa))

which.min(model$residuals)

tbl.YX[149:154,]

forecast::Acf(model$residuals)
forecast::Pacf(model$residuals)
forecast::checkresiduals(model)
summary(model)

soma <- sum(model$coefficients[c("JurosBRA.PC1", "Commod.PC1", "JurosExt.PC1", "MercCapitais.PC1", "Moedas.PC1", "Oil.PC1", "Risco.PC1")])
soma
model$coefficients[c("JurosBRA.PC1", "Commod.PC1", "JurosExt.PC1", "MercCapitais.PC1", "Moedas.PC1", "Oil.PC1", "Risco.PC1")] / soma

