tbl %>% colnames()


fx_tbl <- tbl %>% 
  dplyr::select(DXY, starts_with("FX"))



ols.m <- fx_tbl %>% 
  mutate(FX_EUR_USD = 1/FX_USD_EUR,
         FX_GBP_USD = 1/FX_USD_GBP,) %>% 
  mutate_if(.predicate = is.numeric, .funs = log) %>% 
lm(DXY ~ FX_EUR_USD + FX_JPY_USD + FX_GBP_USD + FX_CAD_USD + FX_SEK_USD + FX_CHF_USD, data = .)

summary(ols.m)

# Euro (EUR), 57.6% weight
# Japanese yen (JPY), 13.6% weight
# Pound sterling (GBP), 11.9% weight
# Canadian dollar (CAD), 9.1% weight
# Swedish krona (SEK), 4.2% weight
# Swiss franc (CHF), 3.6% weight

fx_tbl %>% 
  mutate(FX_EUR_USD = 1/FX_USD_EUR,
         FX_GBP_USD = 1/FX_USD_GBP,) %>% 
  mutate_if(.predicate = is.numeric, .funs = log) %>% 
  dplyr::select(FX_EUR_USD, FX_JPY_USD, FX_GBP_USD, FX_CAD_USD, 
     FX_SEK_USD, FX_CHF_USD) %>% 
  na.omit() %>% 
  prcomp(scale. = TRUE, center = TRUE) ->a

summary(a, )

a




