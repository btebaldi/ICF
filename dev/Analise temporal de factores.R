library(readxl)
library(dplyr)
library(factoextra)
View(Dados_para_o_ICF)


WTI <- read_excel("./database/Dados para o ICF.xlsx", 
                               range = "G1:H5171")


BRENT <- read_excel("./database/Dados para o ICF.xlsx", 
                  range = "D1:E4200")

WTI$Data <- as.Date(WTI$Data)
BRENT$Data <- as.Date(BRENT$Data)


tbl <- WTI %>% full_join(BRENT, by = c("Data"="Data"))

summary(tbl)

res.pca <- tbl %>%
  select(-Data) %>%
  mutate(WTI = WTI, Brent = log(Brent)) %>% 
  prcomp(center=TRUE, scale = TRUE)

tbl2 <- tbl %>% mutate_if(is.numeric, scale)
mdl <- lm(WTI~Brent, data = tbl2)
summary(mdl)

fviz_eig(res.pca)



tbl %>% ggplot() +geom_point(aes(x=WTI, y=Brent)) +
  geom_abline(slope = 1, colour= "red")
