#' Autor: Bruno Tebaldi Barbosa
#'
#' Data: 2021-05-28
#'
#' Calcula os pesos de influencia de cada PCA por meio do efeito total dos
#' choques em cada PCA.
#'
#'
#' Input: Data inicial e final a serem processadas
#'
#' Output: Arquivos rds (individuais e agrupados)


cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "Step 3 - Calculo do ICF"))

# rm(list=setdiff(ls(), c("date.ini", "date.fim")))
rm(list=ls())

library(ggplot2)
library(lubridate)
# Config files ------------------------------------------------------------

PCA_tbl.filepath <- file.path("database", "database_ICF", "PCA_tbl.rds")
wights.filepath <- file.path("database", "database_ICF", "ICF_Weights.rds")


# User defined functions --------------------------------------------------

D.ln <- function(x){
  ret <- log(x) - lag(log(x))
  return(ret)
}

source("./lib/Get_BCB_SGS.R")



# Carrega base de dados dos Pesos -----------------------------------------

weights <- readRDS(wights.filepath)

# Carrega base de dados dos PCA´s -----------------------------------------
cat(sprintf("Carregando PCAs\n"))
tbl <- readRDS(PCA_tbl.filepath)


# Carrega base de dados do IBC-BR -----------------------------------------

cat(sprintf("Cruzando PCAs com IBCBR (D.ln)\n"))
IBCBR.tbl <- Get_IBCBR()
IBCBR.tbl <- IBCBR.tbl %>% mutate(IBCBR = D.ln(Valor))

tbl <- tbl %>% left_join(IBCBR.tbl, by=c("Date"="Data"))

# Calcula o ICF -----------------------------------------------------------

ICF <- data.matrix(tbl[, paste("PCA_Grupo", 1:7, sep = "")]) %*% weights[paste("PCA_Grupo", 1:7, sep = "")]
tbl$ICF <- as.numeric(scale(ICF, scale = FALSE))+100


# Define a Monthly table ----------------------------------------------------

# Calcula os pesos basedos
mdl.0 <- lm(IBCBR ~ 1 + ICF, data=tbl)
summary(mdl.0)


mdl.1 <- lm(lead(IBCBR) ~ 1 + ICF, data=tbl)
summary(mdl.1)

mdl.2 <- lm(lead(IBCBR, 2) ~  1 + ICF, data=tbl)
summary(mdl.2)

mdl.3 <- lm(lead(IBCBR, 3) ~  1 + ICF, data=tbl)
summary(mdl.3)

mdl.4 <- lm(lead(IBCBR, 4) ~  1 + ICF, data=tbl)
summary(mdl.4)

# Graficos ----------------------------------------------------------------

tail(tbl)
g <- tbl %>%
  filter(Ano >= year(Sys.Date()) -3 ) %>%
  ggplot() +
  geom_line(aes(x=Date, y=(IBCBR), colour="IBCBR"), size=1) +
  geom_line(aes(x=Date, y=lead(IBCBR), colour="IBCBR"), size=0.5 , linetype = "dashed") +
  geom_ribbon(aes(x=Date, ymax=ICF-100, ymin=00, fill="ICF"), alpha=0.3) +
  geom_point(aes(x=Date, y=ICF-100, fill="ICF"), alpha=0.3) +
  geom_line(aes(x=Date, y=ICF-100), alpha=0.3) +
  labs(title = "Indice de condições financeiras",
       colour = NULL, fill = NULL) +
  scale_fill_manual(breaks = c("ICF"), values = c("blue")) +
  theme_bw() +
  theme(legend.position="bottom") + 
  scale_x_date(date_breaks = "1 month",# date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

ggsave(filename = sprintf("IRF_%s.png", "Total"),
       path = "./Export/ICF",
       plot = g,
       scale=1, units = "in", dpi = 300, width = 10.4, height = 5.85)
