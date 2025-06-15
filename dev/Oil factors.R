rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

data.ini <- as.Date("2000-01-01")
data.fim <- as.Date("2021-05-01")

tbl <- tibble(Date = seq(from=data.ini,
                         to=data.fim,
                         by="day"))

tbl$wday <- wday(tbl$Date)

WTI <- read_excel("GitHub/NovaFutura/ICF/database/Dados ICF Bloomberg.xlsx", 
                  sheet = "WTI", range = "A1:B5302")
WTI$Date <- as.Date(WTI$Date)

Brent <- read_excel("GitHub/NovaFutura/ICF/database/Dados ICF Bloomberg.xlsx", 
                  sheet = "Brent", range = "A1:B5406")

Brent$Date <- as.Date(Brent$Date)

tbl <- tbl %>% left_join(WTI, by = c("Date"="Date"))
# colnames(tbl) <- c("Date", "wday", "WTI")


tbl <- tbl %>% left_join(Brent, by = c("Date"="Date"))
colnames(tbl) <- c("Date", "wday", "WTI", "Brent")


ggplot(tbl) +
  geom_point(aes(x=WTI, y=Brent), colour="blue", alpha=0.2) + 
  geom_abline(slope = 1, colour="red", size=1)

ggplot(tbl) +
  # geom_point(aes(x=Date, y=Brent), colour="blue", alpha=0.2) +
  geom_line(aes(x=Date, y=Brent), colour="blue") + 
  # geom_point(aes(x=Date, y=WTI), colour="red", alpha=0.2) +
  geom_line(aes(x=Date, y=WTI), colour="red") + 
  geom_hline(yintercept = 0)


tbl %>% mutate(Ano = year(Date), Mes=month(Date)) %>% 
  group_by(Ano, Mes) %>% 
  summarise(max(Date))


is.na(tbl$WTI) & !is.na(tbl$Brent)


d.Ln <- function(x){
  ret <- log(x) - lag(log(x))
  return(ret)
}


tbl.l <- tbl %>% mutate_if(is.numeric, .funs = log)

tbl %>% filter(wday != 7 & wday != 1) %>% 
  ggplot() +
  # geom_point(aes(x=Date, y=Brent), colour="blue", alpha=0.2) +
  geom_line(aes(x=Date, y=Brent), colour="blue") + 
  # geom_point(aes(x=Date, y=WTI), colour="red", alpha=0.2) +
  geom_line(aes(x=Date, y=WTI), colour="red") + 
  geom_hline(yintercept = 0)



lm(WTI~Brent, data=tbl.l)

all(is.na(tbl2$WTI))

tbl2 <- tbl %>% filter(wday != 7 & wday != 1)

table(is.na(tbl2$WTI), is.na(tbl2$Brent), dnn = c("WTI", "BRENT"))






tbl %>% mutate(Ano = year(Date), Mes=month(Date)) %>% 
  group_by(Ano, Mes) %>% 
  summarise(max(Date))


tbl %>% pivot_longer(cols = c("WTI", "Brent"), names_to = "Oil") %>% 
ggplot() +
  # geom_boxplot(color = "gray60", outlier.alpha = 0)
  geom_jitter(aes(y=Oil, x=value, colour=Oil), size = 3, alpha = 0.15) + 
  geom_boxplot(aes(y=Oil, x=value), alpha=0.2) + 
  # geom_jitter(aes(y=Oil, x=value)) +
  # geom_abline(slope = 1, colour="red", size=1) +
  labs()

g +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15)









