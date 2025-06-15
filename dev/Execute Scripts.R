
if(!exists("date.ini")) {date.ini <- as.Date(readline(prompt="Enter initial Date: "))}
if(!exists("date.fim")) {date.fim <- as.Date(readline(prompt="Enter final Date: "))}


script <- "./dev/Grupo1_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo2_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo3_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo4_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo5_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo6_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)

script <- "./dev/Grupo7_Monthly Data.R"
cat(sprintf("[%s] Executando '%s'\n", Sys.time(), script))
source(file = script)