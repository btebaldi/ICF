

cat(sprintf("[%s] Executando '%s'\n", Sys.time(), "All scripts"))
source("./scripts/Step 0A - Update Swap PreDi.R", encoding = "UTF-8")
source("./scripts/Step 0B - Monthly Data_Handler.R", encoding = "UTF-8")
source("./scripts/Step 1 - Estima Componentes.R", encoding = "UTF-8")
source("./scripts/Step 2 - Estima Pesos.R", encoding = "UTF-8")
source("./scripts/Step 3 - Calcula ICF.R", encoding = "UTF-8")
cat(sprintf("[%s] Fim da execução '%s'\n", Sys.time(), "All scripts"))