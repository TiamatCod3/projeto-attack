library(readxl)
Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")

attack1 <- read_excel("Dados/Dados_Adeanio_Attack_I_27052024.xlsx", sheet = "data")

print(attack1$id)
