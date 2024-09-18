library(readxl)

Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")

attack2 <- read_excel("Dados/Dados_Adeanio_Attack_II_27052024.xlsx", sheet = "data")

#Contando o número de ataques
# attack2$attack0173 <- as.numeric
attack2$attack0173 <- as.numeric(attack2$attack0173)
attack2$attack0173 <- ifelse(is.na(attack2$attack0173), 0, attack2$attack0173)
attack2$attack0173[attack2$attack0173 == 999] = 0
attack2$attack0173

attack2$attack0174
attack2$attack0174 <- as.numeric(attack2$attack0174)
attack2$attack0174 <- ifelse(is.na(attack2$attack0174), 0, attack2$attack0174)
attack2$attack0174[attack2$attack0174 == 999] = 0
attack2$attack0174

attack2$attack0573
attack2$attack0573 <- as.numeric(attack2$attack0573)
attack2$attack0573 <- ifelse(is.na(attack2$attack0573), 0, attack2$attack0573)
attack2$attack0573[attack2$attack0573 == 999] = 0
attack2$attack0573


attack2$attack0176
attack2$attack0176 <- as.numeric(attack2$attack0176)
attack2$attack0176 <- ifelse(is.na(attack2$attack0176), 0, attack2$attack0176)
attack2$attack0176[attack2$attack0176 == 999] = 0
attack2$attack0176


attack2$attack0177
attack2$attack0177 <- as.numeric(attack2$attack0177)
attack2$attack0177 <- ifelse(is.na(attack2$attack0177), 0, attack2$attack0177)
attack2$attack0177[attack2$attack0177 == 999] = 0
attack2$attack0177

attack2$soma.ataques <- attack2$attack0173 + attack2$attack0174 + attack2$attack0176 + attack2$attack0177 + attack2$attack0573 
attack2$soma.ataques

attack2$maximo.ataques <- pmax(attack2$attack0173 , attack2$attack0174 , attack2$attack0176 , attack2$attack0177 , attack2$attack0573)
attack2$maximo.ataques

#Barreira Atenção Básica
attack2$attack0643_v3 <- lapply(attack2$attack0643_v3, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack2$attack0643_v3 <- unlist(attack2$attack0643_v3)
unlist(attack2$attack0643_v3)
attack2$attack0643_v3

attack2$barreira.atencao = attack2$attack0643_v3

teste_t_atencao = t.test(attack2$soma.ataques ~ attack2$barreira.atencao, data=attack2, na.action = na.omit)
teste_t_atencao


#Barreira Medicamento
attack2$attack0681 <- lapply(attack2$attack0681, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack2$attack0681 <- unlist(attack2$attack0681)
unlist(attack2$attack0681)
attack2$attack0681

attack2$attack0681_v2 <- lapply(attack2$attack0681_v2, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack2$attack0681_v2 <- unlist(attack2$attack0681_v2)
unlist(attack2$attack0681_v2)
attack2$attack0681_v2

attack2$barreira.medicamento <- pmax(attack2$attack0681 , attack2$attack0681_v2)
attack2$barreira.medicamento

teste_t_medicamento = t.test(attack2$soma.ataques ~ attack2$barreira.medicamento, data=attack2, na.action = na.omit)
teste_t_medicamento

#Barreira Transporte
attack2$attack0662
attack2$attack0662 <- lapply(attack2$attack0662, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack2$attack0662 <- unlist(attack2$attack0662)
unlist(attack2$attack0662)
attack2$attack0662

attack2$barreira.transporte <- attack2$attack0662

teste_t_transporte = t.test(attack2$soma.ataques ~ attack2$barreira.transporte, data=attack2, na.action = na.omit)
teste_t_transporte


#Barreira Exame
attack2$attack0649
attack2$attack0649 <- lapply(attack2$attack0649, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack2$attack0649 <- unlist(attack2$attack0649)
unlist(attack2$attack0649)
attack2$attack0649

attack2$barreira.exame <- attack2$attack0649

teste_t_exame = t.test(attack2$soma.ataques ~ attack2$barreira.exame, data=attack2, na.action = na.omit)
teste_t_exame

attack2$barreira.exame


attack2$barreira.unica <- pmax(attack2$barreira.atencao, attack2$barreira.exame, attack2$barreira.transporte, attack2$barreira.medicamento)
attack2$barreira.unica

teste_t_barreiras = t.test(attack2$soma.ataques ~ attack2$barreira.unica, data=attack2, na.action = na.omit)
teste_t_barreiras












