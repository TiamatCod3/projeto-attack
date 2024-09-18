install.packages("readxl") 
library(readxl)

Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")

attack1 <- read_excel("Dados/Dados_Adeanio_Attack_I_27052024.xlsx", sheet = "data")

#Contando o número de ataques como desfecho
attack1$attack621 <- as.numeric(attack1$attack621)
attack1$attack621 <- ifelse(is.na(attack1$attack621), 0, attack1$attack621)
attack1$attack621[attack1$attack621 == 99] = 0
attack1$attack621

attack1$attack621_v2 <- as.numeric(attack1$attack621_v2)
attack1$attack621_v2 <- ifelse(is.na(attack1$attack621_v2), 0, attack1$attack621_v2)
attack1$attack621_v2[attack1$attack621_v2 == 99] = 0
attack1$attack621_v2

attack1$attack211 <- as.numeric(attack1$attack211)
attack1$attack211 <- ifelse(is.na(attack1$attack211), 0, attack1$attack211)
attack1$attack211[attack1$attack211 == 99] = 0
attack1$attack211

attack1$attack211_v2 <- as.numeric(attack1$attack211_v2)
attack1$attack211_v2 <- ifelse(is.na(attack1$attack211_v2), 0, attack1$attack211_v2)
attack1$attack211_v2[attack1$attack211_v2 == 99] = 0
attack1$attack211_v2

attack1$soma.ataques <- attack1$attack621 + attack1$attack621_v2 + attack1$attack211 + attack1$attack211_v2
attack1$soma.ataques

attack1$maximo.ataques <- pmax(attack1$attack621,  attack1$attack621_v2 ,  attack1$attack211 ,  attack1$attack211_v2, na.rm = T)
attack1$maximo.ataques

#Contando os desfechos de controle da asma por score
# attack184, attack184_v1, attack184_v2, attack184_v3, attack616
#Atualizando o score
attack1$attack180[is.na(attack1$attack180) | attack1$attack180 == "Não"] = 0
attack1$attack180[attack1$attack180 == "Sim"] = 1
attack1$attack180

attack1$attack181[is.na(attack1$attack181) | attack1$attack181 == "Não"] = 0
attack1$attack181[attack1$attack181 == "Sim"] = 1
attack1$attack181
attack1$attack182[is.na(attack1$attack182) | attack1$attack182 == "Não"] = 0
attack1$attack182[attack1$attack182 == "Sim"] = 1
attack1$attack182
attack1$attack183[is.na(attack1$attack183) | attack1$attack183 == "Não"] = 0
attack1$attack183[attack1$attack183 == "Sim"] = 1
attack1$attack183

attack1$attack184 = attack1$attack180 + attack1$attack181 + attack1$attack182 + attack1$attack183
attack1$attack184

#Atualizando o score
attack1$attack180_v2[is.na(attack1$attack180_v2) | attack1$attack180_v2 == "Não"] = 0
attack1$attack180_v2[attack1$attack180_v2 == "Sim"] = 1
attack1$attack180_v2 = as.numeric(attack1$attack180_v2)
attack1$attack180_v2

attack1$attack181_v2[is.na(attack1$attack181_v2) | attack1$attack181_v2 == "Não"] = 0
attack1$attack181_v2[attack1$attack181_v2 == "Sim"] = 1
attack1$attack181_v2 = as.numeric(attack1$attack181_v2)
attack1$attack181_v2

attack1$attack182_v2[is.na(attack1$attack182_v2) | attack1$attack182_v2 == "Não"] = 0
attack1$attack182_v2[attack1$attack182_v2 == "Sim"] = 1
attack1$attack182_v2 = as.numeric(attack1$attack182_v2)
attack1$attack182_v2

attack1$attack183_v2[is.na(attack1$attack183_v2) | attack1$attack183_v2 == "Não"] = 0
attack1$attack183_v2[attack1$attack183_v2 == "Sim"] = 1
attack1$attack183_v2 = as.numeric(attack1$attack183_v2)
attack1$attack183_v2

attack1$attack180_v2 + attack1$attack181_v2 + attack1$attack182_v2 + attack1$attack183_v2

attack1$attack184_v2 = attack1$attack180_v2 + attack1$attack181_v2 + attack1$attack182_v2 + attack1$attack183_v2 
attack1$attack184_v2

#Atualizando o score
attack1$attack180_v3[is.na(attack1$attack180_v3) | attack1$attack180_v3 == "Não"] = 0
attack1$attack180_v3[attack1$attack180_v3 == "Sim"] = 1
attack1$attack180_v3 = as.numeric(attack1$attack180_v3)
attack1$attack180_v3

attack1$attack181_v3[is.na(attack1$attack181_v3) | attack1$attack181_v3 == "Não"] = 0
attack1$attack181_v3[attack1$attack181_v3 == "Sim"] = 1
attack1$attack181_v3 = as.numeric(attack1$attack181_v3)
attack1$attack181_v3

attack1$attack182_v3[is.na(attack1$attack182_v3) | attack1$attack182_v3 == "Não"] = 0
attack1$attack182_v3[attack1$attack182_v3 == "Sim"] = 1
attack1$attack182_v3 = as.numeric(attack1$attack182_v3)
attack1$attack182_v3

attack1$attack183_v3[is.na(attack1$attack183_v3) | attack1$attack183_v3 == "Não"] = 0
attack1$attack183_v3[attack1$attack183_v3 == "Sim"] = 1
attack1$attack183_v3 = as.numeric(attack1$attack183_v3)
attack1$attack183_v3

attack1$attack184_v3 = attack1$attack180_v3 + attack1$attack181_v3 + attack1$attack182_v3 + attack1$attack183_v3 
attack1$attack184_v3

#Atualizando o score
attack1$attack612[is.na(attack1$attack612) | attack1$attack612 == "Não"] = 0
attack1$attack612[attack1$attack612 == "Sim"] = 1
attack1$attack612 = as.numeric(attack1$attack612)
attack1$attack612

attack1$attack613[is.na(attack1$attack613) | attack1$attack613 == "Não"] = 0
attack1$attack613[attack1$attack613 == "Sim"] = 1
attack1$attack613 = as.numeric(attack1$attack613)
attack1$attack613

attack1$attack614[is.na(attack1$attack614) | attack1$attack614 == "Não"] = 0
attack1$attack614[attack1$attack614 == "Sim"] = 1
attack1$attack614 = as.numeric(attack1$attack614)
attack1$attack614

attack1$attack615[is.na(attack1$attack615) | attack1$attack615 == "Não"] = 0
attack1$attack615[attack1$attack615 == "Sim"] = 1
attack1$attack615 = as.numeric(attack1$attack615)
attack1$attack615

attack1$attack616 = attack1$attack612 + attack1$attack613 + attack1$attack614 + attack1$attack615
attack1$attack616


#Criando o campo de score máximo entre os 4
attack1$score.maximo = pmax(attack1$attack184, attack1$attack184_v2, attack1$attack184_v3, attack1$attack616, na.rm = T)
# attack1$score.maximo[attack1$score.maximo > 4] = NA
attack1$score.maximo

attack1$classificao.score.maximo = ifelse(attack1$score.maximo == 0 , "controlado", 
                                          ifelse(attack1$score.maximo %in% c(1,2), "parcialmente controlado", 
                                                 ifelse(attack1$score.maximo %in% c(3,4), "não controlado", NA)))
attack1$classificao.score.maximo


#VERIFICANDO BARREIRAS RELACIONADAS A EXAMES
#Inserindo o valor TRUE para sim e FALSE para não e mantendo o NA
attack1$attack533

attack1$attack533 <- lapply(attack1$attack533, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack1$attack533 <- unlist(attack1$attack533)
unlist(attack1$attack533)
attack1$attack533

attack1$attack599 <- lapply(attack1$attack599, function(x) ifelse(is.na(x), 0, ifelse(x == "Sim", 1, 0)))
attack1$attack599 <- unlist(attack1$attack599)
unlist(attack1$attack599)
attack1$attack599

attack1$barreira.exame <- ifelse(is.na(attack1$attack533) & is.na(attack1$attack599),
                       0,
                       ifelse(is.na(attack1$attack533) & attack1$attack599, attack1$attack599,
                              ifelse(is.na(attack1$attack599) & attack1$attack533, attack1$attack533,
                                     attack1$attack533 | attack1$attack599)))
attack1$barreira.exame

  #Teste t entre ataques x barreira de exames
resultado_t_n_ataques_acesso_exame = t.test(soma.ataques ~ barreira.exame, data=attack1, na.action = na.omit)

# Extraindo os resultados do teste t
t_statistic <- resultado_t_n_ataques_acesso_exame$statistic
p_value <- resultado_t_n_ataques_acesso_exame$p.value
conf_int <- resultado_t_n_ataques_acesso_exame$conf.int
mean_diff <- resultado_t_n_ataques_acesso_exame$estimate
df <- resultado_t_n_ataques_acesso_exame$parameter

# Criando uma tabela de resultados
resultados_ataques_exames <- data.frame(
  "Estatística t" = round(t_statistic, 4),
  "Valor p" = round(p_value, 4),
  "Intervalo de Confiança Inferior" = round(conf_int[1], 4),
  "Intervalo de Confiança Superior" = round(conf_int[2], 4),
  "Diferença de Médias" = round(mean_diff[1] - mean_diff[2], 4),
  "Graus de Liberdade" = df
)

resultados_ataques_exames

#Teste chi quadrado entre controle x barreira exames
tabela_contingencia <- table(attack1$classificao.score.maximo, attack1$barreira.exame)
#Retirando a primeira linha porque o controlado é igual a zero
# tabela_contingencia = tabela_contingencia[-1,]
tabela_contingencia
resultado_fisher <- fisher.test(tabela_contingencia)
resultado_fisher


#VERIFICANDO BARREIRAS RELACIONADAS A TRANSPORTE
attack1$attack546
attack1$barreira.transporte = attack1$attack546
attack1$barreira.transporte[is.na(attack1$barreira.transporte) | attack1$barreira.transporte == "Sim"] = 1
attack1$barreira.transporte[attack1$barreira.transporte == "Não"] = 0
attack1$barreira.transporte = as.numeric(attack1$barreira.transporte)
attack1$barreira.transporte

teste_t_ataques_transporte = t.test(attack1$soma.ataques ~ attack1$barreira.transporte, data=attack1, na.action = na.omit)
teste_t_ataques_transporte

tabela_contingencia <- table(attack1$classificao.score.maximo, attack1$barreira.transporte)
#Retirando a primeira linha porque o controlado é igual a zero
# tabela_contingencia = tabela_contingencia[-1,]
tabela_contingencia
resultado_fisher <- fisher.test(tabela_contingencia)
resultado_fisher

#VERIFICANDO BARREIRAS RELACIONADAS A ATENÇÃO BÁSICA
attack1$attack527
attack1$barreira.atencao = attack1$attack527
attack1$barreira.atencao[attack1$barreira.atencao == "Sim"] = 1
attack1$barreira.atencao
attack1$barreira.atencao[is.na(attack1$barreira.atencao)] = 0
attack1$barreira.atencao[attack1$barreira.atencao == "Não"] = 0
attack1$barreira.atencao[attack1$barreira.atencao == "NA"] = 0
attack1$barreira.atencao = as.numeric(attack1$barreira.atencao)
attack1$barreira.atencao

teste_t_ataques_atencao = t.test(attack1$soma.ataques ~ attack1$barreira.atencao, data=attack1, na.action = na.omit)
teste_t_ataques_atencao

#VERIFICANDO BARREIRAS RELACIONADAS A MEDICAMENTOS
attack1$attack601
attack1$barreira.medicamento = attack1$attack601
attack1$barreira.medicamento[attack1$barreira.medicamento == "Sim"] = 1
attack1$barreira.medicamento[attack1$barreira.medicamento == "Não"] = 0
attack1$barreira.medicamento[is.na(attack1$barreira.medicamento)] = 0
attack1$barreira.medicamento = as.numeric(attack1$barreira.medicamento)
attack1$barreira.medicamento

attack1$barreira.medicamento[attack1$barreira.medicamento == "NA"] = 0
attack1$barreira.medicamento = as.numeric(attack1$barreira.medicamento)
attack1$barreira.medicamento

teste_t_ataques_atencao = t.test(attack1$soma.ataques ~ attack1$barreira.medicamento, data=attack1, na.action = na.omit)
teste_t_ataques_atencao

tabela_contingencia <- table(attack1$classificao.score.maximo, attack1$barreira.medicamento)
#Retirando a primeira linha porque o controlado é igual a zero
# tabela_contingencia = tabela_contingencia[-1,]
tabela_contingencia
resultado_fisher <- fisher.test(tabela_contingencia)
resultado_fisher


#VERIFICANDO BARREIRAS RELACIONADAS A INTERNAÇÃO
attack1$attack929
attack1$barreira.internacao = attack1$attack601
attack1$barreira.internacao
attack1$barreira.internacao[attack1$barreira.internacao == "Sim"] = 1
attack1$barreira.internacao
attack1$barreira.internacao[attack1$barreira.internacao == "Não"] = 0
attack1$barreira.internacao
attack1$barreira.internacao[attack1$barreira.internacao == "NA"] = 0
attack1$barreira.internacao
attack1$barreira.internacao[is.na(attack1$barreira.internacao)] = 0
attack1$barreira.internacao = as.numeric(attack1$barreira.internacao)
attack1$barreira.internacao

teste_t_ataques_internacao = t.test(attack1$soma.ataques ~ attack1$barreira.internacao, data=attack1, na.action = na.omit)
teste_t_ataques_internacao

tabela_contingencia <- table(attack1$classificao.score.maximo, attack1$barreira.internacao)
#Retirando a primeira linha porque o controlado é igual a zero
tabela_contingencia
# tabela_contingencia = tabela_contingencia[-1,]
resultado_fisher <- fisher.test(tabela_contingencia)
resultado_fisher

#COMPARANDO OS CASOS EM QUE HOUVE PELO MENOS UMA BERREIRA DE ACESSO
attack1$barreira.unica = ifelse(attack1$barreira.exame == T , T, F)
attack1$barreira.unica
attack1$barreira.unica = ifelse(attack1$barreira.unica == T | attack1$barreira.transporte == T , T, F)
attack1$barreira.unica = ifelse(attack1$barreira.unica == T | attack1$barreira.atencao == T , T, F)
attack1$barreira.unica = ifelse(attack1$barreira.unica == T | attack1$barreira.medicamento == T , T, F)
attack1$barreira.unica = ifelse(attack1$barreira.unica == T | attack1$barreira.internacao == T , T, F)


teste_t_barreiras = t.test(attack1$soma.ataques ~ attack1$barreira.unica, data=attack1, na.action = na.omit)
teste_t_barreiras

tabela_contingencia <- table(attack1$classificao.score.maximo, attack1$barreira.unica)
#Retirando a primeira linha porque o controlado é igual a zero
tabela_contingencia
# tabela_contingencia = tabela_contingencia[-1,]
resultado_fisher <- fisher.test(tabela_contingencia)
resultado_fisher


attack1$barreira.total = 
  attack1$barreira.exame +
  attack1$barreira.transporte +
  attack1$barreira.atencao +
  attack1$barreira.medicamento +
  attack1$barreira.internacao

attack1$barreira.total
teste_t_barreiras = t.test(attack1$soma.ataques ~ attack1$barreira.dummy )
teste_t_barreiras
# teste_t_n_barreiras = t.test(attack1$soma.ataques ~ attack1$barreira.total, data=attack1, na.action = na.omit)
# teste_t_barreiras
# resultado_anova <- aov(attack1$soma.ataques ~ attack1$barreira.total, data = attack1)
# resultado_anova
# 
# teste_anova <- aov()

tabela_contingencia <- table(attack1$soma.ataques , attack1$barreira.total)
tabela_contingencia

resultado_fisher <- fisher.test(tabela_contingencia, simulate.p.value = T)
resultado_fisher

modelo_linear <- lm(attack1$soma.ataques ~ attack1$barreira.total, data = attack1)

# Exibindo o resumo do modelo
summary(modelo_linear)

# Plotando os pontos dos dados
plot(attack1$soma.ataques, attack1$barreira.total, main = "Modelo Linear com Pontos de Dados",
     xlab = "Variável Independente (x)", ylab = "Variável Dependente (y)",
     pch = 19, col = "blue")

# Adicionando a linha do modelo linear
abline(modelo_linear, col = "red", lwd = 2)

tabela_contingencia <- table(attack1$classificao.score.maximo , attack1$barreira.total)
tabela_contingencia

resultado_fisher <- fisher.test(tabela_contingencia, simulate.p.value = T)
resultado_fisher

attack1$barreira.dummy = attack1$barreira.total
attack1$barreira.dummy[attack1$barreira.dummy > 1] = "Maior que 1"
attack1$barreira.dummy[attack1$barreira.dummy <= 1] = "1 ou menos"
attack1$barreira.dummy

tabela_contingencia <- table(attack1$soma.ataques , attack1$barreira.dummy)
tabela_contingencia
resultado_fisher <- fisher.test(tabela_contingencia, simulate.p.value = T)
resultado_fisher

tabela_contingencia <- table(attack1$classificao.score.maximo , attack1$barreira.dummy)
tabela_contingencia

resultado_fisher <- fisher.test(tabela_contingencia, simulate.p.value = T)
resultado_fisher








