#********************************************************************************************
#*************************** TIC Domic?lios 2019 - Individuos *******************************
# Limpa objetos da memoria (Environment)
rm(list=ls())

# Limpa Console
cat("\014")

###########Carrega (e instala, se necess?rio) pacotes requeridos#####
if (!require("install.load")) install.packages("install.load")
suppressMessages(install.load::install_load("tidyverse", "survey", "haven", "labelled"))
library(tidyverse)
library(dplyr)
library(round)
library(MASS)
library(psych)
library(stargazer)
library(corrplot)
library(car)
install.packages("pchisq")
install.packages("apply_if_")
library("stargazer")
library(DescTools)
library(pscl)
library(jtools)
library(bclust)
install.packages("modEvA")

# Define diret?rio (pasta onde se encontra a base de microdados)

setwd("C:/Users/UsuÃ¡rio/Google Drive/Mestrado/DissertaÃ§Ã£o/Codigos/DiretÃ³rio")

# Carrega as bases de dados da TIC DOMIC?LIOS 2019 -  Indiv?duos
load("ticdom_2019_individuos_base_de_microdados_v1.1.RData")


###############

#ASSISTIU VS. PAGOU PRA VER FILMES
print(sum(baseind19$variables$TC9_A == "1" & baseind19$variables$TC4_A == "0"))
print(sum(baseind19$variables$TC4C_D == "1" & baseind19$variables$C9_C == "0"))
print(sum(baseind19$variables$TC4C_D == "1" & baseind19$variables$C9_C == "0"))


table(baseind19$variables$TC4C_D, baseind19$variables$C9_C)

#musica
print(sum(baseind19$variables$TC3_A == "1"))
print(sum(baseind19$variables$TC3_B == "1"))
print(sum(baseind19$variables$C9_B == "1"))
sum(baseind19$variables$C9_B == "1" & baseind19$variables$TC3_B == "1" & baseind19$variables$TC3_A == "1")
print(sum(baseind19$variables$C9_B == "0" & baseind19$variables$TC3_A == "1"))

##C9_B E C12_B

#audio visual1

print(sum(baseind19$variables$C9_C == "1"))
print(sum(baseind19$variables$c12_A == "0"))
print(sum(baseind19$variables$TC4_A == "1"))
print(sum(baseind19$variables$TC6_A == "1"))
print(sum(baseind19$variables$TC6_A == "1" & baseind19$variables$TC4_A == "1"))


#audio visual2 


###############criação de variaveis################################################## 

#feito isso, vamos criar as variaveis p/ analises  

#1 criação da variavel habitos culturais gerais
baseind19 <- update(baseind19,
                    hab_culturais_gerais = ifelse(TC4C_D==1|
                                                  TC4C_E==1|
                                                  TC2==1|
                                                  TC10_D==1|
                                                  TC11_D==1|
                                                  TC13_A==1|
                                                  TC13_B==1|
                                                  TC13_C==1|
                                                  TC13_D==1|
                                                  TC13_E==1|
                                                  TC13_F==1|
                                                  TC13_G==1|
                                                  TC13_H==1|
                                                  C12_C==1|
                                                  TC4B_G==1|
                                                  C9_B==1|
                                                  C12_B==1|
                                                  C9_C==1|
                                                  C12_A==1|
                                                  C9_F==1,1,0))

#2 criação da variavel habitos culturais gratuitos 
baseind19 <- update(baseind19,
                    hab_culturais_gratuitos = ifelse(C9_B==1|
                                                       C9_C==1|
                                                       C9_F==1|
                                                       C12_A==1|
                                                       C12_B==1|
                                                       C12_E1==1|
                                                       C12_F1==1|
                                                       J2_C==1|
                                                       TC3_A==1|
                                                       TC3_B==1|
                                                       TC4_A==1|
                                                       TC4_B==1|
                                                       TC4B_C==1|
                                                       TC4B_D==1|
                                                       TC4B_F==1,1,0))

# 3 criação da variavel habitos culturais pagos 
baseind19 <- update(baseind19,
                    hab_culturais_pagos = ifelse(TC2==1|
                                                   TC4C_D==1|
                                                   TC4C_E==1,1,0))

# 4 criação da variavel habitos culturais criação 
baseind19 <- update(baseind19,
                    hab_criacao_cultural= ifelse(TC10_D==1|
                                                   TC11_D==1, 1,0))

#5 criação da variavel habitos de pesquisa via tics para visita presencial a atividades culturais
baseind19 <- update(baseind19,
                    hab_visita_cultural = ifelse(TC13_A==1|
                                                   TC13_B==1|
                                                   TC13_C==1|
                                                   TC13_D==1|
                                                   TC13_E==1|
                                                   TC13_F==1|
                                                   TC13_G==1|
                                                   TC13_H==1,1,0)) 
#6 criação da variavel habitos de games
baseind19 <- update(baseind19,
                    hab_games  = ifelse(C12_C==1|TC4B_G==1,1,0))

#7 criação da variavel habitos de música
baseind19 <- update(baseind19,
                    hab_musica  = ifelse(C9_B==1|C12_B==1|TC2==1,1,0))

#8 criação da variavel habitos de audiovisual
baseind19 <- update(baseind19,
                    hab_audiovisual  = ifelse(C9_C==1|C12_A==1|TC4C_D==1|TC4C_E==1,1,0))

#9 criação da variavel habitos de visita a museu e exposição
baseind19 <- update(baseind19,
                    hab_museu = ifelse(C9_F==1,1,0))
                
# 10 criação da variavel habitos comunicação e midias socias
baseind19 <- update(baseind19,
                    hab_comunicacao_social = ifelse(C7_B==1|
                                                      C7_C==1|
                                                      C7_D==1|
                                                      C7_F==1|
                                                      J2_B==1|
                                                      J2_M==1|
                                                      J2_N==1|
                                                      J2_I==1, 1,0))

baseind19 <- update(baseind19,
                    hab_comunicacao_social_modelo = case_when(C7_B =="Sim"|C7_C =="Sim"|C7_D=="Sim"|C7_F=="Sim"|J2_B=="Sim"|J2_M=="Sim"|J2_N=="Sim"|J2_I=="Sim" ~ "Sim",
                                                              C7_B =="Não se aplica"|C7_C =="Não se aplica"|C7_D=="Não se aplica"|C7_F=="Não se aplica"|J2_B=="Não se aplica"|J2_M=="Não se aplica"|J2_N=="Não se aplica"|J2_I=="Não se aplica" ~ "Não" ,
                                                              C7_B =="Não"|C7_C =="Não"|C7_D=="Não"|C7_F=="Não"|J2_B=="Não"|J2_M=="Não"|J2_N=="Não"|J2_I=="Não" ~ "Não",
                                                              TRUE ~ as.character(NA)))
baseind19$variables$hab_comunicacao_social_modelo <- factor(baseind19$variables$hab_comunicacao_social_modelo ,
                                                levels = c("Não", "Sim"))



# 11 criação da variavel habitos educação
baseind19 <- update(baseind19,
                    hab_educacionais = ifelse(C10_A==1|
                                                C10_B==1|
                                                C10_D==1|
                                                TC4B_H==1, 1,0))

baseind19 <- update(baseind19,
                    hab_educacionais_modelo = case_when(C10_A =="Sim"|C10_B =="Sim"|C10_D=="Sim"|TC4B_H=="Sim" ~ "Sim",
                                                        C10_A =="Não se aplica"|C10_B =="Não se aplica"|C10_D=="Não se aplica"|TC4B_H=="Não se aplica" ~ "Não" ,
                                                        C10_A =="Não"|C10_B =="Não"|C10_D=="Não"|TC4B_H=="Não" ~ "Não",
                                                        TRUE ~ as.character(NA)))
baseind19$variables$hab_educacionais_modelo <- factor(baseind19$variables$hab_educacionais_modelo ,
                                                            levels = c("Não", "Sim"))



# 12 criação da variavel ocupação informatizada 
baseind19 <- update(baseind19,
                    ocup_informatizada = ifelse(C10_F==1|
                                                  C6_B==1|
                                                  B4_B==1,1,0))

# 13 criação da variavel habitos de noticias e informação
baseind19 <- update(baseind19,
                    hab_informação  = ifelse(TC4B_A==1|
                                               C9_D==1,1,0))

baseind19 <- update(baseind19,
                    hab_informação_modelo = case_when(TC4B_A =="Sim"|C9_D =="Sim" ~ "Sim",
                                                      TC4B_A =="Não se aplica"|C9_D =="Não se aplica"~ "Não" ,
                                                      TC4B_A =="Não"|C9_D =="Não"|C10_D=="Não" ~ "Não",
                                                      TRUE ~ as.character(NA)))
baseind19$variables$hab_informação_modelo <- factor(baseind19$variables$hab_informação_modelo ,
                                                      levels = c("Não", "Sim"))



#hab computacionais

baseind19 <- update(baseind19,
                    hab_computacionais_modelo = case_when(I1_A=="Sim"|I1_B=="Sim"|I1_C=="Sim"|I1_D=="Sim"|I1_E=="Sim"|I1_F=="Sim"|I1_G=="Sim"|I1_H=="Sim"|I1_I =="Sim" ~ "Sim",
                                                          I1_A=="Não"|I1_B=="Não"|I1_C=="Não"|I1_D=="Não"|I1_E=="Não"|I1_F=="Não"|I1_G=="Não"|I1_H=="Não"|I1_I =="Não" ~ "Não",
                                                          I1_A=="Não se aplica"|I1_B=="Não se aplica"|I1_C=="Não se aplica"|I1_D=="Não se aplica"|I1_E=="Não se aplica"|I1_F=="Não se aplica"|I1_G=="Não se aplica"|I1_H=="Não se aplica"|I1_I =="Não se aplica" ~ "Não se aplica",
                                                          TRUE ~ as.character(NA)))
baseind19$variables$hab_computacionais_modelo<- factor(baseind19$variables$hab_computacionais_modelo,
                                                                     levels = c("Sim", "Não"))

#dispositivos

baseind19 <- update(baseind19,
                                  dispositivos_modelo = case_when(C5_DISPOSITIVOS=="Apenas telefone celular" ~ "Apenas telefone celular",
                                                                  C5_DISPOSITIVOS=="Apenas computador" ~ "Outros",
                                                                  C5_DISPOSITIVOS=="Ambos" ~ "Outros",
                                                                  TRUE ~ as.character(NA)))
baseind19$variables$dispositivos_modelo  <- factor(baseind19$variables$dispositivos_modelo ,
                                                                 levels = c("Apenas telefone celular", "Outros"))





###criação alternativa######
baseind19 <- update(baseind19,
                                  SexoMod = ifelse(SEXO=="Masculino", "Masculino", "Feminino"))

baseind19$variables$SexoMod<- factor(baseind19$variables$SexoMod,
                                                   levels = c("Masculino", "Feminino"))

#Faixa etaria ok 
baseind19 <- update(baseind19,
                                  FaixaCriançamod = ifelse(FAIXA_ETARIA=="De 10 a 15 anos", "Criança", "Outros"))
baseind19 <- update(baseind19,
                                  FaixaJovemmod = ifelse(FAIXA_ETARIA=="De 16 a 24 anos", "Jovem", "Outros"))
baseind19 <- update(baseind19,
                                  FaixaJovemAdultomod = ifelse(FAIXA_ETARIA=="De 25 a 34 anos", "Jovem Adulto", "Outros"))
baseind19 <- update(baseind19,
                                  FaixaAdultamod = ifelse(FAIXA_ETARIA=="De 35 a 44 anos", "Adulto", "Outros"))
baseind19 <- update(baseind19,
                                  FaixaAdulta2mod = ifelse(FAIXA_ETARIA=="De 45 a 59 anos", "Adulto2", "Outros"))
baseind19 <- update(baseind19,
                                  FaixaIdosomod = ifelse(FAIXA_ETARIA=="60 anos ou mais", "Idoso", "Outros"))

baseind19$variables$FaixaCriançamod<- factor(baseind19$variables$FaixaCriançamod,
                                                           levels = c("Criança", "Outros"))
baseind19$variables$FaixaJovemmod<- factor(baseind19$variables$FaixaJovemmod,
                                                         levels = c("Jovem", "Outros"))
baseind19$variables$FaixaJovemAdultomod<- factor(baseind19$variables$FaixaJovemAdultomod,
                                                               levels = c("Jovem Adulto", "Outros"))
baseind19$variables$FaixaAdultamod<- factor(baseind19$variables$FaixaAdultamod,
                                                          levels = c("Adulto", "Outros"))
baseind19$variables$FaixaAdulta2mod<- factor(baseind19$variables$FaixaAdulta2mod,
                                                           levels = c("Adulto2", "Outros"))
baseind19$variables$FaixaIdosomod<- factor(baseind19$variables$FaixaIdosomod,
                                                         levels = c("Idoso", "Outros"))
#Raca ok

baseind19 <- update(baseind19,
                                  RacaModelo = case_when(RACA=="Preta"|RACA=="Parda" ~ "PretaParda",
                                                         RACA=="Branca"|RACA=="Amarela"|RACA=="Indígena" ~ "Outros",
                                                         TRUE ~ as.character(NA)))

baseind19$variables$RacaModelo<- factor(baseind19$variables$RacaModelo,
                                                      levels = c("PretaParda", "Outros"))

#Grau de instrução 
baseind19 <- update(baseind19,
                                  Instrucao1Mod = ifelse(GRAU_INSTRUCAO_2=="Analfabeto / Educação infantil", "Analfabeto / Educação infantil", "Outros"))
baseind19 <- update(baseind19,
                                  Instrucao2Mod = ifelse(GRAU_INSTRUCAO_2=="Fundamental", "Fundamental", "Outros"))
baseind19 <- update(baseind19,
                                  Instrucao3Mod = ifelse(GRAU_INSTRUCAO_2=="Médio", "Médio", "Outros"))
baseind19 <- update(baseind19,
                                  Instrucao4Mod = ifelse(GRAU_INSTRUCAO_2=="Superior", "Superior", "Outros"))

baseind19$variables$Instrucao4Mod<- factor(baseind19$variables$Instrucao4Mod,
                                                         levels = c("Superior", "Outros"))
baseind19$variables$Instrucao3Mod<- factor(baseind19$variables$Instrucao3Mod,
                                                         levels = c("Médio", "Outros"))
baseind19$variables$Instrucao2Mod<- factor(baseind19$variables$Instrucao2Mod,
                                                         levels = c("Fundamental", "Outros"))
baseind19$variables$Instrucao1Mod<- factor(baseind19$variables$Instrucao1Mod,
                                                         levels = c("Analfabeto / Educação infantil", "Outros"))

#Classe Social
baseind19 <- update(baseind19,
                                  ClasseA = ifelse(CLASSE_CB2015=="Classe A", "Classe A", "Outros"))
baseind19 <- update(baseind19,
                                  ClasseB = ifelse(CLASSE_CB2015=="Classe B", "Classe B", "Outros"))
baseind19 <- update(baseind19,
                                  ClasseC = ifelse(CLASSE_CB2015=="Classe C", "Classe C", "Outros"))
baseind19 <- update(baseind19,
                                  ClasseDE = ifelse(CLASSE_CB2015=="Classe D/ E", "Classe D/ E", "Outros"))

baseind19$variables$ClasseA<- factor(baseind19$variables$ClasseA,
                                                   levels = c("Classe A", "Outros"))
baseind19$variables$ClasseB<- factor(baseind19$variables$ClasseB,
                                                   levels = c("Classe B", "Outros"))
baseind19$variables$ClasseC<- factor(baseind19$variables$ClasseC,
                                                   levels = c("Classe C", "Outros"))
baseind19$variables$ClasseDE<- factor(baseind19$variables$ClasseDE,
                                                    levels = c("Classe D/ E", "Outros"))

#regiao 
baseind19 <- update(baseind19,
                                  SUL = ifelse(COD_REGIAO_2=="Sul", "Sul", "Outros"))
baseind19 <- update(baseind19,
                                  NORTE = ifelse(COD_REGIAO_2=="Norte", "Norte", "Outros"))
baseind19 <- update(baseind19,
                                  NORDESTE = ifelse(COD_REGIAO_2=="Nordeste", "Nordeste", "Outros"))
baseind19 <- update(baseind19,
                                  SUDESTE = ifelse(COD_REGIAO_2=="Sudeste", "Sudeste", "Outros"))
baseind19 <- update(baseind19,
                                  CentroOeste = ifelse(COD_REGIAO_2=="Centro-Oeste", "Centro-Oeste", "Outros"))
baseind19$variables$SUL<- factor(baseind19$variables$SUL,
                                               levels = c("Sul", "Outros"))
baseind19$variables$NORTE<- factor(baseind19$variables$NORTE,
                                                 levels = c("Norte", "Outros"))
baseind19$variables$NORDESTE<- factor(baseind19$variables$NORDESTE,
                                                    levels = c("Nordeste", "Outros"))
baseind19$variables$SUDESTE<- factor(baseind19$variables$SUDESTE,
                                                   levels = c("Sudeste", "Outros"))
baseind19$variables$CentroOeste<- factor(baseind19$variables$CentroOeste,
                                                       levels = c("Centro-Oeste", "Outros"))


#hab habito cultural geral
baseind19 <- update(baseind19,
                                  hab_culturais_gerais_modelo = case_when(TC4C_D=="Sim"|TC4C_E=="Sim"|TC2=="Sim"|TC10_D=="Sim"|TC11_D=="Sim"|TC13_A=="Sim"| TC13_B=="Sim"|TC13_C=="Sim"|TC13_D=="Sim"| TC13_E=="Sim"| TC13_F=="Sim"| TC13_G=="Sim"|TC13_H=="Sim"|C12_C=="Sim"|TC4B_G=="Sim"|C9_B=="Sim"|C12_B=="Sim"|C9_C=="Sim"|C12_A=="Sim"|C9_F=="Sim"~ "Sim",
                                                                          TC4C_D=="Não"|TC4C_E=="Não"|TC2=="Não"|TC10_D=="Não"|TC11_D=="Não"|TC13_A=="Não"| TC13_B=="Não"|TC13_C=="Não"|TC13_D=="Não"| TC13_E=="Não"| TC13_F=="Não"| TC13_G=="Não"|TC13_H=="Não"|C12_C=="Não"|TC4B_G=="Não"|C9_B=="Não"|C12_B=="Não"|C9_C=="Não"|C12_A=="Não"|C9_F=="Não"~ "Não",
                                                                          TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica"|TC2=="Não se aplica"|TC10_D=="Não se aplica"|TC11_D=="Não se aplica"|TC13_A=="Não se aplica"| TC13_B=="Não se aplica"|TC13_C=="Não se aplica"|TC13_D=="Não se aplica"| TC13_E=="Não se aplica"| TC13_F=="Não se aplica"| TC13_G=="Não se aplica"|TC13_H=="Não se aplica"|C12_C=="Não se aplica"|TC4B_G=="Não"|C9_B=="Não se aplica"|C12_B=="Não se aplica"|C9_C=="Não se aplica"|C12_A=="Não se aplica"|C9_F=="Não se aplica"~ "Não",
                                                                          TRUE ~ as.character(NA)))

baseind19$variables$hab_culturais_gerais_modelo<- factor(baseind19$variables$hab_culturais_gerais_modelo,
                                                                       levels = c("Sim", "Não"))

#hab habito cultural pagos
baseind19 <- update(baseind19,
                                  hab_culturais_pagos_modelo = case_when(TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                         TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                         TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                         TRUE ~ as.character(NA)))
baseind19$variables$hab_culturais_pagos_modelo<- factor(baseind19$variables$hab_culturais_pagos_modelo,
                                                                      levels = c("Sim", "Não"))

#hab cultural gratuitos 
baseind19 <- update(baseind19,
                                  hab_culturais_gratuitos_modelo = case_when(C9_B=="Sim"|C9_C=="Sim"|C9_F=="Sim"|C12_A=="Sim"|C12_B=="Sim"|C12_E1=="Sim"|C12_F1=="Sim"|J2_C=="Sim"|TC3_A=="Sim"|TC3_B=="Sim"|TC4_A=="Sim"|TC4_B=="Sim"|TC4B_C=="Sim"|TC4B_D=="Sim"|TC4B_F=="Sim"|TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                             C9_B=="Não"|C9_C=="Não"|C9_F=="Não"|C12_A=="Não"|C12_B=="Não"|C12_E1=="Não"|C12_F1=="Não"|J2_C=="Não"|TC3_A=="Não"|TC3_B=="Não"|TC4_A=="Não"|TC4_B=="Não"|TC4B_C=="Não"|TC4B_D=="Não"|TC4B_F=="Não"|TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                             C9_B=="Não se aplica"|C9_C=="Não se aplica"|C9_F=="Não se aplica"|C12_A=="Não se aplica"|C12_B=="Não se aplica"|C12_E1=="Não se aplica"|C12_F1=="Não se aplica"|J2_C=="Não se aplica"|TC3_A=="Não se aplica"|TC3_B=="Não se aplica"|TC4_A=="Não se aplica"|TC4_B=="Não se aplica"|TC4B_C=="Não se aplica"|TC4B_D=="Não se aplica"|TC4B_F=="Não se aplica"|TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                             TRUE ~ as.character(NA)))
baseind19$variables$hab_culturais_gratuitos_modelo<- factor(baseind19$variables$hab_culturais_gratuitos_modelo,
                                                                          levels = c("Sim", "Não"))

#hab ocupacao informatizada 
baseind19 <- update(baseind19,
                                  ocup_informatizada_modelo = case_when(C10_F=="Sim"|C6_B=="Sim" ~ "Sim",
                                                                        C10_F=="Não"|C6_B=="Não" ~ "Não",
                                                                        C10_F=="Não se aplica"|C6_B=="Não se aplica" ~ "Não",
                                                                        TRUE ~ as.character(NA)))
baseind19$variables$ocup_informatizada_modelo<- factor(baseind19$variables$ocup_informatizada_modelo,
                                                                     levels = c("Sim", "Não"))

#hab computacionais

baseind19 <- update(baseind19,
                                  hab_computacionais_modelo = case_when(I1_A=="Sim"|I1_B=="Sim"|I1_C=="Sim"|I1_D=="Sim"|I1_E=="Sim"|I1_F=="Sim"|I1_G=="Sim"|I1_H=="Sim"|I1_I =="Sim" ~ "Sim",
                                                                        I1_A=="Não"|I1_B=="Não"|I1_C=="Não"|I1_D=="Não"|I1_E=="Não"|I1_F=="Não"|I1_G=="Não"|I1_H=="Não"|I1_I =="Não" ~ "Não",
                                                                        I1_A=="Não se aplica"|I1_B=="Não se aplica"|I1_C=="Não se aplica"|I1_D=="Não se aplica"|I1_E=="Não se aplica"|I1_F=="Não se aplica"|I1_G=="Não se aplica"|I1_H=="Não se aplica"|I1_I =="Não se aplica" ~ "Não se aplica",
                                                                        TRUE ~ as.character(NA)))
baseind19$variables$hab_computacionais_modelo<- factor(baseind19$variables$hab_computacionais_modelo,
                                                                     levels = c("Sim", "Não"))

#dispositivos

baseind19 <- update(baseind19,
                                  dispositivos_modelo = case_when(C5_DISPOSITIVOS=="Apenas telefone celular" ~ "Apenas telefone celular",
                                                                  C5_DISPOSITIVOS=="Apenas computador" ~ "Outros",
                                                                  C5_DISPOSITIVOS=="Ambos" ~ "Outros",
                                                                  TRUE ~ as.character(NA)))
baseind19$variables$dispositivos_modelo  <- factor(baseind19$variables$dispositivos_modelo ,
                                                                 levels = c("Apenas telefone celular", "Outros"))

#MUSICA

baseind19 <- update(baseind19,
                                  hab_musica_modelo = case_when(C9_B=="Sim"| C12_B == "Sim" | TC2 == "Sim" ~ "Sim",
                                                                C9_B=="Não"| C12_B == "Não" | TC2 == "Não" ~ "Não",
                                                                C9_B=="Não se aplica"| C12_B == "Não se aplica" | TC2 == "Não se aplica" ~ "Não",
                                                                TRUE ~ as.character(NA)))
baseind19$variables$hab_musica_modelo  <- factor(baseind19$variables$hab_musica_modelo,
                                                               levels = c("Não", "Sim"))

#audiovisual

baseind19 <- update(baseind19,
                                  hab_audiovisual_modelo = case_when(C9_C=="Sim"|C12_A=="Sim"|TC4C_D =="Sim"|TC4C_E =="Sim" ~ "Sim",
                                                                     C9_C=="Não"|C12_A=="Não"|TC4C_D =="Não"|TC4C_E =="Sim" ~ "Não",
                                                                     C9_C=="Não se aplica"|C12_A=="Não se aplica"|TC4C_D =="Não se aplica"|TC4C_E =="Não se aplica" ~ "Não",
                                                                     TRUE ~ as.character(NA)))
baseind19$variables$hab_audiovisual_modelo  <- factor(baseind19$variables$hab_audiovisual_modelo ,
                                                                    levels = c("Não", "Sim"))

#museu

baseind19 <- update(baseind19,
                                  hab_museu_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não "|C12_C =="Não se aplica" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19$variables$hab_museu_modelo  <- factor(baseind19$variables$hab_museu_modelo ,
                                                              levels = c("Não", "Sim"))

#JOGOS

baseind19 <- update(baseind19,
                                  hab_games_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não"|C12_C =="Não" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19$variables$hab_games_modelo  <- factor(baseind19$variables$hab_games_modelo ,
                                                              levels = c("Não", "Sim"))

###############Cria os leves e labels de valores das variáveis derivadas acima:#########################
baseind19$variables$hab_culturais_gerais <- factor(baseind19$variables$hab_culturais_gerais,
                                                levels = c(0, 1),
                                                labels = c("Não", "Sim"))

baseind19$variables$hab_culturais_gratuitos <- factor(baseind19$variables$hab_culturais_gratuitos,
                                                      levels = c(0, 1),
                                                      labels = c("Não", "Sim"))
                                                                                                    
baseind19$variables$hab_culturais_pagos <- factor(baseind19$variables$hab_culturais_pagos,
                                                      levels = c(0, 1),
                                                      labels = c("Não", "Sim"))

baseind19$variables$hab_criacao_cultural <- factor(baseind19$variables$hab_criacao_cultural,
                                                  levels = c(0, 1),
                                                  labels = c("Não", "Sim"))

baseind19$variables$hab_comunicacao_social <- factor(baseind19$variables$hab_comunicacao_social,
                                                   levels = c(0, 1),
                                                   labels = c("Não", "Sim"))

baseind19$variables$hab_educacionais <- factor(baseind19$variables$hab_educacionais,
                                                     levels = c(0, 1),
                                                     labels = c("Não", "Sim"))

baseind19$variables$hab_informação <- factor(baseind19$variables$hab_informação,
                                               levels = c(0, 1),
                                               labels = c("Não", "Sim"))

baseind19$variables$hab_visita_cultural <- factor(baseind19$variables$hab_visita_cultural,
                                             levels = c(0, 1),
                                             labels = c("Não", "Sim"))

baseind19$variables$ocup_informatizada <- factor(baseind19$variables$ocup_informatizada,
                                                  levels = c(0, 1),
                                                  labels = c("Não", "Sim"))
baseind19$variables$hab_museu <- factor(baseind19$variables$hab_museu,
                                                 levels = c(0, 1),
                                                 labels = c("Não", "Sim"))
baseind19$variables$hab_audiovisual <- factor(baseind19$variables$hab_audiovisual,
                                                 levels = c(0, 1),
                                                 labels = c("Não", "Sim"))
baseind19$variables$hab_musica <- factor(baseind19$variables$hab_musica,
                                              levels = c(0, 1),
                                              labels = c("Não", "Sim"))

baseind19$variables$hab_games <- factor(baseind19$variables$hab_games,
                                             levels = c(0, 1),
                                             labels = c("Não", "Sim"))

summary(baseind19$variables$hab_games)


###############Converte o tipo de todas as variáveis que possuem label para factor#####################
baseind19$variables <- as_factor(baseind19$variables,
                               only_labelled = "TRUE", 
                               levels = "TRUE", 
                               ordered = "FALSE")   

baseind19acessointernet$variables <- as_factor(baseind19acessointernet$variables,
                                 only_labelled = "TRUE", 
                                 levels = "default", 
                                 ordered = "FALSE") 

#verifica a natureza das variaiveis; verifica se todas já estão em factor 
glimpse(baseind19acessointernet$variables)
glimpse(baseind19$variables)

###############Realizar soma dos pesos###################################
#este processo nos responde com o total da amostra com os pesos considerados 
#processo importante para endenter os numeros das analises a diante 

sum(baseind19$variables$PESO)
sum(baseind19acessointernet$variables$PESO)

###############verifica a criação de tabelas iguais a do site###################
#calcular o indicador C2 do site: usuario de internet por ultimo uso 
str(baseind19$variables$C3)
#sem desenho amostral 
table(baseind19$variables$C3)
table(baseind19$variables$PEA_2)
#com desenho amostral: bater com o do site 
C2_PEA_PROP <- svyby(~C3, ~PEA_2, design = baseind19, svymean)
c2_PEA_PROP <- round(svyby(~C3, ~PEA_2, design = baseind19, svymean)*100,
                     digitis=1)


###############estatisticas descritivas com total da população#####################
#IMPORTANTE: Proporções e os respectivos erros amostrais: usamos a FUN svymean.
#IMPORTANTE: Totais populacionais e os respectivos erros amostrais: usamos a FUN svytotal.
#IMPORTANTE: P/ análises de variaveis categoricas/fatores, usamos a função sbyby

#calcular proporcoes da população por variaveis selecionadas e variavel de cruzamento, exemplos:
#somar o total de cada variaveis utilizada 
svytotal(~ SEXO, baseind19)
svymean(~ SEXO, baseind19acessointernet)
svytotal(~ RACA, baseind19)
svymean(~ RACA, baseind19acessointernet)
svytotal(~ GRAU_INSTRUCAO_2, baseind19)
svymean(~ GRAU_INSTRUCAO_2, baseind19acessointernet)
svytotal(~ CLASSE_CB2015, baseind19)
svymean(~ CLASSE_CB2015, baseind19acessointernet)
svytotal(~ FAIXA_ETARIA, baseind19)
svymean(~ FAIXA_ETARIA, baseind19acessointernet)
svytotal(~ AREA, baseind19)
svymean(~ AREA, baseind19acessointernet)

#habitos culturais gerais 
svymean(~ hab_culturais_gerais_modelo_modelo, baseind19)
svyby(~ hab_culturais_gerais_modelo, ~ SEXO, design = baseind19, svymean)
svyby(~ hab_culturais_gerais_modelo, ~ RACA, design = baseind19, svymean)
svyby(~ hab_culturais_gerais_modelo, ~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_culturais_gerais_modelo, ~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_culturais_gerais_modelo, ~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_culturais_gerais_modelo, ~ AREA, design = baseind19, svymean)

#habitos culturais gratuitos 
svymean(~ hab_culturais_gratuitos_modelo, baseind19)
svyby(~ hab_culturais_gratuitos_modelo, ~ SEXO, design = baseind19, svymean)
svyby(~ hab_culturais_gratuitos_modelo, ~ RACA, design = baseind19, svymean)
svyby(~ hab_culturais_gratuitos_modelo, ~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_culturais_gratuitos_modelo, ~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_culturais_gratuitos_modelo, ~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_culturais_gratuitos_modelo, ~ AREA, design = baseind19, svymean)

#habitos culturais pagos 
svymean(~ hab_culturais_pagos_modelo_modelo, baseind19)
svyby(~ hab_culturais_pagos_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_culturais_pagos_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_culturais_pagos_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_culturais_pagos_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_culturais_pagos_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_culturais_pagos_modelo,~ AREA, design = baseind19, svymean)

#habitos culturais criação
svymean(~ hab_criacao_cultural_modelo_, baseind19)
svyby(~ hab_criacao_cultural_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_criacao_cultural_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_criacao_cultural_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_criacao_cultural_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_criacao_cultural_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_criacao_cultural_modelo,~ AREA, design = baseind19, svymean)

#habitos culturais pesquisa de informações culturais 
svymean(~ hab_visita_cultural, baseind19)
svyby(~ hab_visita_cultural_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_visita_cultural,~ RACA, design = baseind19, svymean)
svyby(~ hab_visita_cultural,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_visita_cultural,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_visita_cultural,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_visita_cultural,~ FAIXA_ETARIA, design = baseind19, svymean)

#habitos culturais: games 
svymean(~ hab_games_modelo, baseind19)
svyby(~ hab_games_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_games_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_games_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_games_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_games_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_games_modelo,~ AREA, design = baseind19, svymean)

#habitos culturais: música  
svymean(~ hab_musica_modelo, baseind19)
svyby(~ hab_musica_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_musica_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_musica_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_musica_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_musica_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_musica_modelo,~ AREA, design = baseind19, svymean)

#habitos culturais: audiovisual  
svymean(~ hab_audiovisual_modelo, baseind19)
svyby(~ hab_audiovisual_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_audiovisual_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_audiovisual_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_audiovisual_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_audiovisual_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_audiovisual_modelo,~ AREA, design = baseind19, svymean)

#habitos culturais: museu  
svymean(~ hab_museu_modelo, baseind19, na.rm = TRUE)
svyby(~ hab_museu_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_museu_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_museu_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_museu_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_museu_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_museu_modelo,~ AREA, design = baseind19, svymean)

#habitos de informação e notícias 
svymean(~ hab_informação_modelo, baseind19)
svyby(~ hab_informação_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_informação_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_informação_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_informação_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_informação_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)
svyby(~ hab_informação_modelo,~ AREA, design = baseind19, svymean)

#habitos de comunicacao e social
svymean(~ hab_comunicacao_social_modelo, baseind19)
svyby(~ hab_comunicacao_social,~ SEXO, design = baseind19, svymean)
svyby(~ hab_comunicacao_social,~ RACA, design = baseind19, svymean)
svyby(~ hab_comunicacao_social,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_comunicacao_social,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_comunicacao_social,~ FAIXA_ETARIA, design = baseind19, svymean)

#habitos educacionais
svymean(~ hab_educacionais_modelo, baseind19)
svyby(~ hab_educacionais,~ SEXO, design = baseind19, svymean)
svyby(~ hab_educacionais,~ RACA, design = baseind19, svymean)
svyby(~ hab_educacionais,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_educacionais,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_educacionais,~ FAIXA_ETARIA, design = baseind19, svymean)

#habitos ocupações informatizadas 
svymean(~ ocup_informatizada, baseind19)
svyby(~ ocup_informatizada,~ SEXO, design = baseind19, svymean)
svyby(~ ocup_informatizada,~ RACA, design = baseind19, svymean)
svyby(~ ocup_informatizada,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ ocup_informatizada,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ ocup_informatizada,~ FAIXA_ETARIA, design = baseind19, svymean)

#calcular o total de variaveis da base:
svytotal(~ hab_culturais_gratuitos, baseind19)
svytotal(~ AREA, baseind19)
svytotal(~ AREA, baseind19acessointernet)

###############estatisticas descritivas com  população c acesso a internet#####################
baseind19acessointernet <- baseind19[baseind19$variables$C3J3 == "Sim",]

#somar o total de cada variaveis utilizada 
svytotal(~ SEXO, baseind19acessointernet)
svytotal(~ RACA, baseind19acessointernet)
svytotal(~ GRAU_INSTRUCAO_2, baseind19acessointernet)
svytotal(~ CLASSE_CB2015, baseind19acessointernet)
svytotal(~ FAIXA_ETARIA, baseind19acessointernet)
sum(baseind19acessointernet$variables$PESO)

summary(baseind19acessointernet$variables$hab_culturais_pagos)

summmarise(baseind19$variables$SEXO, baseind19$variables$hab_culturais_pagos)


#habitos culturais gerais 
svyby(~ hab_culturais_gerais_modelo, ~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ AREA, design = baseind19acessointernet, svymean)


#habitos culturais gratuitos 
svyby(~ hab_culturais_gratuitos_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais pagos 
svyby(~ hab_culturais_pagos_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais criação 
svyby(~ hab_criacao_cultural_modelo, ~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)

#habitos culturais pesquisa de informações culturais 
svyby(~ hab_visita_cultural,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)

#habitos culturais: games 
svyby(~ hab_games_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ GRAU_INSTRUCAO_2,design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais: música  
svyby(~ hab_musica_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_musica_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_musica_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_musica_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_musica_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_musica_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais: audiovisual  
svyby(~ hab_audiovisual_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais: museu  
svyby(~ hab_museu_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_museu_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_museu_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_museu_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_museu_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_museu_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos de informação e notícias 
svyby(~ hab_informação_modelo, ~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_informação_modelo, ~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_informação_modelo, ~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_informação_modelo, ~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_informação_modelo, ~ FAIXA_ETARIA,design = baseind19acessointernet, svymean)
svyby(~ hab_informação_modelo, ~ AREA,design = baseind19acessointernet, svymean)

#habitos de comunicacao e social
svyby(~ hab_comunicacao_social_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_comunicacao_social_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_comunicacao_social_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_comunicacao_social_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_comunicacao_social_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_comunicacao_social_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos educacionais
svyby(~ hab_educacionais_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_educacionais_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_educacionais_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_educacionais_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_educacionais_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_educacionais_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos ocupações informatizadas 
svyby(~ ocup_informatizada_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada_modelo,~ AREA, design = baseind19acessointernet, svymean)

################analisando hipoteses do ref teorico###############

#FREQUENCIA DE MUSICA por classe/ mmusica
round(svyby(~ TC1, ~ CLASSE_CB2015, design = baseind19acessointernet, svymean)*100,
      digits = 1)


#FREQUENCIA DE FILMES por classe/filme 
round(svyby(~ TC5_A, ~ CLASSE_CB2015, design = baseind19acessointernet, svymean)*100,
      digits = 1)

#FREQUENCIA DE SERIES por classe/serie 
round(svyby(~ TC5_B, ~ CLASSE_CB2015,design = baseind19acessointernet, svymean)*100,
      digits = 1)

#FREQUENCIA DE FILMES por classe/musica base total 
round(svyby(~ TC1, ~ CLASSE_CB2015, design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE FILMES por classe/filme base total 
round(svyby(~ TC5_A, ~ CLASSE_CB2015, design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE SERIES por classe/serie base total
round(svyby(~ TC5_B, ~ CLASSE_CB2015,design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE MUSICA por PEA com categorias 
testeMUSICA <- round(svyby(~ TC1, ~ PEA, design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE FILMES por PEA com categorias 
testeFILME <- round(svyby(~ TC5_A, ~ PEA, design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE SERIES por PEA com categorias 
testeSERIE <- round(svyby(~ TC5_B, ~ PEA, design = baseind19, svymean)*100,
      digits = 1)

#FREQUENCIA DE SERIES por PEA com 2 categorias 
testeMUSICAnet <- round(svyby(~ TC1, ~ PEA_2, design = baseind19acessointernet, svymean)*100,
                     digits = 1)

testeFILMEnet <- round(svyby(~ TC5_A, ~ PEA_2, design = baseind19acessointernet, svymean)*100,
                    digits = 1)

testeSERIEnet <- round(svyby(~ TC5_B, ~ PEA_2, design = baseind19acessointernet, svymean)*100,
                    digits = 1)

#FREQUENCIA DE SERIES por SEXO com 2 categorias COM INTERNET 

testeMUSICAnet <- round(svyby(~ TC1, ~ SEXO, design = baseind19acessointernet, svymean)*100,
                        digits = 1)

testeFILMEnet <- round(svyby(~ TC5_A, ~ SEXO, design = baseind19acessointernet, svymean)*100,
                       digits = 1)

testeSERIEnet <- round(svyby(~ TC5_B, ~ SEXO, design = baseind19acessointernet, svymean)*100,
                       digits = 1)

#FREQUENCIA DE SERIES por SEXO com 2 categorias POP GERAL
testeMUSICA <- round(svyby(~ TC1, ~ SEXO, design = baseind19, svymean)*100,
                        digits = 1)

testeFILME <- round(svyby(~ TC5_A, ~ SEXO, design = baseind19, svymean)*100,
                       digits = 1)

testeSERIE <- round(svyby(~ TC5_B, ~ SEXO, design = baseind19, svymean)*100,
                       digits = 1)




################Ajustando variaveis p/ modelo#################
#Sexo ok 
baseind19acessointernet <- update(baseind19acessointernet,
                                  SexoMod = ifelse(SEXO=="Masculino", "Masculino", "Feminino"))

baseind19acessointernet$variables$SexoMod<- factor(baseind19acessointernet$variables$SexoMod,
                                                   levels = c("Masculino", "Feminino"))

#Faixa etaria ok 
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaCriançamod = ifelse(FAIXA_ETARIA=="De 10 a 15 anos", "Criança", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaJovemmod = ifelse(FAIXA_ETARIA=="De 16 a 24 anos", "Jovem", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaJovemAdultomod = ifelse(FAIXA_ETARIA=="De 25 a 34 anos", "Jovem Adulto", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaAdultamod = ifelse(FAIXA_ETARIA=="De 35 a 44 anos", "Adulto", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaAdulta2mod = ifelse(FAIXA_ETARIA=="De 45 a 59 anos", "Adulto2", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  FaixaIdosomod = ifelse(FAIXA_ETARIA=="60 anos ou mais", "Idoso", "Outros"))

baseind19acessointernet$variables$FaixaCriançamod<- factor(baseind19acessointernet$variables$FaixaCriançamod,
                                                           levels = c("Criança", "Outros"))
baseind19acessointernet$variables$FaixaJovemmod<- factor(baseind19acessointernet$variables$FaixaJovemmod,
                                                         levels = c("Jovem", "Outros"))
baseind19acessointernet$variables$FaixaJovemAdultomod<- factor(baseind19acessointernet$variables$FaixaJovemAdultomod,
                                                          levels = c("Jovem Adulto", "Outros"))
baseind19acessointernet$variables$FaixaAdultamod<- factor(baseind19acessointernet$variables$FaixaAdultamod,
                                                               levels = c("Adulto", "Outros"))
baseind19acessointernet$variables$FaixaAdulta2mod<- factor(baseind19acessointernet$variables$FaixaAdulta2mod,
                                                          levels = c("Adulto2", "Outros"))
baseind19acessointernet$variables$FaixaIdosomod<- factor(baseind19acessointernet$variables$FaixaIdosomod,
                                                         levels = c("Idoso", "Outros"))
#Raca ok

baseind19acessointernet <- update(baseind19acessointernet,
                                  RacaModelo = case_when(RACA=="Preta"|RACA=="Parda" ~ "PretaParda",
                                                         RACA=="Branca"|RACA=="Amarela"|RACA=="Indígena" ~ "Outros",
                                                         TRUE ~ as.character(NA)))

baseind19acessointernet$variables$RacaModelo<- factor(baseind19acessointernet$variables$RacaModelo,
                                                   levels = c("PretaParda", "Outros"))

#Grau de instrução 
baseind19acessointernet <- update(baseind19acessointernet,
                                  Instrucao1Mod = ifelse(GRAU_INSTRUCAO_2=="Analfabeto / Educação infantil", "Analfabeto / Educação infantil", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  Instrucao2Mod = ifelse(GRAU_INSTRUCAO_2=="Fundamental", "Fundamental", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  Instrucao3Mod = ifelse(GRAU_INSTRUCAO_2=="Médio", "Médio", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  Instrucao4Mod = ifelse(GRAU_INSTRUCAO_2=="Superior", "Superior", "Outros"))

baseind19acessointernet$variables$Instrucao4Mod<- factor(baseind19acessointernet$variables$Instrucao4Mod,
                                                         levels = c("Superior", "Outros"))
baseind19acessointernet$variables$Instrucao3Mod<- factor(baseind19acessointernet$variables$Instrucao3Mod,
                                                         levels = c("Médio", "Outros"))
baseind19acessointernet$variables$Instrucao2Mod<- factor(baseind19acessointernet$variables$Instrucao2Mod,
                                                         levels = c("Fundamental", "Outros"))
baseind19acessointernet$variables$Instrucao1Mod<- factor(baseind19acessointernet$variables$Instrucao1Mod,
                                                         levels = c("Analfabeto / Educação infantil", "Outros"))

#Classe Social
baseind19acessointernet <- update(baseind19acessointernet,
                                  ClasseA = ifelse(CLASSE_CB2015=="Classe A", "Classe A", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  ClasseB = ifelse(CLASSE_CB2015=="Classe B", "Classe B", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  ClasseC = ifelse(CLASSE_CB2015=="Classe C", "Classe C", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  ClasseDE = ifelse(CLASSE_CB2015=="Classe D/ E", "Classe D/ E", "Outros"))

baseind19acessointernet$variables$ClasseA<- factor(baseind19acessointernet$variables$ClasseA,
                                                   levels = c("Classe A", "Outros"))
baseind19acessointernet$variables$ClasseB<- factor(baseind19acessointernet$variables$ClasseB,
                                                   levels = c("Classe B", "Outros"))
baseind19acessointernet$variables$ClasseC<- factor(baseind19acessointernet$variables$ClasseC,
                                                   levels = c("Classe C", "Outros"))
baseind19acessointernet$variables$ClasseDE<- factor(baseind19acessointernet$variables$ClasseDE,
                                                   levels = c("Classe D/ E", "Outros"))

#regiao 
baseind19acessointernet <- update(baseind19acessointernet,
                                  SUL = ifelse(COD_REGIAO_2=="Sul", "Sul", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  NORTE = ifelse(COD_REGIAO_2=="Norte", "Norte", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  NORDESTE = ifelse(COD_REGIAO_2=="Nordeste", "Nordeste", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  SUDESTE = ifelse(COD_REGIAO_2=="Sudeste", "Sudeste", "Outros"))
baseind19acessointernet <- update(baseind19acessointernet,
                                  CentroOeste = ifelse(COD_REGIAO_2=="Centro-Oeste", "Centro-Oeste", "Outros"))
baseind19acessointernet$variables$SUL<- factor(baseind19acessointernet$variables$SUL,
                                                    levels = c("Sul", "Outros"))
baseind19acessointernet$variables$NORTE<- factor(baseind19acessointernet$variables$NORTE,
                                               levels = c("Norte", "Outros"))
baseind19acessointernet$variables$NORDESTE<- factor(baseind19acessointernet$variables$NORDESTE,
                                                 levels = c("Nordeste", "Outros"))
baseind19acessointernet$variables$SUDESTE<- factor(baseind19acessointernet$variables$SUDESTE,
                                                    levels = c("Sudeste", "Outros"))
baseind19acessointernet$variables$CentroOeste<- factor(baseind19acessointernet$variables$CentroOeste,
                                                   levels = c("Centro-Oeste", "Outros"))


#hab habito cultural geral
baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_culturais_gerais_modelo = case_when(TC4C_D=="Sim"|TC4C_E=="Sim"|TC2=="Sim"|TC10_D=="Sim"|TC11_D=="Sim"|TC13_A=="Sim"| TC13_B=="Sim"|TC13_C=="Sim"|TC13_D=="Sim"| TC13_E=="Sim"| TC13_F=="Sim"| TC13_G=="Sim"|TC13_H=="Sim"|C12_C=="Sim"|TC4B_G=="Sim"|C9_B=="Sim"|C12_B=="Sim"|C9_C=="Sim"|C12_A=="Sim"|C9_F=="Sim"~ "Sim",
                                                                          TC4C_D=="Não"|TC4C_E=="Não"|TC2=="Não"|TC10_D=="Não"|TC11_D=="Não"|TC13_A=="Não"| TC13_B=="Não"|TC13_C=="Não"|TC13_D=="Não"| TC13_E=="Não"| TC13_F=="Não"| TC13_G=="Não"|TC13_H=="Não"|C12_C=="Não"|TC4B_G=="Não"|C9_B=="Não"|C12_B=="Não"|C9_C=="Não"|C12_A=="Não"|C9_F=="Não"~ "Não",
                                                                          TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica"|TC2=="Não se aplica"|TC10_D=="Não se aplica"|TC11_D=="Não se aplica"|TC13_A=="Não se aplica"| TC13_B=="Não se aplica"|TC13_C=="Não se aplica"|TC13_D=="Não se aplica"| TC13_E=="Não se aplica"| TC13_F=="Não se aplica"| TC13_G=="Não se aplica"|TC13_H=="Não se aplica"|C12_C=="Não se aplica"|TC4B_G=="Não"|C9_B=="Não se aplica"|C12_B=="Não se aplica"|C9_C=="Não se aplica"|C12_A=="Não se aplica"|C9_F=="Não se aplica"~ "Não",
                                                                          TRUE ~ as.character(NA)))

baseind19acessointernet$variables$hab_culturais_gerais_modelo<- factor(baseind19acessointernet$variables$hab_culturais_gerais_modelo,
                                                       levels = c("Sim", "Não"))

#hab habito cultural pagos
baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_culturais_pagos_modelo = case_when(TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                          TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                          TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                          TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_culturais_pagos_modelo<- factor(baseind19acessointernet$variables$hab_culturais_pagos_modelo,
                                                                       levels = c("Sim", "Não"))

#hab cultural gratuitos 
baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_culturais_gratuitos_modelo = case_when(C9_B=="Sim"|C9_C=="Sim"|C9_F=="Sim"|C12_A=="Sim"|C12_B=="Sim"|C12_E1=="Sim"|C12_F1=="Sim"|J2_C=="Sim"|TC3_A=="Sim"|TC3_B=="Sim"|TC4_A=="Sim"|TC4_B=="Sim"|TC4B_C=="Sim"|TC4B_D=="Sim"|TC4B_F=="Sim"|TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                             C9_B=="Não"|C9_C=="Não"|C9_F=="Não"|C12_A=="Não"|C12_B=="Não"|C12_E1=="Não"|C12_F1=="Não"|J2_C=="Não"|TC3_A=="Não"|TC3_B=="Não"|TC4_A=="Não"|TC4_B=="Não"|TC4B_C=="Não"|TC4B_D=="Não"|TC4B_F=="Não"|TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                             C9_B=="Não se aplica"|C9_C=="Não se aplica"|C9_F=="Não se aplica"|C12_A=="Não se aplica"|C12_B=="Não se aplica"|C12_E1=="Não se aplica"|C12_F1=="Não se aplica"|J2_C=="Não se aplica"|TC3_A=="Não se aplica"|TC3_B=="Não se aplica"|TC4_A=="Não se aplica"|TC4_B=="Não se aplica"|TC4B_C=="Não se aplica"|TC4B_D=="Não se aplica"|TC4B_F=="Não se aplica"|TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                             TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_culturais_gratuitos_modelo<- factor(baseind19acessointernet$variables$hab_culturais_gratuitos_modelo,
                                                                      levels = c("Sim", "Não"))

#hab ocupacao informatizada 
baseind19acessointernet <- update(baseind19acessointernet,
                                  ocup_informatizada_modelo = case_when(C10_F=="Sim"|C6_B=="Sim" ~ "Sim",
                                                                        C10_F=="Não"|C6_B=="Não" ~ "Não",
                                                                        C10_F=="Não se aplica"|C6_B=="Não se aplica" ~ "Não",
                                                                        TRUE ~ as.character(NA)))
baseind19acessointernet$variables$ocup_informatizada_modelo<- factor(baseind19acessointernet$variables$ocup_informatizada_modelo,
                                                                          levels = c("Sim", "Não"))

#hab computacionais

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_computacionais_modelo = case_when(I1_A=="Sim"|I1_B=="Sim"|I1_C=="Sim"|I1_D=="Sim"|I1_E=="Sim"|I1_F=="Sim"|I1_G=="Sim"|I1_H=="Sim"|I1_I =="Sim" ~ "Sim",
                                                                        I1_A=="Não"|I1_B=="Não"|I1_C=="Não"|I1_D=="Não"|I1_E=="Não"|I1_F=="Não"|I1_G=="Não"|I1_H=="Não"|I1_I =="Não" ~ "Não",
                                                                        I1_A=="Não se aplica"|I1_B=="Não se aplica"|I1_C=="Não se aplica"|I1_D=="Não se aplica"|I1_E=="Não se aplica"|I1_F=="Não se aplica"|I1_G=="Não se aplica"|I1_H=="Não se aplica"|I1_I =="Não se aplica" ~ "Não se aplica",
                                                                        TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_computacionais_modelo<- factor(baseind19acessointernet$variables$hab_computacionais_modelo,
                                                                     levels = c("Sim", "Não"))

#dispositivos

baseind19acessointernet <- update(baseind19acessointernet,
                                  dispositivos_modelo = case_when(C5_DISPOSITIVOS=="Apenas telefone celular" ~ "Apenas telefone celular",
                                                                  C5_DISPOSITIVOS=="Apenas computador" ~ "Outros",
                                                                  C5_DISPOSITIVOS=="Ambos" ~ "Outros",
                                                                  TRUE ~ as.character(NA)))
baseind19acessointernet$variables$dispositivos_modelo  <- factor(baseind19acessointernet$variables$dispositivos_modelo ,
                                                                     levels = c("Apenas telefone celular", "Outros"))

#MUSICA

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_musica_modelo = case_when(C9_B=="Sim"| C12_B == "Sim" | TC2 == "Sim" ~ "Sim",
                                                                  C9_B=="Não"| C12_B == "Não" | TC2 == "Não" ~ "Não",
                                                                  C9_B=="Não se aplica"| C12_B == "Não se aplica" | TC2 == "Não se aplica" ~ "Não",
                                                                  TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_musica_modelo  <- factor(baseind19acessointernet$variables$hab_musica_modelo,
                                                                 levels = c("Não", "Sim"))

#audiovisual

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_audiovisual_modelo = case_when(C9_C=="Sim"|C12_A=="Sim"|TC4C_D =="Sim"|TC4C_E =="Sim" ~ "Sim",
                                                                     C9_C=="Não"|C12_A=="Não"|TC4C_D =="Não"|TC4C_E =="Sim" ~ "Não",
                                                                     C9_C=="Não se aplica"|C12_A=="Não se aplica"|TC4C_D =="Não se aplica"|TC4C_E =="Não se aplica" ~ "Não",
                                                                     TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_audiovisual_modelo  <- factor(baseind19acessointernet$variables$hab_audiovisual_modelo ,
                                                                 levels = c("Não", "Sim"))

#museu

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_museu_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não "|C12_C =="Não se aplica" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_museu_modelo  <- factor(baseind19acessointernet$variables$hab_museu_modelo ,
                                                                 levels = c("Não", "Sim"))

#JOGOS

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_games_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não"|C12_C =="Não" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_games_modelo  <- factor(baseind19acessointernet$variables$hab_games_modelo ,
                                                              levels = c("Não", "Sim"))

#hab pesquisa total


$hab criacao total


#hab info

baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_info_modelo = case_when(TC4B_A =="Sim"|C9_D =="Sim" ~ "Sim",
                                                               TC4B_A =="Não se aplica"|C9_D =="Não se aplica" ~ "Não",
                                                               TC4B_A =="Não"|C9_D =="Não" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_info_modelo  <- factor(baseind19acessointernet$variables$hab_info_modelo ,
                                                              levels = c("Não", "Sim"))

#hab comu


baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_comunica_modelo = case_when(C7_B =="Sim"|C7_C =="Sim" |C7_D =="Sim"|C7_F=="Sim"|J2_B=="Sim"|J2_M=="Sim"|J2_N=="Sim"|J2_I=="Sim" ~ "Sim",
                                                              C7_B =="Não se aplica"|C7_C =="Não se aplica" |C7_D =="Não se aplica"|C7_F=="Não se aplica"|J2_B=="Não se aplica"|J2_M=="Não se aplica"|J2_N=="Não se aplica"|J2_I=="Não se aplica" ~ "Não",
                                                              C7_B =="Não"|C7_C =="Não" |C7_D =="Não"|C7_F=="Não"|J2_B=="Não"|J2_M=="Não"|J2_N=="Não"|J2_I=="Não" ~ "Não",
                                                              TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_comunica_modelo  <- factor(baseind19acessointernet$variables$hab_comunica_modelo ,
                                                             levels = c("Não", "Sim"))

#hab edc
baseind19acessointernet <- update(baseind19acessointernet,
                                  hab_educ_modelo = case_when(C10_A =="Sim"|C10_B =="Sim" |C10_D =="Sim"|TC4B_H=="Sim" ~ "Sim",
                                                              C10_A =="Não se aplica"|C10_B =="Não se aplica" |C10_D =="Não se aplica"|TC4B_H=="Não se aplica" ~ "Não",
                                                              C10_A =="Não"|C10_B =="Não" |C10_D =="Não"|TC4B_H=="Não" ~ "Não",
                                                              TRUE ~ as.character(NA)))
baseind19acessointernet$variables$hab_educ_modelo  <- factor(baseind19acessointernet$variables$hab_educ_modelo,
                                                                 levels = c("Não", "Sim"))



######VERIFICANDO AS VARIAVEIS###### 
sum(baseind19acessointernet$variables$PESO)
sum(baseind19$variables$PESO)
summary(baseind19acessointernet$variables$RACA)
svytotal(~RacaMod, baseind19acessointernet)
summary(baseind19acessointernet$variables$FaixaCriançamod)
svytotal(~FaixaCriançamod, baseind19acessointernet)
summary(baseind19acessointernet$variables$GRAU_INSTRUCAO_2)
summary(baseind19acessointernet$variables$CLASSE_CB2015)
summary(baseind19acessointernet$variables$ocup_informatizada)
summary(baseind19acessointernet$variables$FAIXA_ETARIA)
summary(baseind19acessointernet$variables$COD_REGIAO_2)
summary(baseind19acessointernet$variables$TC2)
summary(baseind19acessointernet$variables$TC4C_D)
summary(baseind19acessointernet$variables$TC4C_E)
summary(baseind19acessointernet$variables$RacaModelo)
summary(baseind19acessointernet$variables$hab_culturais_gerais_modelo)
summary(baseind19acessointernet$variables$hab_culturais_gerais)
summary(baseind19acessointernet$variables$hab_culturais_pagos_modelo)
summary(baseind19acessointernet$variables$hab_culturais_pagos)
summary(baseind19acessointernet$variables$hab_culturais_gratuitos_modelo)
summary(baseind19acessointernet$variables$hab_culturais_gratuitos)
summary(baseind19acessointernet$variables$ocup_informatizada_modelo)
summary(baseind19acessointernet$variables$hab_computacionais_modelo)
summary(baseind19acessointernet$variables$hab_computacionais_modelo)
summary(baseind19acessointernet$variables$dispositivos_modelo)
summary(baseind19acessointernet$variables$hab_musica_modelo)
summary(baseind19acessointernet$variables$hab_audiovisual_modelo)
summary(baseind19acessointernet$variables$hab_museu_modelo)

######VERIFICANDO AS VARIAVEIS em estatistica descritiva###### 

#somar o total de cada variaveis utilizada 
svymean(~ ocup_informatizada_modelo, baseind19acessointernet)
svymean(~ ocup_informatizada_modelo, baseind19acessointernet)

svymean(~ RACA, baseind19acessointernet)
svymean(~ GRAU_INSTRUCAO_2, baseind19acessointernet)
svymean(~ CLASSE_CB2015, baseind19acessointernet)

svymean(~ CLASSE_CB2015, baseind19acessointernet)


sum(baseind19acessointernet$variables$PESO)
summary(baseind19acessointernet$variables$SEXO)

summmarise(baseind19$variables$SEXO, baseind19$variables$SEXO)


#habitos culturais gerais 
svymean(~ hab_culturais_gerais_modelo, baseind19acessointernet)
svyby(~ hab_culturais_gerais_modelo, ~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ Instrucao4Mod, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ FaixaIdosomod , design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gerais_modelo,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais gratuitos 
svymean(~ hab_culturais_gratuitos, baseind19acessointernet)
svyby(~ hab_culturais_gratuitos,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_gratuitos,~ AREA, design = baseind19acessointernet, svymean)


#habitos culturais pagos 
svymean(~ hab_culturais_pagos, baseind19acessointernet)
svyby(~ hab_culturais_pagos,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_culturais_pagos,~ AREA, design = baseind19acessointernet, svymean)


#habitos culturais criação 
svymean(~ hab_criacao_cultural, baseind19acessointernet)
svymean(~ hab_criacao_cultural, baseind19, na.rm = TRUE)
svyby(~ hab_criacao_cultural,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_criacao_cultural,~ FAIXA_ETARIA, design = baseind19acessointernet, mean)
svyby(~ hab_criacao_cultural,~ AREA, design = baseind19acessointernet, svymean)

#habitos culturais pesquisa de informações culturais 
svymean(~ hab_visita_cultural, baseind19acessointernet)
svyby(~ hab_visita_cultural,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_visita_cultural,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)

#habitos culturais: games 
svymean(~ hab_games, baseind19acessointernet, na.rm = TRUE)
svyby(~ hab_games,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_games,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_games,~ GRAU_INSTRUCAO_2,design = baseind19acessointernet, svymean)
svyby(~ hab_games,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_games,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_games_modelo,~ AREA, design = baseind19acessointernet, svymean)


#habitos culturais: música  
svymean(~ hab_musica_modelo, baseind19acessointernet, na.rm = TRUE)
svyby(~ hab_musica_modelo,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_musica,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_musica,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_musica,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_musica,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)

#habitos culturais: audiovisual  
svymean(~ hab_audiovisual, baseind19acessointernet, na.rm = TRUE)
svyby(~ hab_audiovisual,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_audiovisual,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)

#habitos culturais: museu  
svymean(~ hab_museu, baseind19acessointernet, na.rm = TRUE)
svyby(~ hab_museu,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ hab_museu,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ hab_museu,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ hab_museu,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ hab_museu,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)
svyby(~ hab_museu,~ AREA, design = baseind19acessointernet, svymean)

#habitos ocupações informatizadas 
svymean(~ ocup_informatizada_modelo, design = baseind19,  na.rm = TRUE)
svymean(~ ocup_informatizada, design = baseind19acessointernet,  na.rm = TRUE)
svyby(~ ocup_informatizada,~ SEXO, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada,~ RACA, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada,~ CLASSE_CB2015, design = baseind19acessointernet, svymean)
svyby(~ ocup_informatizada,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean)


#habitos ocupações dispos utilizado 
svymean(~ dispositivos_modelo, baseind19acessointernet, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ SEXO, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ RACA, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ AREA, design = baseind19acessointernet, svymean, na.rm = TRUE)

svyby(~ dispositivos_modelo,~ SEXO, design = baseind19, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ RACA, design = baseind19, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ CLASSE_CB2015, design = baseind19, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ FAIXA_ETARIA, design = baseind19, svymean, na.rm = TRUE)
svyby(~ dispositivos_modelo,~ AREA, design = baseind19, svymean, na.rm = TRUE)


#habitos ocupações habuilidades 
svymean(~ hab_computacionais_modelo, baseind19acessointernet, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ SexoMod, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ RACA, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ GRAU_INSTRUCAO_2, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ CLASSE_CB2015, design = baseind19acessointernet, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ FAIXA_ETARIA, design = baseind19acessointernet, svymean, na.rm = TRUE)


#habitos ocupações habuilidades 
svymean(~ hab_computacionais_modelo, design = baseind19, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ RACA, design = baseind19, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ CLASSE_CB2015, design = baseind19, svymean, na.rm = TRUE)
svyby(~ hab_computacionais_modelo,~ FAIXA_ETARIA, design = baseind19, svymean, na.rm = TRUE)


svymean(~ hab_info_modelo, baseind19acessointernet, na.rm = TRUE)
svymean(~ hab_comunica_modelo, baseind19acessointernet, na.rm = TRUE)
svymean(~ hab_educ_modelo, baseind19acessointernet, na.rm = TRUE)
svymean(~ hab_criacao_cultural, baseind19, na.rm = TRUE)

##############Rodando modelo#################
#hab cultural geral
reg.culturageral<- svyglm(hab_culturais_gerais_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )
reg.culturageral2<- svyglm(hab_culturais_gerais_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )


#hab cultural pago
reg.culturapago<- svyglm(hab_culturais_pagos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#hab cultural gratuito
reg.culturagratuito<- svyglm(hab_culturais_gratuitos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )


#hab cultural musica
reg.musica <- svyglm(hab_musica_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )
stargazer(reg.musica, title = "Habitos de consumo musical", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#hab cultural audiovisual
reg.audiovisual <- svyglm(hab_audiovisual_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )
stargazer(reg.audiovisual, title = "Habitos de consumo audiovisual", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#hab cultural museu
reg.museu <- svyglm(hab_museu_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )
stargazer(reg.museu, title = "Habitos de consumo museu", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

vif(reg.museu)

#hab cultural games
reg.games <- svyglm(hab_games_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )
stargazer(reg.games, title = "Habitos de consumo games", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

######VERIFICANDO CATEGORIA DE REFERENCIA######
#Implica na interpretacao de razao de chance
levels(baseind19acessointernet$variables$SEXO)
levels(baseind19acessointernet$variables$RACA)
levels(baseind19acessointernet$variables$RacaMod)
levels(baseind19acessointernet$variables$FaixaIdosomod)
levels(baseind19acessointernet$variables$FaixaCriançamod)
levels(baseind19acessointernet$variables$FaixaJovemmod)
levels(baseind19acessointernet$variables$FaixaAdultamod)
levels(baseind19acessointernet$variables$FaixaIdosomod)
levels(baseind19acessointernet$variables$Sexomod)
levels(baseind19acessointernet$variables$RacaMod)
levels(baseind19acessointernet$variables$Instrucao1Mod)
levels(baseind19acessointernet$variables$Instrucao2Mod)
levels(baseind19acessointernet$variables$Instrucao3Mod)
levels(baseind19acessointernet$variables$Instrucao4Mod)
levels(baseind19acessointernet$variables$ocup_informatizada_modelo)
levels(baseind19acessointernet$variables$dispositivos_modelo)
levels(baseind19acessointernet$variables$hab_computacionais_modelo)
levels(baseind19acessointernet$variables$AREA)
levels(baseind19acessointernet$variables$ClasseA)
levels(baseind19acessointernet$variables$ClasseB)
levels(baseind19acessointernet$variables$ClasseC)
levels(baseind19acessointernet$variables$ClasseD)
levels(baseind19acessointernet$variables$PEA_2)
levels(baseind19acessointernet$variables$NORTE)
levels(baseind19acessointernet$variables$SUL)
levels(baseind19acessointernet$variables$SUDESTE)
levels(baseind19acessointernet$variables$CentroOeste)
levels(baseind19acessointernet$variables$NORDESTE)
levels(baseind19acessointernet$variables$hab_culturais_gerais_modelo)
levels(baseind19acessointernet$variables$hab_culturais_gerais)
levels(baseind19acessointernet$variables$hab_culturais_pagos_modelo)
levels(baseind19acessointernet$variables$hab_culturais_gratuitos_modelo)
levels(baseind19acessointernet$variables$hab_audiovisual_modelo)
levels(baseind19acessointernet$variables$hab_musica_modelo)
levels(baseind19acessointernet$variables$hab_museu_modelo)
levels(baseind19acessointernet$variables$hab_games_modelo)

levels(baseind19acessointernet$variables$NORDESTE)

levels(baseind19acessointernet$variables$ocup_informatizada_modelo)

levels(baseind19acessointernet$variables$RacaModelo)

#invertendo o level

#independentes
baseind19acessointernet$variables$hab_culturais_gerais_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_gerais_modelo, ref = "Não")
baseind19acessointernet$variables$hab_culturais_pagos_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_pagos_modelo, ref = "Não")
baseind19acessointernet$variables$hab_culturais_gratuitos_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_gratuitos_modelo, ref = "Não")


#classe
baseind19acessointernet$variables$ClasseA <- relevel(baseind19acessointernet$variables$ClasseA, ref = "Outros")
baseind19acessointernet$variables$ClasseB <- relevel(baseind19acessointernet$variables$ClasseB, ref = "Outros")
baseind19acessointernet$variables$ClasseC <- relevel(baseind19acessointernet$variables$ClasseC, ref = "Outros")
baseind19acessointernet$variables$ClasseDE <- relevel(baseind19acessointernet$variables$ClasseDE, ref = "Outros")

#instrucao 

baseind19acessointernet$variables$Instrucao1Mod <- relevel(baseind19acessointernet$variables$Instrucao1Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao2Mod <- relevel(baseind19acessointernet$variables$Instrucao2Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao3Mod <- relevel(baseind19acessointernet$variables$Instrucao3Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao4Mod <- relevel(baseind19acessointernet$variables$Instrucao4Mod, ref = "Outros")

#faixa etaria
baseind19acessointernet$variables$FaixaCriançamod <- relevel(baseind19acessointernet$variables$FaixaCriançamod, ref = "Outros")
baseind19acessointernet$variables$FaixaJovemmod <- relevel(baseind19acessointernet$variables$FaixaJovemmod, ref = "Outros")
baseind19acessointernet$variables$FaixaJovemAdultomod <- relevel(baseind19acessointernet$variables$FaixaJovemAdultomod, ref = "Outros")
baseind19acessointernet$variables$FaixaAdultamod <- relevel(baseind19acessointernet$variables$FaixaAdultamod, ref = "Outros")
baseind19acessointernet$variables$FaixaAdulta2mod <- relevel(baseind19acessointernet$variables$FaixaAdulta2mod, ref = "Outros")
baseind19acessointernet$variables$FaixaIdosomod <- relevel(baseind19acessointernet$variables$FaixaIdosomod, ref = "Outros")

#rEGIAO
baseind19acessointernet$variables$SUDESTE <- relevel(baseind19acessointernet$variables$SUDESTE, ref = "Outros")
baseind19acessointernet$variables$SUL <- relevel(baseind19acessointernet$variables$SUL, ref = "Outros")
baseind19acessointernet$variables$NORTE <- relevel(baseind19acessointernet$variables$NORTE, ref="Outros")
baseind19acessointernet$variables$NORDESTE <- relevel(baseind19acessointernet$variables$NORDESTE, ref="Outros")
baseind19acessointernet$variables$CentroOeste <- relevel(baseind19acessointernet$variables$CentroOeste, ref = "Outros")

baseind19acessointernet$variables$dispositivos_modelo <- relevel(baseind19acessointernet$variables$dispositivos_modelo, ref = "Outros")

baseind19acessointernet$variables$hab_computacionais_modelo <- relevel(baseind19acessointernet$variables$hab_computacionais_modelo, ref = "Não")

baseind19acessointernet$variables$ocup_informatizada_modelo <- relevel(baseind19acessointernet$variables$ocup_informatizada_modelo, ref = "Não")

baseind19acessointernet$variables$RacaModelo <- relevel(baseind19acessointernet$variables$RacaModelo, ref = "Outros")

#########verificacao e outliers/pontos de alavancagem#####
##grafico dos residuos padronizados, no grafico nao pode ter observacao abaixo da linha pontilhada. 
##no summary a literatura recomenda estar dentro do -3 e +3
plot(reg.culturageral, which = 5)
summary(stdres(reg.culturageral))
plot(1:300)

plot(reg.culturagratuito, which = 5)
summary(stdres(reg.culturagratuito))


plot(reg.culturapago, which = 5)
summary(stdres(reg.culturapago))


plot(reg.audiovisual , which = 5)
summary(stdres(reg.audiovisual))

plot(reg.museu, which = 5)
summary(stdres(reg.museu))

plot(reg.games, which = 5)
summary(stdres(reg.musica))

plot(reg.audiovisual, which = 5)
summary(stdres(reg.audiovisual))
summary(stdres(reg.games))



#######verificacao de multicolineariedade, onde r > 0.9 (ou 0.8)#######
cor(baseind19acessointernet$variables$RacaMod, baseind19acessointernet$variables$SexoMod)
cor(reg.culturageral)

#Outra opção é utilizar o fator de inflação da variância (Variance inflation factor - VIF). Indica multi com VIF > 10 
vif(reg.culturageral)

vif(reg.culturagratuito)

vif(reg.culturapago)

vif(reg.museu)

vif(reg.musica)

vif(reg.audiovisual)

car::vif(reg.audiovisual)
###### Analise dos modelos######
##### Overhall effects 

anova(reg.culturageral, test.statistic="Wald")
anova(reg.culturapago, test.statistic="Wald")
anova(reg.culturagratuito, test.statistic="Wald")
anova(reg.audiovisual, test.statistic="Wald")
anova(reg.musica, test.statistic="Wald")
anova(reg.games, test.statistic="Wald")
anova(reg.museu, test.statistic="Wald")


knitr::kable(summary(reg.culturageral)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
stargazer(reg.culturageral, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.culturageral, vifs = TRUE, AIC = TRUE)


knitr::kable(summary(reg.culturapago)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.culturapago)
stargazer(reg.culturapago, title = "Habitos culturais pagos", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.culturapago, vifs = TRUE, AIC = TRUE)
AIC(reg.culturapago)


knitr::kable(summary(reg.culturagratuito)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.culturagratuito)
stargazer(reg.culturagratuito, title = "Habitos culturais gratuito", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.culturagratuito, vifs = TRUE, AIC = TRUE)
AIC(reg.culturagratuito)

knitr::kable(summary(reg.musica)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.musica)
stargazer(reg.musica, title = "Habitos culturais musica", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.musica, vifs = TRUE, AIC = TRUE)
AIC(reg.musica)

knitr::kable(summary(reg.audiovisual)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.audiovisual)
stargazer(reg.audiovisual, title = "Habitos culturais audiovisual", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.audiovisual, vifs = TRUE, AIC = TRUE)
AIC(reg.audiovisual)

knitr::kable(summary(reg.museu)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.audiovisual)
stargazer(reg.museu, title = "Habitos culturais museu", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.museu, vifs = TRUE, AIC = TRUE)
AIC(reg.museu)
vif(reg.museu)

knitr::kable(summary(reg.games)$coefficients,booktabs=TRUE, digits = c(3,3,3,2))
summary(reg.games)
stargazer(reg.museu, title = "Habitos culturais museu", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
summ(reg.games, vifs = TRUE, AIC = TRUE)
AIC(reg.games)
vif(reg.games)

######CONFIANCA DO MODELO#####
#De notar que a função confint.default() utilizada através do package MASS gera I.C.´s através da estatística de Wald enquanto que a função confint() gera os valores para os limites dos I.C.´s através da verossimelhança. 
#Wald
confint.default(reg.culturapago)

#Verossimilhanca 
confint(reg.culturapago)
exp(confint(reg.culturageral))

##### obtenção das razões de chance (ODS RATIO) com IC de 95% ( usando erro padrao = SPSS)#####
exp(coef(reg.culturageral))
exp(coef(reg.culturagratuito))
exp(coef(reg.culturapago))
exp(coef(reg.musica))
exp(coef(reg.museu))
exp(coef(reg.audiovisual))
exp(coef(reg.games))

exp(cbind(OR = coef(reg.culturageral)), confint(reg.culturageral))

exp(cbind(coef(reg.culturageral), confint.default(reg.culturageral)))


###### obtenção das razões de chance com IC de 95% ( usando erro padrao = SPSS)

exp(cbind(OR = coef((reg.culturapago), IC = confint.default(reg.culturapago))))

######Pseudo R2######
psrsq(reg.culturageral, method=c("Nagelkerke"))
psrsq(reg.culturageral, method=c("Cox-Snell"))

psrsq(reg.culturapago, method=c("Nagelkerke"))
psrsq(reg.culturapago, method=c("Cox-Snell"))

psrsq(reg.culturagratuito, method=c("Nagelkerke"))
psrsq(reg.culturagratuito, method=c("Cox-Snell"))

psrsq(reg.audiovisual, method=c("Nagelkerke"))
psrsq(reg.audiovisual, method=c("Cox-Snell"))

psrsq(reg.museu, method=c("Nagelkerke"))
psrsq(reg.museu, method=c("Cox-Snell"))

psrsq(reg.games, method=c("Nagelkerke"))
psrsq(reg.games, method=c("Cox-Snell"))

psrsq(reg.musica, method=c("Nagelkerke"))
psrsq(reg.musica, method=c("Cox-Snell"))


#######longlikehood teste#### 
install.packages("bclust")
fitdistr(reg.culturapago, densfun = logistic, coef(reg.culturapago))
fit

svyloglin(coef(reg.culturageral),intercept = "FALSE", reg.culturageral)

svyloglin(reg.culturageral, design = reg.culturageral)
svymle(loglike, gradient = NULL, design =  reg.culturageral)
#############teste de robustez retirando sudeste########
baseind19acessointernetROBUSTEZ <- baseind19acessointernet[baseind19acessointernet$variables$COD_REGIAO_2 != "Sudeste",]

#verificando a soma dos pesos
sum(baseind19acessointernet$variables$PESO)
sum(baseind19acessointernetROBUSTEZ$variables$PESO)

#verificando se o numero excluido confere
summary(baseind19$variables$COD_REGIAO_2)
summary(baseind19acessointernet$variables$COD_REGIAO_2)
summary(baseind19acessointernetROBUSTEZ$variables$COD_REGIAO_2)

levels(baseind19acessointernetROBUSTEZ$variables$CentroOeste)

#rodando modelo geral 
reg.culturageralROB<- svyglm(hab_culturais_gerais_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.culturageralROB, vifs = TRUE, AIC = TRUE)
summary(reg.culturageralROB, vifs = TRUE, AIC = TRUE)
stargazer(reg.culturageralROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
VIF(reg.culturageralROB)
vif(reg.culturageralROB)
exp(coef(reg.culturageralROB))

psrsq(reg.culturageralROB, method=c("Nagelkerke"))
psrsq(reg.culturageralROB, method=c("Cox-Snell"))

AIC(reg.culturageralROB)

#rodando modelo pago
reg.culturapagoROB<- svyglm(hab_culturais_pagos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.culturapagoROB, vifs = TRUE, AIC = TRUE)
summary(reg.culturapagoROB)
stargazer(reg.culturapagoROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
vif(reg.culturapagoROB)
exp(coef(reg.culturapagoROB))

psrsq(reg.culturapagoROB, method=c("Nagelkerke"))
psrsq(reg.culturapagoROB, method=c("Cox-Snell"))

AIC(reg.culturapagoROB)

#rodando modelo gratuito

reg.culturagratuitoROB<- svyglm(hab_culturais_gratuitos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.culturagratuitoROB, vifs = TRUE, AIC = TRUE)
summary(reg.culturagratuitoROB)
stargazer(reg.culturagratuitoROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

vif(reg.culturagratuitoROB)
exp(coef(reg.culturagratuitoROB))

psrsq(reg.culturagratuitoROB, method=c("Nagelkerke"))
psrsq(reg.culturagratuitoROB, method=c("Cox-Snell"))

AIC(reg.culturagratuitoROB)

#rodando modelo audiovisual

reg.culturaaudiovisualROB<- svyglm(hab_audiovisual_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.culturagratuito, vifs = TRUE, AIC = TRUE)
summary(reg.culturaaudiovisualROB)
stargazer(reg.culturaaudiovisualROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))
exp(coef(reg.culturaaudiovisualROB))

#rodando modelo musica

reg.culturaamusicaROB<- svyglm(hab_musica_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.musica, vifs = TRUE, AIC = TRUE)
summary(reg.culturaamusicaROB)
stargazer(reg.culturaamusicaROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))


#rodando modelo games

reg.culturagamesROB<- svyglm(hab_games_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+NORTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernetROBUSTEZ, family = quasibinomial(link = logit), maxit = 100 )

summary(reg.games, vifs = TRUE, AIC = TRUE)
summary(reg.culturagamesROB)

stargazer(reg.culturagamesROB, title = "Habitos culturais GERAL", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#######fodasseeeeeeee#######