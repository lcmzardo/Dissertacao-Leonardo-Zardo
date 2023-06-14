#*************************** TIC Domicílios 2019 - Individuos *******************************
#A base de dados utilizada é a pesquisa TIC Domicílios de 2019 de dominio da Centro Regional de Estudos para o Desenvolvimento da Sociedade da Informação (Cetic), microdados disponível em: https://cetic.br/pt/pesquisa/domicilios/microdados/

###### Carrega as bases de dados da TIC DOMIC?LIOS 2019 -  Indivíduos######

load("ticdom_2019_individuos_base_de_microdados_v1.1.RData")

###### Carrega (e instala, se necessário) pacotes requeridos #####

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

###### Verificar os resultados dos códigos junto aos resultados divulgado pela Cetic #####
#IMPORTANTE: Por ser uma pesquisa amostral, toda e qualquer análise deve ser feita utilizando o pacote "survey".
###### Ao realizar a soma dos pesos, o resultado deve ser igual o total também no site, neste caso 181.083.512 #####

sum(baseind19$variables$PESO)

###### Para verificacação de uma variavel específica, veja abaixo #####
#Sem o peso amostroal

table(baseind19$variables$SEXO)
table(baseind19$variables$AREA)

#Com o peso amostral 

svytotal(~SEXO, baseind19)
svytotal(~AREA, baseind19)

### É possível realizar as demais verificações com os painéis disponibilizados no site. No entanto, para captar melhor o objeto de estudo, tanto na realização das estatisticas descritivas quanto no modelo, foram utilizadas as variáveis criadas pelo autor. Abaixo o nome da variável criada no projeto e os respectivos IDs das variáveis utilizadas na criação (a descrição de cada variável atraves do ID pode ser visualizados no dicionário de variável disponibilizado pela Cetic): #####

#Hábitos culturais gerais: TC4C_D; TC4C_E; TC2; TC10_D; TC11_D; TC13_A; TC13_B; TC13_C; TC13_D; TC13_E; TC13_F; TC13_G; TC13_H; C12C_C; TC48_G; C9_B; C12_B; C9_C; C12_A; C9_F;

#Hábitos culturais pagos: TC4C_D; TC4C_E;TC2;

#Hábitos culturais gratuitos: TC10_D; TC11_D;TC13_A; TC13_B; TC13_C; TC13_D; TC13_E; TC13_F; TC13_G; TC13_H; C12_C; TC4B_G; C9_B; C12_B; C9_C; C12_A; C9_F;

#Hábitos de criação cultural: TC10_D; TC11_D;	

#Hábitos de pesquisa de informações culturais: TC13_A; TC13_B; TC13_C; TC13_D; TC13_E; TC13_F; TC13_G; TC13_H;

#Jogos: C12_C; TC4B_G;

#Música: C9_B; C12_B; TC2;

#Audiovisual:C9_C; C12_A; TC4C_D; TC4C_E; 	

#Museu: C9_F;

#Hábitos de informação: TC4B_A; C9_D; 

#Hábitos de comunicação e social: C7_B; C7_C; C7_D; C7_F; J2_B; J2_M; J2_N; J2_I;

#Hábitos de educacionais: C10_A; C10_B; C10_D; TC4B_H

#Hábitos de ocupação informatizada: C10_F; C6_B; B4_B

#Habilidades computacionais: I1_A; I1_B; I1_C; I1_D; I1_E; I1_F; I1_G; I1_H; I1_I;

