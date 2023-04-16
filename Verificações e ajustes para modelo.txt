#### O ajuste realizado nas variáveis abaixo se deu por conta da natureza do modelo: logit binominal. Neste caso, as variáveis são binárias e o cálculo de probabilidade é realizado na probabilidade refeerente a outra categoria da variável. ####
#### Por isto, foi criado as variaveis "Outros", por exemplo: cria-se a variavel ClasseA onde existem duas categorias, sendo "Classe A" e "Outros" e nao mais a variável Classe Social com 4 categorias. O objetivo da transformação foi para a não realização do modelo Logit Multinominal #####

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


#### Ao se tratar de um modelo logit binominal, conforme dito anteriormente, é importante destacar que o cálculo da probabilidade é feito de uma categoria em relação a outra categoria (no caso de modelo binário). Por isso, é importante verificar as categorias de referencias pois irão influenciar diretamente no resultado. #### 

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

##### Neste caso, foi necessáro inverter os levels das categorias de referencia, tanto para variaveis independentes quanto para váriaveis dependentes do modelo #####
#Variáveis Independentes/Explicadas
baseind19acessointernet$variables$hab_culturais_gerais_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_gerais_modelo, ref = "Não")
baseind19acessointernet$variables$hab_culturais_pagos_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_pagos_modelo, ref = "Não")
baseind19acessointernet$variables$hab_culturais_gratuitos_modelo <- relevel(baseind19acessointernet$variables$hab_culturais_gratuitos_modelo, ref = "Não")

#Variável Dependente/Explicativa: Classe
baseind19acessointernet$variables$ClasseA <- relevel(baseind19acessointernet$variables$ClasseA, ref = "Outros")
baseind19acessointernet$variables$ClasseB <- relevel(baseind19acessointernet$variables$ClasseB, ref = "Outros")
baseind19acessointernet$variables$ClasseC <- relevel(baseind19acessointernet$variables$ClasseC, ref = "Outros")
baseind19acessointernet$variables$ClasseDE <- relevel(baseind19acessointernet$variables$ClasseDE, ref = "Outros")

#Variável Dependente/Explicativa: Instrucao 
baseind19acessointernet$variables$Instrucao1Mod <- relevel(baseind19acessointernet$variables$Instrucao1Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao2Mod <- relevel(baseind19acessointernet$variables$Instrucao2Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao3Mod <- relevel(baseind19acessointernet$variables$Instrucao3Mod, ref = "Outros")
baseind19acessointernet$variables$Instrucao4Mod <- relevel(baseind19acessointernet$variables$Instrucao4Mod, ref = "Outros")

#Variável Dependente/Explicativa: Faixa etaria
baseind19acessointernet$variables$FaixaCriançamod <- relevel(baseind19acessointernet$variables$FaixaCriançamod, ref = "Outros")
baseind19acessointernet$variables$FaixaJovemmod <- relevel(baseind19acessointernet$variables$FaixaJovemmod, ref = "Outros")
baseind19acessointernet$variables$FaixaJovemAdultomod <- relevel(baseind19acessointernet$variables$FaixaJovemAdultomod, ref = "Outros")
baseind19acessointernet$variables$FaixaAdultamod <- relevel(baseind19acessointernet$variables$FaixaAdultamod, ref = "Outros")
baseind19acessointernet$variables$FaixaAdulta2mod <- relevel(baseind19acessointernet$variables$FaixaAdulta2mod, ref = "Outros")
baseind19acessointernet$variables$FaixaIdosomod <- relevel(baseind19acessointernet$variables$FaixaIdosomod, ref = "Outros")

#Variável Dependente/Explicativa: Regiao
baseind19acessointernet$variables$SUDESTE <- relevel(baseind19acessointernet$variables$SUDESTE, ref = "Outros")
baseind19acessointernet$variables$SUL <- relevel(baseind19acessointernet$variables$SUL, ref = "Outros")
baseind19acessointernet$variables$NORTE <- relevel(baseind19acessointernet$variables$NORTE, ref="Outros")
baseind19acessointernet$variables$NORDESTE <- relevel(baseind19acessointernet$variables$NORDESTE, ref="Outros")
baseind19acessointernet$variables$CentroOeste <- relevel(baseind19acessointernet$variables$CentroOeste, ref = "Outros")

baseind19acessointernet$variables$dispositivos_modelo <- relevel(baseind19acessointernet$variables$dispositivos_modelo, ref = "Outros")

baseind19acessointernet$variables$hab_computacionais_modelo <- relevel(baseind19acessointernet$variables$hab_computacionais_modelo, ref = "Não")

baseind19acessointernet$variables$ocup_informatizada_modelo <- relevel(baseind19acessointernet$variables$ocup_informatizada_modelo, ref = "Não")

baseind19acessointernet$variables$RacaModelo <- relevel(baseind19acessointernet$variables$RacaModelo, ref = "Outros")

