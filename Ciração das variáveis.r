#Após adaptação das variáveis originárias, cria-se as variáveis que serão utilizadas no projeto:


#Hab habito cultural geral
baseind19 <- update(baseind19,
                                  hab_culturais_gerais_modelo = case_when(TC4C_D=="Sim"|TC4C_E=="Sim"|TC2=="Sim"|TC10_D=="Sim"|TC11_D=="Sim"|TC13_A=="Sim"| TC13_B=="Sim"|TC13_C=="Sim"|TC13_D=="Sim"| TC13_E=="Sim"| TC13_F=="Sim"| TC13_G=="Sim"|TC13_H=="Sim"|C12_C=="Sim"|TC4B_G=="Sim"|C9_B=="Sim"|C12_B=="Sim"|C9_C=="Sim"|C12_A=="Sim"|C9_F=="Sim"~ "Sim",
                                                                          TC4C_D=="Não"|TC4C_E=="Não"|TC2=="Não"|TC10_D=="Não"|TC11_D=="Não"|TC13_A=="Não"| TC13_B=="Não"|TC13_C=="Não"|TC13_D=="Não"| TC13_E=="Não"| TC13_F=="Não"| TC13_G=="Não"|TC13_H=="Não"|C12_C=="Não"|TC4B_G=="Não"|C9_B=="Não"|C12_B=="Não"|C9_C=="Não"|C12_A=="Não"|C9_F=="Não"~ "Não",
                                                                          TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica"|TC2=="Não se aplica"|TC10_D=="Não se aplica"|TC11_D=="Não se aplica"|TC13_A=="Não se aplica"| TC13_B=="Não se aplica"|TC13_C=="Não se aplica"|TC13_D=="Não se aplica"| TC13_E=="Não se aplica"| TC13_F=="Não se aplica"| TC13_G=="Não se aplica"|TC13_H=="Não se aplica"|C12_C=="Não se aplica"|TC4B_G=="Não"|C9_B=="Não se aplica"|C12_B=="Não se aplica"|C9_C=="Não se aplica"|C12_A=="Não se aplica"|C9_F=="Não se aplica"~ "Não",
                                                                          TRUE ~ as.character(NA)))

baseind19$variables$hab_culturais_gerais_modelo<- factor(baseind19$variables$hab_culturais_gerais_modelo,
                                                                       levels = c("Sim", "Não"))

#Hab habito cultural pagos
baseind19 <- update(baseind19,
                                  hab_culturais_pagos_modelo = case_when(TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                         TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                         TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                         TRUE ~ as.character(NA)))
baseind19$variables$hab_culturais_pagos_modelo<- factor(baseind19$variables$hab_culturais_pagos_modelo,
                                                                      levels = c("Sim", "Não"))

#Hab cultural gratuitos 
baseind19 <- update(baseind19,
                                  hab_culturais_gratuitos_modelo = case_when(C9_B=="Sim"|C9_C=="Sim"|C9_F=="Sim"|C12_A=="Sim"|C12_B=="Sim"|C12_E1=="Sim"|C12_F1=="Sim"|J2_C=="Sim"|TC3_A=="Sim"|TC3_B=="Sim"|TC4_A=="Sim"|TC4_B=="Sim"|TC4B_C=="Sim"|TC4B_D=="Sim"|TC4B_F=="Sim"|TC2=="Sim"|TC4C_D=="Sim"|TC4C_E=="Sim" ~ "Sim",
                                                                             C9_B=="Não"|C9_C=="Não"|C9_F=="Não"|C12_A=="Não"|C12_B=="Não"|C12_E1=="Não"|C12_F1=="Não"|J2_C=="Não"|TC3_A=="Não"|TC3_B=="Não"|TC4_A=="Não"|TC4_B=="Não"|TC4B_C=="Não"|TC4B_D=="Não"|TC4B_F=="Não"|TC2=="Não"|TC4C_D=="Não"|TC4C_E=="Não" ~ "Não",
                                                                             C9_B=="Não se aplica"|C9_C=="Não se aplica"|C9_F=="Não se aplica"|C12_A=="Não se aplica"|C12_B=="Não se aplica"|C12_E1=="Não se aplica"|C12_F1=="Não se aplica"|J2_C=="Não se aplica"|TC3_A=="Não se aplica"|TC3_B=="Não se aplica"|TC4_A=="Não se aplica"|TC4_B=="Não se aplica"|TC4B_C=="Não se aplica"|TC4B_D=="Não se aplica"|TC4B_F=="Não se aplica"|TC2=="Não se aplica"|TC4C_D=="Não se aplica"|TC4C_E=="Não se aplica" ~ "Não",
                                                                             TRUE ~ as.character(NA)))
baseind19$variables$hab_culturais_gratuitos_modelo<- factor(baseind19$variables$hab_culturais_gratuitos_modelo,
                                                                          levels = c("Sim", "Não"))

#Hab ocupacao informatizada 
baseind19 <- update(baseind19,
                                  ocup_informatizada_modelo = case_when(C10_F=="Sim"|C6_B=="Sim" ~ "Sim",
                                                                        C10_F=="Não"|C6_B=="Não" ~ "Não",
                                                                        C10_F=="Não se aplica"|C6_B=="Não se aplica" ~ "Não",
                                                                        TRUE ~ as.character(NA)))
baseind19$variables$ocup_informatizada_modelo<- factor(baseind19$variables$ocup_informatizada_modelo,
                                                                     levels = c("Sim", "Não"))

#Hab computacionais

baseind19 <- update(baseind19,
                                  hab_computacionais_modelo = case_when(I1_A=="Sim"|I1_B=="Sim"|I1_C=="Sim"|I1_D=="Sim"|I1_E=="Sim"|I1_F=="Sim"|I1_G=="Sim"|I1_H=="Sim"|I1_I =="Sim" ~ "Sim",
                                                                        I1_A=="Não"|I1_B=="Não"|I1_C=="Não"|I1_D=="Não"|I1_E=="Não"|I1_F=="Não"|I1_G=="Não"|I1_H=="Não"|I1_I =="Não" ~ "Não",
                                                                        I1_A=="Não se aplica"|I1_B=="Não se aplica"|I1_C=="Não se aplica"|I1_D=="Não se aplica"|I1_E=="Não se aplica"|I1_F=="Não se aplica"|I1_G=="Não se aplica"|I1_H=="Não se aplica"|I1_I =="Não se aplica" ~ "Não se aplica",
                                                                        TRUE ~ as.character(NA)))
baseind19$variables$hab_computacionais_modelo<- factor(baseind19$variables$hab_computacionais_modelo,
                                                                     levels = c("Sim", "Não"))

#Dispositivos

baseind19 <- update(baseind19,
                                  dispositivos_modelo = case_when(C5_DISPOSITIVOS=="Apenas telefone celular" ~ "Apenas telefone celular",
                                                                  C5_DISPOSITIVOS=="Apenas computador" ~ "Outros",
                                                                  C5_DISPOSITIVOS=="Ambos" ~ "Outros",
                                                                  TRUE ~ as.character(NA)))
baseind19$variables$dispositivos_modelo  <- factor(baseind19$variables$dispositivos_modelo ,
                                                                 levels = c("Apenas telefone celular", "Outros"))

#Musica

baseind19 <- update(baseind19,
                                  hab_musica_modelo = case_when(C9_B=="Sim"| C12_B == "Sim" | TC2 == "Sim" ~ "Sim",
                                                                C9_B=="Não"| C12_B == "Não" | TC2 == "Não" ~ "Não",
                                                                C9_B=="Não se aplica"| C12_B == "Não se aplica" | TC2 == "Não se aplica" ~ "Não",
                                                                TRUE ~ as.character(NA)))
baseind19$variables$hab_musica_modelo  <- factor(baseind19$variables$hab_musica_modelo,
                                                               levels = c("Não", "Sim"))

#Audiovisual

baseind19 <- update(baseind19,
                                  hab_audiovisual_modelo = case_when(C9_C=="Sim"|C12_A=="Sim"|TC4C_D =="Sim"|TC4C_E =="Sim" ~ "Sim",
                                                                     C9_C=="Não"|C12_A=="Não"|TC4C_D =="Não"|TC4C_E =="Sim" ~ "Não",
                                                                     C9_C=="Não se aplica"|C12_A=="Não se aplica"|TC4C_D =="Não se aplica"|TC4C_E =="Não se aplica" ~ "Não",
                                                                     TRUE ~ as.character(NA)))
baseind19$variables$hab_audiovisual_modelo  <- factor(baseind19$variables$hab_audiovisual_modelo ,
                                                                    levels = c("Não", "Sim"))

#Museu

baseind19 <- update(baseind19,
                                  hab_museu_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não "|C12_C =="Não se aplica" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19$variables$hab_museu_modelo  <- factor(baseind19$variables$hab_museu_modelo ,
                                                              levels = c("Não", "Sim"))

#Jogos

baseind19 <- update(baseind19,
                                  hab_games_modelo = case_when(TC4B_G =="Sim"|C12_C =="Sim" ~ "Sim",
                                                               TC4B_G =="Não se aplica"|C12_C =="Não se aplica" ~ "Não",
                                                               TC4B_G =="Não"|C12_C =="Não" ~ "Não",
                                                               TRUE ~ as.character(NA)))
baseind19$variables$hab_games_modelo  <- factor(baseind19$variables$hab_games_modelo ,
                                                              levels = c("Não", "Sim"))

#Comunicacao e midias sociais
baseind19 <- update(baseind19,
                    hab_comunicacao_social_modelo = case_when(C7_B =="Sim"|C7_C =="Sim"|C7_D=="Sim"|C7_F=="Sim"|J2_B=="Sim"|J2_M=="Sim"|J2_N=="Sim"|J2_I=="Sim" ~ "Sim",
                                                              C7_B =="Não se aplica"|C7_C =="Não se aplica"|C7_D=="Não se aplica"|C7_F=="Não se aplica"|J2_B=="Não se aplica"|J2_M=="Não se aplica"|J2_N=="Não se aplica"|J2_I=="Não se aplica" ~ "Não" ,
                                                              C7_B =="Não"|C7_C =="Não"|C7_D=="Não"|C7_F=="Não"|J2_B=="Não"|J2_M=="Não"|J2_N=="Não"|J2_I=="Não" ~ "Não",
                                                              TRUE ~ as.character(NA)))
baseind19$variables$hab_comunicacao_social_modelo <- factor(baseind19$variables$hab_comunicacao_social_modelo ,
                                                levels = c("Não", "Sim"))

#Hab educacionais 
baseind19 <- update(baseind19,
                    hab_educacionais_modelo = case_when(C10_A =="Sim"|C10_B =="Sim"|C10_D=="Sim"|TC4B_H=="Sim" ~ "Sim",
                                                        C10_A =="Não se aplica"|C10_B =="Não se aplica"|C10_D=="Não se aplica"|TC4B_H=="Não se aplica" ~ "Não" ,
                                                        C10_A =="Não"|C10_B =="Não"|C10_D=="Não"|TC4B_H=="Não" ~ "Não",
                                                        TRUE ~ as.character(NA)))
baseind19$variables$hab_educacionais_modelo <- factor(baseind19$variables$hab_educacionais_modelo ,
                                                            levels = c("Não", "Sim"))

#Noticias e informação
baseind19 <- update(baseind19,
                    hab_informação_modelo = case_when(TC4B_A =="Sim"|C9_D =="Sim" ~ "Sim",
                                                      TC4B_A =="Não se aplica"|C9_D =="Não se aplica"~ "Não" ,
                                                      TC4B_A =="Não"|C9_D =="Não"|C10_D=="Não" ~ "Não",
                                                      TRUE ~ as.character(NA)))
baseind19$variables$hab_informação_modelo <- factor(baseind19$variables$hab_informação_modelo ,
                                                      levels = c("Não", "Sim"))

#Hab Criacao cultural 
baseind19 <- update(baseind19,
                    hab_criacao_cultural_modelo= case_when(TC10_D== "Sim" |TC11_D == "Sim" ~ "Sim",
                                                   	   TC10_D== "Não se aplica" |TC11_D == "Não se aplica" ~ "Não",
							   TC10_D== "Não" |TC11_D == "Não" ~ "Não",
							   TRUE ~ as.character(NA)))
baseind19$variables$hab_criacao_cultural_modelo <- factor(baseind19$variables$hab_criacao_cultural_modelo ,
                                                      levels = c("Não", "Sim"))