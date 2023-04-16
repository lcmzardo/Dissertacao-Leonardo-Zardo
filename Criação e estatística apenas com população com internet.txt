#### A partir da pesquisa total, o projeto foi recomendado pela banca avaliadora que fosse utilizado somente a população com internet. ####
#### Cria-se, portanto: #### 

baseind19acessointernet <- baseind19[baseind19$variables$C3J3 == "Sim",]


#### Abaixo as estatisticas, DESTAQUE que são as mesmas relaizadas para a população com internet, alterando apenas o objeto acima criado ####

svytotal(~ SEXO, baseind19acessointernet)
svytotal(~ RACA, baseind19acessointernet)
svytotal(~ GRAU_INSTRUCAO_2, baseind19acessointernet)
svytotal(~ CLASSE_CB2015, baseind19acessointernet)
svytotal(~ FAIXA_ETARIA, baseind19acessointernet)
svytotal(~ AREA, baseind19acessointernet)

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