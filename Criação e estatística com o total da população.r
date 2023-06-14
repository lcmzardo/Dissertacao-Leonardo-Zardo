##### Criação de estatísticas com as variáveis criadas para o total da população pesquisada (com e sem internet). #####

svytotal(~ SEXO, baseind19)
svytotal(~ RACA, baseind19)
svytotal(~ GRAU_INSTRUCAO_2, baseind19)
svytotal(~ CLASSE_CB2015, baseind19)
svytotal(~ FAIXA_ETARIA, baseind19)
svytotal(~ AREA, baseind19)


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
svyby(~ hab_comunicacao_social_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_comunicacao_social_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_comunicacao_social_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_comunicacao_social_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_comunicacao_social_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)

#habitos educacionais
svymean(~ hab_educacionais_modelo, baseind19)
svyby(~ hab_educacionais_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ hab_educacionais_modelo,~ RACA, design = baseind19, svymean)
svyby(~ hab_educacionais_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ hab_educacionais_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ hab_educacionais_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)

#habitos ocupações informatizadas 
svymean(~ ocup_informatizada, baseind19)
svyby(~ ocup_informatizada_modelo,~ SEXO, design = baseind19, svymean)
svyby(~ ocup_informatizada_modelo,~ RACA, design = baseind19, svymean)
svyby(~ ocup_informatizada_modelo,~ GRAU_INSTRUCAO_2, design = baseind19, svymean)
svyby(~ ocup_informatizada_modelo,~ CLASSE_CB2015, design = baseind19, svymean)
svyby(~ ocup_informatizada_modelo,~ FAIXA_ETARIA, design = baseind19, svymean)