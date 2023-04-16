#Rodando o modelo logit.

#Hábito cultural geral
reg.culturageral<- svyglm(hab_culturais_gerais_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.culturageral, title = "Habitos culturais geral", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito cultural pago
reg.culturapago<- svyglm(hab_culturais_pagos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.culturapago, title = "Habitos culturais pago", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito culturalgratuito
reg.culturagratuito<- svyglm(hab_culturais_gratuitos_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.culturagratuito, title = "Habitos culturais gratuito", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito cultural musica
reg.musica <- svyglm(hab_musica_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.musica, title = "Habitos de consumo musical", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito cultural audiovisual
reg.audiovisual <- svyglm(hab_audiovisual_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.audiovisual, title = "Habitos de consumo audiovisual", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito cultural museu
reg.museu <- svyglm(hab_museu_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.museu, title = "Habitos de consumo museu", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))

#Hábito cultural games
reg.games <- svyglm(hab_games_modelo ~ SexoMod+RacaModelo+FaixaCriançamod+FaixaJovemmod+FaixaJovemAdultomod+FaixaAdultamod+FaixaAdulta2mod+Instrucao4Mod+ClasseA+ClasseB+ClasseC+SUDESTE+NORDESTE+SUL+CentroOeste+AREA+ocup_informatizada_modelo+dispositivos_modelo+hab_computacionais_modelo, design = baseind19acessointernet, family = quasibinomial(link = logit), maxit = 100 )

#Visualizando o objeto do modelo
stargazer(reg.games, title = "Habitos de consumo games", align = TRUE, type = "text",
          style = "all", keep.stat = c("aic", "bic", "adj.rsq", "n"))