#### Mais importante que executar o modelo, são comandos que visualizam seus resultados e verificam sua confiabilidade. Abaixo os overhall dos modelos.

anova(reg.culturageral, test.statistic="Wald")
anova(reg.culturapago, test.statistic="Wald")
anova(reg.culturagratuito, test.statistic="Wald")
anova(reg.audiovisual, test.statistic="Wald")
anova(reg.musica, test.statistic="Wald")
anova(reg.games, test.statistic="Wald")
anova(reg.museu, test.statistic="Wald")

#### Conforme bibliográfia consultada, realizar o teste de multicolineariedade. Teste utilizado através do metodo de VIF #####
#### fator de inflação da variância (VIF), que avalia o quanto a variância de um coeficiente de regressão estimado aumenta se as suas preditoras estiverem correlacionadas. ####

vif(reg.culturageral)

vif(reg.culturagratuito)

vif(reg.culturapago)

vif(reg.museu)

vif(reg.musica)

vif(reg.audiovisual)

#### Teste de pseudo-R2 (R2 utilizado para casos de regressão em modelo Logit #####

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

##### Confiança do modelo #####
##### Verossimilhanca ######

confint(reg.culturageral)

confint(reg.culturagratuito)

confint(reg.culturapago)

confint(reg.museu)

confint(reg.musica)

confint(reg.audiovisual)

confint(reg.culturapago)

##### Plotagem dos pontos de alavancagem dos modelos/outliers #####

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