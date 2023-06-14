#Conforme exposto, na estatística descritiva e no modelo utilizado, foram utilizadas váriaveis criadas pelo autor com a finalidade de captar os comportamentos objetos do estudo. Antes da criação das variáveis, foi excluido os NAS das variáveis originais (primárias que foram utilizadas na criação das variaveis utilizadas). Abaixo: #####

###Para isso, foi criacado um dataframe para realização das alterações, onde: ###

df = baseind19acessointernet$variables

#-------------------------------
# TC4C_D PAGO 
#-------------------------------

summary(df$TC4C_D)

df[df$TC4C_D == 'Não se aplica', 'TC4C_D' ] = 'Não'

df[df$TC4C_D == 'Não sabe' | df$TC4C_D == 'Não respondeu' , 'TC4C_D' ] = as.character(NA)

summary(df$TC4C_D)


#-------------------------------
# TC4C_E PAGO 
#-------------------------------

summary(df$TC4C_E)

df[df$TC4C_E == 'Não se aplica'  , 'TC4C_E' ] = 'Não'

df[df$TC4C_E == 'Não sabe' | df$TC4C_E == 'Não respondeu' , 'TC4C_E' ] = as.character(NA)

summary(df$TC4C_E)

#-------------------------------
# TC2 PAGO 
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# TC10_D GRATUITO 
#-------------------------------

summary(df$TC10_D)

df[df$TC10_D == 'Não se aplica'  , 'TC10_D' ] = 'Não'

df[df$TC10_D == 'Não sabe' | df$TC10_D == 'Não respondeu' , 'TC10_D' ] = as.character(NA)

summary(df$TC10_D)

#-------------------------------
# TC11_D GRATUITO 
#-------------------------------

summary(df$TC11_D)

df[df$TC11_D == 'Não se aplica'  , 'TC11_D' ] = 'Não'

df[df$TC11_D == 'Não sabe' | df$TC11_D == 'Não respondeu' , 'TC11_D' ] = as.character(NA)

summary(df$TC11_D)

#-------------------------------
# TC13_A GRATUITO 
#-------------------------------

summary(df$TC13_A)

df[df$TC13_A == 'Não se aplica'  , 'TC13_A' ] = 'Não'

df[df$TC13_A == 'Não sabe' | df$TC13_A == 'Não respondeu' , 'TC13_A' ] = as.character(NA)

summary(df$TC13_A)

#-------------------------------
# TC13_B GRATUITO 
#-------------------------------

summary(df$TC13_B)

df[df$TC13_B == 'Não se aplica'  , 'TC13_B' ] = 'Não'

df[df$TC13_B == 'Não sabe' | df$TC13_B == 'Não respondeu' , 'TC13_B' ] = as.character(NA)

summary(df$TC13_B)

#-------------------------------
# TC13_C GRATUITO 
#-------------------------------

summary(df$TC13_C)

df[df$TC13_C == 'Não se aplica'  , 'TC13_C' ] = 'Não'

df[df$TC13_C == 'Não sabe' | df$TC13_C == 'Não respondeu' , 'TC13_C' ] = as.character(NA)

summary(df$TC13_C)

#-------------------------------
# TC13_D GRATUITO 
#-------------------------------

summary(df$TC13_D)

df[df$TC13_D == 'Não se aplica'  , 'TC13_D' ] = 'Não'

df[df$TC13_D == 'Não sabe' | df$TC13_D == 'Não respondeu' , 'TC13_D' ] = as.character(NA)

summary(df$TC13_D)

#-------------------------------
# TC13_E GRATUITO 
#-------------------------------

summary(df$TC13_E)

df[df$TC13_E == 'Não se aplica'  , 'TC13_E' ] = 'Não'

df[df$TC13_E == 'Não sabe' | df$TC13_E == 'Não respondeu' , 'TC13_E' ] = as.character(NA)

summary(df$TC13_E)

#-------------------------------
# TC13_F GRATUITO 
#-------------------------------

summary(df$TC13_F)

df[df$TC13_F == 'Não se aplica'  , 'TC13_F' ] = 'Não'

df[df$TC13_F == 'Não sabe' | df$TC13_F == 'Não respondeu' , 'TC13_F' ] = as.character(NA)

summary(df$TC13_F)

#-------------------------------
# TC13_G GRATUITO 
#-------------------------------

summary(df$TC13_G)

df[df$TC13_G == 'Não se aplica'  , 'TC13_G' ] = 'Não'

df[df$TC13_G == 'Não sabe' | df$TC13_G == 'Não respondeu' , 'TC13_G' ] = as.character(NA)

summary(df$TC13_G)

#-------------------------------
# TC13_H GRATUITO 
#-------------------------------

summary(df$TC13_H)

df[df$TC13_H == 'Não se aplica'  , 'TC13_H' ] = 'Não'

df[df$TC13_H == 'Não sabe' | df$TC13_H == 'Não respondeu' , 'TC13_H' ] = as.character(NA)

summary(df$TC13_H)


#-------------------------------
# C12C_ GRATUITO 
#-------------------------------

summary(df$C12_C)

df[df$C12_C == 'Não se aplica'  , 'C12_C' ] = 'Não'

df[df$C12_C == 'Não sabe' | df$C12_C == 'Não respondeu' , 'C12_C' ] = as.character(NA)

summary(df$C12_C)

#-------------------------------
# TC4B_G GRATUITO 
#-------------------------------

summary(df$TC4B_G)

df[df$TC4B_G == 'Não se aplica'  , 'TC4B_G' ] = 'Não'

df[df$TC4B_G == 'Não sabe' | df$TC4B_G == 'Não respondeu' , 'TC4B_G' ] = as.character(NA)

summary(df$TC4B_G)

#-------------------------------
# C9_B GRATUITO 
#-------------------------------

summary(df$C9_B)

df[df$C9_B == 'Não se aplica'  , 'C9_B' ] = 'Não'

df[df$C9_B == 'Não sabe' | df$C9_B == 'Não respondeu' , 'C9_B' ] = as.character(NA)

summary(df$C9_B)

#-------------------------------
# TC2 GRATUITO 
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# C9_C GRATUITO 
#-------------------------------

summary(df$C9_C)

df[df$C9_C == 'Não se aplica'  , 'C9_C' ] = 'Não'

df[df$C9_C == 'Não sabe' | df$C9_C == 'Não respondeu' , 'C9_C' ] = as.character(NA)

summary(df$C9_C)

#-------------------------------
# C12_A GRATUITO 
#-------------------------------

summary(df$C12_A)

df[df$C12_A == 'Não se aplica'  , 'C12_A' ] = 'Não'

df[df$C12_A == 'Não sabe' | df$C12_A == 'Não respondeu' , 'C12_A' ] = as.character(NA)

summary(df$C12_A)

#-------------------------------
# C9_F GRATUITO 
#-------------------------------

summary(df$C9_F)

df[df$C9_F == 'Não se aplica'  , 'C9_F' ] = 'Não'

df[df$C9_F == 'Não sabe' | df$C9_F == 'Não respondeu' , 'C9_F' ] = as.character(NA)

summary(df$C9_F)

#-------------------------------
# C10_F OCUP INFO
#-------------------------------

summary(df$C10_F)

df[df$C10_F == 'Não se aplica'  , 'C10_F' ] = 'Não'

df[df$C10_F == 'Não sabe' | df$C10_F == 'Não respondeu' , 'C10_F' ] = as.character(NA)

summary(df$C10_F)

#-------------------------------
# C6_B OCUP INFO
#-------------------------------

summary(df$C6_B)

df[df$C6_B == 'Não se aplica'  , 'C6_B' ] = 'Não'

df[df$C6_B == 'Não sabe' | df$C6_B == 'Não respondeu' , 'C6_B' ] = as.character(NA)

summary(df$C6_B)

#-------------------------------
# C9_F MUSEU 
#-------------------------------

summary(df$C9_F)

df[df$C9_F == 'Não se aplica'  , 'C9_F' ] = 'Não'

df[df$C9_F == 'Não sabe' | df$C9_F == 'Não respondeu' , 'C9_F' ] = as.character(NA)

summary(df$C9_F)


#-------------------------------
# C9_C AUDIOVISUAL
#-------------------------------

summary(df$C9_C)

df[df$C9_C == 'Não se aplica'  , 'C9_C' ] = 'Não'

df[df$C9_C == 'Não sabe' | df$C9_C == 'Não respondeu' , 'C9_C' ] = as.character(NA)

summary(df$C9_C)


#-------------------------------
# C12_A AUDIOVISUAL
#-------------------------------

summary(df$C12_A)

df[df$C12_A == 'Não se aplica'  , 'C12_A' ] = 'Não'

df[df$C12_A == 'Não sabe' | df$C12_A == 'Não respondeu' , 'C12_A' ] = as.character(NA)

summary(df$C12_A)

#-------------------------------
# TC4C_D AUDIOVISUAL
#-------------------------------

summary(df$TC4C_D)

df[df$TC4C_D == 'Não se aplica'  , 'TC4C_D' ] = 'Não'

df[df$TC4C_D == 'Não sabe' | df$TC4C_D == 'Não respondeu' , 'TC4C_D' ] = as.character(NA)

summary(df$TC4C_D)

#-------------------------------
# TC4C_E AUDIOVISUAL
#-------------------------------

summary(df$TC4C_E)

df[df$TC4C_E == 'Não se aplica'  , 'TC4C_E' ] = 'Não'

df[df$TC4C_E == 'Não sabe' | df$TC4C_E == 'Não respondeu' , 'TC4C_E' ] = as.character(NA)

summary(df$TC4C_E)

#-------------------------------
# C9_B MUSICA
#-------------------------------

summary(df$C9_B)

df[df$C9_B == 'Não se aplica'  , 'C9_B' ] = 'Não'

df[df$C9_B == 'Não sabe' | df$C9_B == 'Não respondeu' , 'C9_B' ] = as.character(NA)

summary(df$C9_B)

#-------------------------------
# TC2 MUSICA
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# TC2 MUSICA
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# C12_C JOGOS
#-------------------------------

summary(df$C12_C)

df[df$C12_C == 'Não se aplica'  , 'C12_C' ] = 'Não'

df[df$C12_C == 'Não sabe' | df$C12_C == 'Não respondeu' , 'C12_C' ] = as.character(NA)

summary(df$C12_C)

#-------------------------------
# TC4B_G JOGOS
#-------------------------------

summary(df$TC4B_G)

df[df$TC4B_G == 'Não se aplica'  , 'TC4B_G' ] = 'Não'

df[df$TC4B_G == 'Não sabe' | df$TC4B_G == 'Não respondeu' , 'TC4B_G' ] = as.character(NA)

summary(df$TC4B_G)

#-------------------------------
# TC13_A PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_A)

df[df$TC13_A == 'Não se aplica'  , 'TC13_A' ] = 'Não'

df[df$TC13_A == 'Não sabe' | df$TC13_A == 'Não respondeu' , 'TC13_A' ] = as.character(NA)

summary(df$TC13_A)

#-------------------------------
# TC13_B PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_B)

df[df$TC13_B == 'Não se aplica'  , 'TC13_B' ] = 'Não'

df[df$TC13_B == 'Não sabe' | df$TC13_B == 'Não respondeu' , 'TC13_B' ] = as.character(NA)

summary(df$TC13_B)

#-------------------------------
# TC13_C PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_C)

df[df$TC13_C == 'Não se aplica'  , 'TC13_C' ] = 'Não'

df[df$TC13_C == 'Não sabe' | df$TC13_C == 'Não respondeu' , 'TC13_C' ] = as.character(NA)

summary(df$TC13_C)

#-------------------------------
# TC13_D PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_D)

df[df$TC13_D == 'Não se aplica'  , 'TC13_D' ] = 'Não'

df[df$TC13_D == 'Não sabe' | df$TC13_D == 'Não respondeu' , 'TC13_D' ] = as.character(NA)

summary(df$TC13_D)


#-------------------------------
# TC13_E PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_E)

df[df$TC13_E == 'Não se aplica'  , 'TC13_E' ] = 'Não'

df[df$TC13_E == 'Não sabe' | df$TC13_E == 'Não respondeu' , 'TC13_E' ] = as.character(NA)

summary(df$TC13_E)

#-------------------------------
# TC13_F PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_F)

df[df$TC13_F == 'Não se aplica'  , 'TC13_F' ] = 'Não'

df[df$TC13_F == 'Não sabe' | df$TC13_F == 'Não respondeu' , 'TC13_F' ] = as.character(NA)

summary(df$TC13_F)

#-------------------------------
# TC13_G PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_G)

df[df$TC13_G == 'Não se aplica'  , 'TC13_G' ] = 'Não'

df[df$TC13_G == 'Não sabe' | df$TC13_G == 'Não respondeu' , 'TC13_G' ] = as.character(NA)

summary(df$TC13_G)

#-------------------------------
# TC13_H PESQUISA DE INF. CULT.
#-------------------------------

summary(df$TC13_H)

df[df$TC13_H == 'Não se aplica'  , 'TC13_H' ] = 'Não'

df[df$TC13_H == 'Não sabe' | df$TC13_H == 'Não respondeu' , 'TC13_H' ] = as.character(NA)

summary(df$TC13_H)

#-------------------------------
# TC10_D CRIAÇÃO CULTURAL
#-------------------------------

summary(df$TC10_D)

df[df$TC10_D == 'Não se aplica'  , 'TC10_D' ] = 'Não'

df[df$TC10_D == 'Não sabe' | df$TC10_D == 'Não respondeu' , 'TC10_D' ] = as.character(NA)

summary(df$TC10_D)

#-------------------------------
# TC11_D CRIAÇÃO CULTURAL
#-------------------------------

summary(df$TC11_D)

df[df$TC11_D == 'Não se aplica'  , 'TC11_D' ] = 'Não'

df[df$TC11_D == 'Não sabe' | df$TC11_D == 'Não respondeu' , 'TC11_D' ] = as.character(NA)

summary(df$TC11_D)

#-------------------------------
# TC11_D CRIAÇÃO CULTURAL
#-------------------------------

summary(df$TC11_D)

df[df$TC11_D == 'Não se aplica'  , 'TC11_D' ] = 'Não'

df[df$TC11_D == 'Não sabe' | df$TC11_D == 'Não respondeu' , 'TC11_D' ] = as.character(NA)

summary(df$TC11_D)

#-------------------------------
# TC4B_A DE INFORMACAO
#-------------------------------

summary(df$TC4B_A)

df[df$TC4B_A == 'Não se aplica'  , 'TC4B_A' ] = 'Não'

df[df$TC4B_A == 'Não sabe' | df$TC4B_A == 'Não respondeu' , 'TC4B_A' ] = as.character(NA)

summary(df$TC4B_A)

#-------------------------------
# C9_D DE INFORMACAO
#-------------------------------

summary(df$C9_D)

df[df$C9_D == 'Não se aplica'  , 'C9_D' ] = 'Não'

df[df$C9_D == 'Não sabe' | df$C9_D == 'Não respondeu' , 'C9_D' ] = as.character(NA)

summary(df$C9_D)

#-------------------------------
# C7_B COM E SOCIAL
#-------------------------------

summary(df$C7_B)

df[df$C7_B == 'Não se aplica'  , 'C7_B' ] = 'Não'

df[df$C7_B == 'Não sabe' | df$C7_B == 'Não respondeu' , 'C7_B' ] = as.character(NA)

summary(df$C7_B)

#-------------------------------
#  C7_C COM E SOCIAL
#-------------------------------

summary(df$C7_C)

df[df$C7_C == 'Não se aplica'  , 'C7_C' ] = 'Não'

df[df$C7_C == 'Não sabe' | df$C7_C == 'Não respondeu' , 'C7_C' ] = as.character(NA)

summary(df$C7_C)


#-------------------------------
# C7_D COM E SOCIAL
#-------------------------------

summary(df$C7_D)

df[df$C7_D == 'Não se aplica'  , 'C7_D' ] = 'Não'

df[df$C7_D == 'Não sabe' | df$C7_D == 'Não respondeu' , 'C7_D' ] = as.character(NA)

summary(df$C7_D)


#-------------------------------
# C7_F COM E SOCIAL
#-------------------------------

summary(df$C7_F)

df[df$C7_F == 'Não se aplica'  , 'C7_F' ] = 'Não'

df[df$C7_F == 'Não sabe' | df$C7_F == 'Não respondeu' , 'C7_F' ] = as.character(NA)

summary(df$C7_F)


#-------------------------------
# J2_B COM E SOCIAL
#-------------------------------

summary(df$J2_B)

df[df$J2_B == 'Não se aplica'  , 'J2_B' ] = 'Não'

df[df$J2_B == 'Não sabe' | df$J2_B == 'Não respondeu' , 'J2_B' ] = as.character(NA)

summary(df$J2_B)

#-------------------------------
# J2_M COM E SOCIAL
#-------------------------------

summary(df$J2_M)

df[df$J2_M == 'Não se aplica'  , 'J2_M' ] = 'Não'

df[df$J2_M == 'Não sabe' | df$J2_M == 'Não respondeu' , 'J2_M' ] = as.character(NA)

summary(df$J2_M)

#-------------------------------
# J2_N COM E SOCIAL
#-------------------------------

summary(df$J2_N)

df[df$J2_N == 'Não se aplica'  , 'J2_N' ] = 'Não'

df[df$J2_N == 'Não sabe' | df$J2_N == 'Não respondeu' , 'J2_N' ] = as.character(NA)

summary(df$J2_N)

#-------------------------------
# J2_I COM E SOCIAL
#-------------------------------

summary(df$J2_I)

df[df$J2_I == 'Não se aplica'  , 'J2_I' ] = 'Não'

df[df$J2_I == 'Não sabe' | df$J2_I == 'Não respondeu' , 'J2_I' ] = as.character(NA)

summary(df$J2_I)

#-------------------------------
# C10_A HAB EDUCACIONAIS
#-------------------------------

summary(df$C10_A)

df[df$C10_A == 'Não se aplica'  , 'C10_A' ] = 'Não'

df[df$C10_A == 'Não sabe' | df$C10_A == 'Não respondeu' , 'C10_A' ] = as.character(NA)

summary(df$C10_A)

#-------------------------------
# C10_B HAB EDUCACIONAIS
#-------------------------------

summary(df$C10_B)

df[df$C10_B == 'Não se aplica'  , 'C10_B' ] = 'Não'

df[df$C10_B == 'Não sabe' | df$C10_B == 'Não respondeu' , 'C10_B' ] = as.character(NA)

summary(df$C10_B)

#-------------------------------
# C10_D HAB EDUCACIONAIS
#-------------------------------

summary(df$C10_D)

df[df$C10_D == 'Não se aplica'  , 'C10_D' ] = 'Não'

df[df$C10_D == 'Não sabe' | df$C10_D == 'Não respondeu' , 'C10_D' ] = as.character(NA)

summary(df$C10_D)


#-------------------------------
# TC4B_H HAB EDUCACIONAIS
#-------------------------------

summary(df$TC4B_H)

df[df$TC4B_H == 'Não se aplica'  , 'TC4B_H' ] = 'Não'

df[df$TC4B_H == 'Não sabe' | df$TC4B_H == 'Não respondeu' , 'TC4B_H' ] = as.character(NA)

summary(df$TC4B_H)

#-------------------------------
# TC4B_H HAB EDUCACIONAIS
#-------------------------------

summary(df$TC4B_H)

df[df$TC4B_H == 'Não se aplica'  , 'TC4B_H' ] = 'Não'

df[df$TC4B_H == 'Não sabe' | df$TC4B_H == 'Não respondeu' , 'TC4B_H' ] = as.character(NA)

summary(df$TC4B_H)

#-------------------------------
# C5_DISPOSITIVOS
#-------------------------------

summary(df$C5_DISPOSITIVOS)

df[df$C5_DISPOSITIVOS == '99'  , 'C5_DISPOSITIVOS' ] = 'Não'

df[df$C5_DISPOSITIVOS == 'Não sabe' | df$C5_DISPOSITIVOS == 'Não respondeu' , 'C5_DISPOSITIVOS' ] = as.character(NA)

summary(df$C5_DISPOSITIVOS)


#-------------------------------
# I1_A HABILIDADES COMPUTADOR
#-------------------------------

summary(df$I1_A)

df[df$I1_A == 'Não se aplica'  , 'I1_A' ] = 'Não'

df[df$I1_A == 'Não sabe' | df$I1_A == 'Não respondeu' , 'I1_A' ] = as.character(NA)

summary(df$I1_A)

#-------------------------------
# I1_B HABILIDADES COMPUTADOR
#-------------------------------

summary(df$I1_B)

df[df$I1_B == 'Não se aplica'  , 'I1_B' ] = 'Não'

df[df$I1_B == 'Não sabe' | df$I1_B == 'Não respondeu' , 'I1_B' ] = as.character(NA)

summary(df$I1_B)

#-------------------------------
# I1_CHABILIDADES COMPUTADOR
#-------------------------------

summary(df$I1_C)

df[df$I1_C == 'Não se aplica'  , 'I1_C' ] = 'Não'

df[df$I1_C == 'Não sabe' | df$I1_C == 'Não respondeu' , 'I1_C' ] = as.character(NA)

summary(df$I1_C)

#-------------------------------
# B3 COMPUTACIONAIS
#-------------------------------

summary(df$I1_D)

df[df$I1_D == 'Não se aplica'  , 'I1_D' ] = 'Não'

df[df$I1_D == 'Não sabe' | df$I1_D == 'Não respondeu' , 'I1_D' ] = as.character(NA)

summary(df$I1_D)

#-------------------------------
# I1_E COMPUTACIONAIS
#-------------------------------

summary(df$I1_E)

df[df$I1_E == 'Não se aplica'  , 'I1_E' ] = 'Não'

df[df$I1_E == 'Não sabe' | df$I1_E == 'Não respondeu' , 'I1_E' ] = as.character(NA)

summary(df$I1_E)

#-------------------------------
# I1_F COMPUTACIONAIS
#-------------------------------

summary(df$I1_G)

df[df$I1_F == 'Não se aplica'  , 'I1_F' ] = 'Não'

df[df$I1_F == 'Não sabe' | df$I1_F == 'Não respondeu' , 'I1_F' ] = as.character(NA)

summary(df$I1_F)

#-------------------------------
# I1_G COMPUTACIONAIS
#-------------------------------

summary(df$I1_G)

df[df$I1_G == 'Não se aplica'  , 'I1_G' ] = 'Não'

df[df$I1_G == 'Não sabe' | df$I1_G == 'Não respondeu' , 'I1_G' ] = as.character(NA)

summary(df$I1_G)

#-------------------------------
# I1_H COMPUTACIONAIS
#-------------------------------

summary(df$I1_H)

df[df$I1_H == 'Não se aplica'  , 'I1_H' ] = 'Não'

df[df$I1_H == 'Não sabe' | df$I1_H == 'Não respondeu' , 'I1_H' ] = as.character(NA)

summary(df$I1_H)

#-------------------------------
# B3 COMPUTACIONAIS
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# MUSICA
#-------------------------------

summary(df$C9_B)

df[df$C9_B == 'Não se aplica'  , 'C9_B' ] = 'Não'

df[df$C9_B == 'Não sabe' | df$C9_B == 'Não respondeu' , 'C9_B' ] = as.character(NA)

summary(df$C9_B)

#-------------------------------
# MUSICA
#-------------------------------

summary(df$C12_B)

df[df$C12_B == 'Não se aplica'  , 'C12_B' ] = 'Não'

df[df$C12_B == 'Não sabe' | df$C12_B == 'Não respondeu' , 'C12_B' ] = as.character(NA)

summary(df$C12_B)

#-------------------------------
# MUSICA
#-------------------------------

summary(df$TC2)

df[df$TC2 == 'Não se aplica'  , 'TC2' ] = 'Não'

df[df$TC2 == 'Não sabe' | df$TC2 == 'Não respondeu' , 'TC2' ] = as.character(NA)

summary(df$TC2)

#-------------------------------
# AUDIOVISUAL
#-------------------------------

summary(df$C9_C)

df[df$C9_C == 'Não se aplica'  , 'C9_C' ] = 'Não'

df[df$C9_C == 'Não sabe' | df$C9_C == 'Não respondeu' , 'C9_C' ] = as.character(NA)

summary(df$C9_C)

#-------------------------------
# AUDIOVISUAL
#-------------------------------

summary(df$C12_A)

df[df$C12_A == 'Não se aplica'  , 'C12_A' ] = 'Não'

df[df$C12_A == 'Não sabe' | df$C12_A == 'Não respondeu' , 'C12_A' ] = as.character(NA)

summary(df$C12_A)
#-------------------------------
# AUDIOVISUAL
#-------------------------------

summary(df$TC4C_D)

df[df$TC4C_D == 'Não se aplica'  , 'TC4C_D' ] = 'Não'

df[df$TC4C_D == 'Não sabe' | df$TC4C_D == 'Não respondeu' , 'TC4C_D' ] = as.character(NA)

summary(df$TC4C_D)
#-------------------------------
# AUDIOVISUAL
#-------------------------------

summary(df$TC4C_E)

df[df$TC4C_E == 'Não se aplica'  , 'TC4C_E' ] = 'Não'

df[df$TC4C_E == 'Não sabe' | df$TC4C_E == 'Não respondeu' , 'TC4C_E' ] = as.character(NA)

summary(df$TC4C_E)

#-------------------------------
# MUSEU
#-------------------------------

summary(df$C9_F)

df[df$C9_F == 'Não se aplica'  , 'C9_F' ] = 'Não'

df[df$C9_F == 'Não sabe' | df$C9_F == 'Não respondeu' , 'C9_F' ] = as.character(NA)

summary(df$C9_F)

##### Ao final das alterções, deve subsertituitr no objeto "survey" as variaveis pelas avariaveis agora alteradas no dataframe #####

baseind19acessointernet$variables = df  