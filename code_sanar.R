library(readxl)
library(openxlsx)
library(psych)
library(ggplot2)
library(stringr)
library(dplyr)
library(viridis)
library(hrbrthemes)
install.packages('lubridate')
library(lubridate)
library(tidyr)

#### Tuberculose ####

# 1) Abrir as bases

Tuberc <- read_excel("Tuberc.xlsx")
Tuberm <- read_excel("TubermComp.xlsx")
Pop <- read_excel("Pop.xlsx")
Cura <- read_excel("Cura.xlsx")
Aband <- read_excel("Aband.xlsx")


# 2) Juntar as Bases

Tuber <- left_join(Tuberc[-2], Pop, by = 'CodM')
Tuber <- left_join(Tuber, Tuberm[,-2], by = 'CodM')
Tuber <- left_join(Tuber, Cura[,-2],  by = 'CodM')
Tuber <- left_join(Tuber, Aband[,-2],   by = 'CodM')


# Calcular Medidas de Aprsentação

# Taxas de Incidência

Tuber$Incidtx01 <-round((Tuber$Caso01/Tuber$Pop01)*100000,3)
Tuber$Incidtx02 <-round((Tuber$Caso02/Tuber$Pop02)*100000,3) 
Tuber$Incidtx03 <-round((Tuber$Caso03/Tuber$Pop03)*100000,3) 
Tuber$Incidtx04 <-round((Tuber$Caso04/Tuber$Pop04)*100000,3) 
Tuber$Incidtx05 <-round((Tuber$Caso05/Tuber$Pop05)*100000,3) 
Tuber$Incidtx06 <-round((Tuber$Caso06/Tuber$Pop06)*100000,3) 
Tuber$Incidtx07 <-round((Tuber$Caso07/Tuber$Pop07)*100000,3) 
Tuber$Incidtx08 <-round((Tuber$Caso08/Tuber$Pop08)*100000,3) 
Tuber$Incidtx09 <-round((Tuber$Caso09/Tuber$Pop09)*100000,3) 
Tuber$Incidtx10 <-round((Tuber$Caso10/Tuber$Pop10)*100000,3) 
Tuber$Incidtx11 <-round((Tuber$Caso11/Tuber$Pop11)*100000,3)
Tuber$Incidtx12 <-round((Tuber$Caso12/Tuber$Pop12)*100000,3) 
Tuber$Incidtx13 <-round((Tuber$Caso13/Tuber$Pop13)*100000,3) 
Tuber$Incidtx14 <-round((Tuber$Caso14/Tuber$Pop14)*100000,3) 
Tuber$Incidtx15 <-round((Tuber$Caso15/Tuber$Pop15)*100000,3) 
Tuber$Incidtx16 <-round((Tuber$Caso16/Tuber$Pop16)*100000,3) 
Tuber$Incidtx17 <-round((Tuber$Caso17/Tuber$Pop17)*100000,3) 
Tuber$Incidtx18 <-round((Tuber$Caso18/Tuber$Pop18)*100000,3) 
Tuber$Incidtx19 <-round((Tuber$Caso19/Tuber$Pop19)*100000,3) 

### Proporção de Motalidade 

Tuber$Mortprop01 <-round((Tuber$Mort01/Tuber$Caso01),3)
Tuber$Mortprop02 <-round((Tuber$Mort02/Tuber$Caso02),3) 
Tuber$Mortprop03 <-round((Tuber$Mort03/Tuber$Caso03),3) 
Tuber$Mortprop04 <-round((Tuber$Mort04/Tuber$Caso04),3) 
Tuber$Mortprop05 <-round((Tuber$Mort05/Tuber$Caso05),3) 
Tuber$Mortprop06 <-round((Tuber$Mort06/Tuber$Caso06),3) 
Tuber$Mortprop07 <-round((Tuber$Mort07/Tuber$Caso07),3) 
Tuber$Mortprop08 <-round((Tuber$Mort08/Tuber$Caso08),3) 
Tuber$Mortprop09 <-round((Tuber$Mort09/Tuber$Caso09),3) 
Tuber$Mortprop10 <-round((Tuber$Mort10/Tuber$Caso10),3) 
Tuber$Mortprop11 <-round((Tuber$Mort11/Tuber$Caso11),3)
Tuber$Mortprop12 <-round((Tuber$Mort12/Tuber$Caso12),3) 
Tuber$Mortprop13 <-round((Tuber$Mort13/Tuber$Caso13),3) 
Tuber$Mortprop14 <-round((Tuber$Mort14/Tuber$Caso14),3) 
Tuber$Mortprop15 <-round((Tuber$Mort15/Tuber$Caso15),3) 
Tuber$Mortprop16 <-round((Tuber$Mort16/Tuber$Caso16),3) 
Tuber$Mortprop17 <-round((Tuber$Mort17/Tuber$Caso17),3) 
Tuber$Mortprop18 <-round((Tuber$Mort18/Tuber$Caso18),3) 
Tuber$Mortprop19 <-round((Tuber$Mort19/Tuber$Caso19),3)


# Proporção de Cura 

Tuber$Curprop01 <-round((Tuber$Cura01/Tuber$Caso01),3)
Tuber$Curprop02 <-round((Tuber$Cura02/Tuber$Caso02),3) 
Tuber$Curprop03 <-round((Tuber$Cura03/Tuber$Caso03),3) 
Tuber$Curprop04 <-round((Tuber$Cura04/Tuber$Caso04),3) 
Tuber$Curprop05 <-round((Tuber$Cura05/Tuber$Caso05),3) 
Tuber$Curprop06 <-round((Tuber$Cura06/Tuber$Caso06),3) 
Tuber$Curprop07 <-round((Tuber$Cura07/Tuber$Caso07),3) 
Tuber$Curprop08 <-round((Tuber$Cura08/Tuber$Caso08),3) 
Tuber$Curprop09 <-round((Tuber$Cura09/Tuber$Caso09),3) 
Tuber$Curprop10 <-round((Tuber$Cura10/Tuber$Caso10),3) 
Tuber$Curprop11 <-round((Tuber$Cura11/Tuber$Caso11),3)
Tuber$Curprop12 <-round((Tuber$Cura12/Tuber$Caso12),3) 
Tuber$Curprop13 <-round((Tuber$Cura13/Tuber$Caso13),3) 
Tuber$Curprop14 <-round((Tuber$Cura14/Tuber$Caso14),3) 
Tuber$Curprop15 <-round((Tuber$Cura15/Tuber$Caso15),3) 
Tuber$Curprop16 <-round((Tuber$Cura16/Tuber$Caso16),3) 
Tuber$Curprop17 <-round((Tuber$Cura17/Tuber$Caso17),3) 
Tuber$Curprop18 <-round((Tuber$Cura18/Tuber$Caso18),3) 
Tuber$Curprop19 <-round((Tuber$Cura19/Tuber$Caso19),3)



# Proporção de Abandono


Tuber$Abandprop01 <-round((Tuber$Aband01/Tuber$Caso01),3)
Tuber$Abandprop02 <-round((Tuber$Aband02/Tuber$Caso02),3) 
Tuber$Abandprop03 <-round((Tuber$Aband03/Tuber$Caso03),3) 
Tuber$Abandprop04 <-round((Tuber$Aband04/Tuber$Caso04),3) 
Tuber$Abandprop05 <-round((Tuber$Aband05/Tuber$Caso05),3) 
Tuber$Abandprop06 <-round((Tuber$Aband06/Tuber$Caso06),3) 
Tuber$Abandprop07 <-round((Tuber$Aband07/Tuber$Caso07),3) 
Tuber$Abandprop08 <-round((Tuber$Aband08/Tuber$Caso08),3) 
Tuber$Abandprop09 <-round((Tuber$Aband09/Tuber$Caso09),3) 
Tuber$Abandprop10 <-round((Tuber$Aband10/Tuber$Caso10),3) 
Tuber$Abandprop11 <-round((Tuber$Aband11/Tuber$Caso11),3)
Tuber$Abandprop12 <-round((Tuber$Aband12/Tuber$Caso12),3) 
Tuber$Abandprop13 <-round((Tuber$Aband13/Tuber$Caso13),3) 
Tuber$Abandprop14 <-round((Tuber$Aband14/Tuber$Caso14),3) 
Tuber$Abandprop15 <-round((Tuber$Aband15/Tuber$Caso15),3) 
Tuber$Abandprop16 <-round((Tuber$Aband16/Tuber$Caso16),3) 
Tuber$Abandprop17 <-round((Tuber$Aband17/Tuber$Caso17),3) 
Tuber$Abandprop18 <-round((Tuber$Aband18/Tuber$Caso18),3) 
Tuber$Abandprop19 <-round((Tuber$Aband19/Tuber$Caso19),3)



### Base de Séreis Temporais 


## Transformar linhas em colunas

## Incidência 

TuberInc <- select(Tuber, CodM, Municípios, cols =99:117)

TuberInc <- TuberInc %>% 
  pivot_longer(
    cols = 3:21, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Incidtx") # e os seus valores armazenados nessa nova coluna


TuberInc <- TuberInc %>%
 mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                        (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                        (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                        (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016,
                        (ano == 17) ~ 2017, (ano == 18) ~ 2018, (ano == 19) ~ 2019))


## Mortes 

TuberMort <- select(Tuber, CodM, Municípios, cols =118:136)

TuberMort <- TuberMort %>% 
  pivot_longer(
    cols = 3:21, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Mortprop") # e os seus valores armazenados nessa nova coluna


TuberMort <- TuberMort %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016,
                         (ano == 17) ~ 2017, (ano == 18) ~ 2018, (ano == 19) ~ 2019))


## Curas

TuberCura <- select(Tuber, CodM, Municípios, cols =137:155)

TuberCura <- TuberCura %>% 
  pivot_longer(
    cols = 3:21, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Curaprop") # e os seus valores armazenados nessa nova coluna


TuberCura <- TuberCura %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016,
                         (ano == 17) ~ 2017, (ano == 18) ~ 2018, (ano == 19) ~ 2019))

## Abandonos

TuberAband <- select(Tuber, CodM, Municípios, cols =156:174)

TuberAband <- TuberAband %>% 
  pivot_longer(
    cols = 3:21, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Abandprop") # e os seus valores armazenados nessa nova coluna


TuberAband <- TuberAband %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016,
                         (ano == 17) ~ 2017, (ano == 18) ~ 2018, (ano == 19) ~ 2019))



# Jutar Bases para Séries Temporais

Tuberl <- left_join(TuberInc, TuberMort[,-c(2,5)], by = c("CodM", "ano" ))
Tuberl <- left_join(Tuberl, TuberCura[,-c(2,5)], by = c("CodM", "ano" ))
Tuberl <- left_join(Tuberl, TuberAband[,-c(2,5)], by = c("CodM", "ano" ))


# Criação dos grupos de tratamento - Primeira Fase a


Tuberl$Ano2 <- as.numeric(Tuberl$Ano)

Tuberla <- Tuberl %>%
  #filter(Ano2 > 2009)%>%
  mutate(trat = case_when((Municípios == 'Abreu e Lima')|(Municípios == 'Cabo de Santo Agostinho')|
                            (Municípios == 'Camaragibe')|(Municípios == 'Igarassu')|
                            (Municípios == 'Ipojuca')|(Municípios == 'Jaboatão dos Guararapes')|
                            (Municípios == 'Olinda')|(Municípios == 'Paulista')|(Municípios == ' Recife')|
                            (Municípios == 'Vitória de Santo Antão')|(Municípios == 'Carpina')|
                            (Municípios == 'Escada')|(Municípios == 'Caruaru')|(Municípios == 'Petrolina')|
                            (Municípios == 'Goiana') ~ 1, TRUE ~ 0))



# Renomeando 

Tuberla$trat <- recode(Tuberla$trat, '0' = 'Não Tratado', '1' = 'Tratado')

# Nova fase (2011 - 2014) Segunda Fase b


Tuberlb <- Tuberl %>%
  #filter(Ano2 < 2010) %>%
  mutate(trat = case_when((Municípios == 'Abreu e Lima')|(Municípios == 'Cabo de Santo Agostinho')|
                            (Municípios == 'Camaragibe')|(Municípios == 'Igarassu')|(Municípios == 'Chã de Alegria')|
                            (Municípios == 'Ipojuca')|(Municípios == 'Jaboatão dos Guararapes')|(Municípios == 'Itapissuma')|
                            (Municípios == 'Olinda')|(Municípios == 'Paulista')|(Municípios == 'Recife')|
                            (Municípios == 'Vitória de Santo Antão')|(Municípios == 'Carpina')|(Municípios == 'Itamaracá')|
                            (Municípios == 'São Lourenço da Mata')|(Municípios == 'Feira Nova')|(Municípios == 'João Alfredo')|
                            (Municípios == 'Lagoa do Carro')|(Municípios == 'Limoeiro')|(Municípios == 'Machado')|
                            (Municípios == 'Nazaré da Mata')|(Municípios == 'Passira')|(Municípios == 'Paudalho')|
                            (Municípios == 'Vicência')|(Municípios == 'Água Preta')|(Municípios == 'Barreiros')|(Municípios == 'Palmares')|
                            (Municípios == 'Rio Formoso')|(Municípios == 'São José da Coroa Grande')|
                            (Municípios == 'Bonito')|(Municípios == 'Gravatá')|(Municípios == 'Pesqueira')|
                            (Municípios == 'São Joaquim do Monte')|(Municípios == 'Taquaritinga do Norte')|
                            (Municípios == 'São João')|
                            (Municípios == 'Arcoverde')|(Municípios == 'Jatobá')|
                            (Municípios == 'Pedra')|(Municípios == 'Petrolândia')|
                            (Municípios == 'Mirandiba')|(Municípios == 'Salgueiro')|
                            (Municípios == 'Serrita')|(Municípios == 'Araripina')|
                            (Municípios == 'Bodocó')|(Municípios == 'Trindade')|
                            (Municípios == 'Serra talahda')| (Municípios == 'Escada')|
                            (Municípios == 'Caruaru')|(Municípios == 'Petrolina')|
                            (Municípios == 'Goiana') ~ 1, TRUE ~ 0))

# Renomeando 

Tuberlb$trat <- recode(Tuberlb$trat, '0' = 'Não Tratado', '1' = 'Tratado')



## Gráfico de Linha 

### Configuração da Base para Gráfico de linha 

Tuberl$Data <- format(Tuberl$Ano, format = "%Y")

## Incidência 

Tuberla %>%
  group_by(Ano2)%>%
  summarise(taxa = mean(Incidtx))%>%
  ggplot(aes(x=Ano2, y=taxa)) + geom_line(size = 1.0, color = 'gray') +
  theme_ipsum(base_size = 10)+
  scale_color_ft(name = "Medida:")+
  xlab('Ano')+ylab('Taxa')+
  scale_y_continuous(breaks = seq(0.0, 60, 5), limits = c(0.00, 60))+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  labs(title = paste('Gráfico 01 - Taxa de Incidência por Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))

Tuberla$Incidtx <- as.numeric(Tuberla$Incidtx)

Tuberla %>%
  group_by(Ano2, trat)%>%
  summarise(taxa = mean(Incidtx))%>%
  ggplot(aes(x=Ano2, y=taxa, group=trat, color=trat)) + geom_line(size = 1.0) +
  theme_ipsum(base_size = 10)+
  scale_color_manual(labels = c("Não Tratado", "Tratado"),
                     values=c("#AEB6BF", "#5D6D7E")) + 
  xlab('Ano')+ylab('Taxa')+
  scale_color_ft(name = "Grupo:")+
  scale_y_continuous(breaks = seq(0.0, 90, 5), limits = c(0.00, 90))+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  labs(title = paste('Gráfico 02 - Taxa de Incidência de Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")

## Mortalidade

Tuberla$Mortprop <- as.numeric(Tuberl$Mortprop)
  


Tuberla %>%
  filter(Mortprop >= 0.0  & Mortprop < 1)%>%
  group_by(Ano2)%>%
  summarise(Prop = mean(Mortprop))%>%
  ggplot(aes(x=Ano2, y=Prop)) + geom_line(size = 1.0, color = 'gray') +
  theme_ipsum(base_size = 10)+
  xlab('Ano')+ylab('Proporção')+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  scale_y_continuous(breaks = seq(0.0, 0.15, 0.01), limits = c(0.00, 0.15))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))+
  labs(title = paste('Gráfico 03 - Proporção Mortes por Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")
  


Tuberla %>%
  filter(Mortprop >= 0.0  & Mortprop < 1)%>%
  group_by(Ano2, trat)%>%
  summarise(Prop = mean(Mortprop))%>%
  ggplot(aes(x=Ano2, y=Prop, group=trat, color=trat)) + geom_line(size = 1.0) +
  theme_ipsum(base_size = 10)+
  scale_color_manual(labels = c("Não Tratado", "Tratado"),
                     values=c("#AEB6BF", "#5D6D7E")) + 
  xlab('Ano')+ylab('Proporção')+
  scale_color_ft(name = "Grupo:")+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  scale_y_continuous(breaks = seq(0.0, 0.15, 0.01), limits = c(0.00, 0.15))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))+
  labs(title = paste('Gráfico 04 - Proporção Mortes por Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")


## Cura 

Tuberla %>%
  filter(Curaprop >= 0.0  & Curaprop < 1)%>%
  group_by(Ano2)%>%
  summarise(Prop = mean(Curaprop))%>%
  ggplot(aes(x=Ano2, y=Prop)) + geom_line(size = 1.0, color = 'gray') +
  theme_ipsum(base_size = 10)+
  xlab('Ano')+ylab('Proporção')+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  scale_y_continuous(breaks = seq(0.0, 0.80, 0.1), limits = c(0.00, 0.80))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))+
  labs(title = paste('Gráfico 05 - Proporção de Cura de Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")


Tuberla %>%
  filter(Curaprop >= 0.0  & Curaprop < 1)%>%
  group_by(Ano2, trat)%>%
  summarise(Prop = mean(Curaprop))%>%
  ggplot(aes(x=Ano2, y=Prop, group=trat, color=trat)) + geom_line(size = 1.0) +
  theme_ipsum(base_size = 10)+
  scale_color_manual(labels = c("Não Tratado", "Tratado"),
                     values=c("#AEB6BF", "#5D6D7E")) + 
  xlab('Ano')+ylab('Proporção')+
  scale_color_ft(name = "Grupo:")+
  scale_x_continuous(breaks = seq(2001, 20019, 1), limits = c(2001, 2019))+
  scale_y_continuous(breaks = seq(0.0, 0.80, 0.1), limits = c(0.00, 0.80))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))+
  labs(title = paste('Gráfico 06 - Proporção Cura de Tuberculose (2001 - 2019)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")


### Esquistossomose ####


# 1) Abrir as bases necessárias

EsqExam <- read_excel("EsqExam.xlsx")
EsqPosit <- read_excel("EsqPosit.xlsx")
EsqTratados <- read_excel("EsqTratados.xlsx")
EsqAusen <- read_excel("EsqAusen.xlsx")
EsqPos <- read_excel("EsqPos.xlsx")
Pop <- read_excel("Pop.xlsx")

### Juntar as bases necessárias (variável chave código do município)

Esq <- left_join(Pop, EsqExam[,-2], by = "CodM")
Esq <- left_join(Esq, EsqPosit[,-2], by = "CodM")
Esq <- left_join(Esq, EsqTratados[,-2], by = "CodM")
Esq <- left_join(Esq, EsqAusen[,-2], by = "CodM")
Esq <- left_join(Esq, EsqPos[,-2], by = "CodM")



## Calcular as medidas de apresentação 

## Taxa de Exames pela população

Esq$Exam01tx <- round((Esq$Exam01/Esq$Pop01)*1000,3)
Esq$Exam02tx <- round((Esq$Exam02/Esq$Pop02)*1000,3)
Esq$Exam03tx <- round((Esq$Exam03/Esq$Pop03)*1000,3)
Esq$Exam04tx <- round((Esq$Exam02/Esq$Pop04)*1000,3)
Esq$Exam05tx <- round((Esq$Exam05/Esq$Pop05)*1000,3)
Esq$Exam06tx <- round((Esq$Exam06/Esq$Pop06)*1000,3)
Esq$Exam07tx <- round((Esq$Exam07/Esq$Pop07)*1000,3)
Esq$Exam08tx <- round((Esq$Exam08/Esq$Pop08)*1000,3)
Esq$Exam09tx <- round((Esq$Exam09/Esq$Pop09)*1000,3)
Esq$Exam10tx <- round((Esq$Exam10/Esq$Pop10)*1000,3)
Esq$Exam11tx <- round((Esq$Exam11/Esq$Pop11)*1000,3)
Esq$Exam12tx <- round((Esq$Exam12/Esq$Pop12)*1000,3)
Esq$Exam13tx <- round((Esq$Exam13/Esq$Pop13)*1000,3)
Esq$Exam14tx <- round((Esq$Exam14/Esq$Pop14)*1000,3)
Esq$Exam15tx <- round((Esq$Exam15/Esq$Pop15)*1000,3)
Esq$Exam16tx <- round((Esq$Exam16/Esq$Pop16)*1000,3)

## Taxa de tratados pela população total


Esq$Trat01tx <- round((Esq$Trat01/Esq$Pop01)*1000,3)
Esq$Trat02tx <- round((Esq$Trat02/Esq$Pop02)*1000,3)
Esq$Trat03tx <- round((Esq$Trat03/Esq$Pop03)*1000,3)
Esq$Trat04tx <- round((Esq$Trat04/Esq$Pop04)*1000,3)
Esq$Trat05tx <- round((Esq$Trat05/Esq$Pop05)*1000,3)
Esq$Trat06tx <- round((Esq$Trat06/Esq$Pop06)*1000,3)
Esq$Trat07tx <- round((Esq$Trat07/Esq$Pop07)*1000,3)
Esq$Trat08tx <- round((Esq$Trat08/Esq$Pop08)*1000,3)
Esq$Trat09tx <- round((Esq$Trat09/Esq$Pop09)*1000,3)
Esq$Trat10tx <- round((Esq$Trat10/Esq$Pop10)*1000,3)
Esq$Trat11tx <- round((Esq$Trat11/Esq$Pop11)*1000,3)
Esq$Trat12tx <- round((Esq$Trat12/Esq$Pop12)*1000,3)
Esq$Trat13tx <- round((Esq$Trat13/Esq$Pop13)*1000,3)
Esq$Trat14tx <- round((Esq$Trat14/Esq$Pop14)*1000,3)
Esq$Trat15tx <- round((Esq$Trat15/Esq$Pop15)*1000,3)
Esq$Trat16tx <- round((Esq$Trat16/Esq$Pop16)*1000,3)


## Proporção de Ausente por Exames Positivos


Esq$Ausen01 <- as.numeric(Esq$Ausen01)
Esq$Posit01 <- as.numeric(Esq$Posit01)

Esq$Aus01prop <- round((Esq$Ausen01/Esq$Posit01),3)
Esq$Aus02prop <- round((Esq$Ausen02/Esq$Posit02),3)
Esq$Aus03prop <- round((Esq$Ausen03/Esq$Posit03),3)
Esq$Aus04prop <- round((Esq$Ausen04/Esq$Posit04),3)
Esq$Aus05prop <- round((Esq$Ausen05/Esq$Posit05),3)
Esq$Aus06prop <- round((Esq$Ausen05/Esq$Posit06),3)
Esq$Aus07prop <- round((Esq$Ausen07/Esq$Posit07),3)
Esq$Aus08prop <- round((Esq$Ausen08/Esq$Posit08),3)
Esq$Aus09prop <- round((Esq$Ausen09/Esq$Posit09),3)
Esq$Aus10prop <- round((Esq$Ausen10/Esq$Posit10),3)
Esq$Aus11prop <- round((Esq$Ausen11/Esq$Posit11),3)
Esq$Aus12prop <- round((Esq$Ausen12/Esq$Posit12),3)
Esq$Aus13prop <- round((Esq$Ausen13/Esq$Posit13),3)
Esq$Aus14prop <- round((Esq$Ausen14/Esq$Posit14),3)
Esq$Aus15prop <- round((Esq$Ausen15/Esq$Posit15),3)
Esq$Aus16prop <- round((Esq$Ausen16/Esq$Posit16),3)



### Base para Gráficos de Linha - Séries Temporais


## Transormar linhas em colunas 

## Exames

EsqExam <- select(Esq, CodM, Municípios, cols =103:118)

EsqExam <- EsqExam %>% 
  pivot_longer(
    cols = 3:18, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Examtx") # e os seus valores armazenados nessa nova coluna


EsqExam  <- EsqExam %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016))


# Tratados

EsqTrat <- select(Esq, CodM, Municípios, cols =119:134)

EsqTrat <- EsqTrat %>% 
  pivot_longer(
    cols = 3:18, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Trattx") # e os seus valores armazenados nessa nova coluna


EsqTrat  <- EsqTrat %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016))


# Positividade

EsqPosit <- select(Esq, CodM, Municípios, cols =39:54)

EsqPosit <- EsqPosit %>% 
  pivot_longer(
    cols = 3:18, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Positper") # e os seus valores armazenados nessa nova coluna


EsqPosit  <- EsqPosit %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016))

# Ausente

EsqAusen <- select(Esq, CodM, Municípios, cols =135:150)

EsqAusen <- EsqAusen %>% 
  pivot_longer(
    cols = 3:18, # as colunas desse intervalo
    names_to = "ano", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "cols", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "Ausenprop") # e os seus valores armazenados nessa nova coluna


EsqAusen  <- EsqAusen %>%
  mutate(Ano = case_when((ano == 1) ~ 2001, (ano == 2) ~ 2002, (ano == 3) ~ 2003, (ano == 4) ~ 2004,
                         (ano == 5) ~ 2005, (ano == 6) ~ 2006, (ano == 7) ~ 2007, (ano == 8) ~ 2008,
                         (ano == 9) ~ 2009, (ano == 10) ~ 2010, (ano == 11) ~ 2011, (ano == 12) ~ 2012,
                         (ano == 13) ~ 2013, (ano == 14) ~ 2014, (ano == 15) ~ 2015, (ano == 16) ~ 2016))


## Juntar as bases das séries temporais


Esqst <- left_join(EsqExam, EsqTrat[,c(1,4,5)], by = c('CodM', 'Ano'))
Esqst <- left_join(Esqst, EsqPosit[,c(1,4,5)], by = c('CodM', 'Ano'))
Esqst <- left_join(Esqst, EsqAusen[,c(1,4,5)], by = c('CodM', 'Ano'))


# Criação dos grupos de tratamento - Primeira Fase

# Esquistossomose 

Esqst <- Esqst %>%
  mutate(trat = case_when((Municípios == 'Chã de Alegria')|(Municípios == 'Gloria do Goitá')|
                            (Municípios == ' Vitória de Santo Antão')| (Municípios == 'Lagoa do Carro')|
                            (Municípios == 'Nazaré da Mata')|(Municípios == 'Paudalho')|
                            (Municípios == 'Tracunhaém')| (Municípios == 'Vicência')|
                            (Municípios == 'Água Preta')|(Municípios == 'Belém de Maria')|
                            (Municípios == 'Catende')|(Municípios == 'Cortes')|
                            (Municípios == 'Escada')|(Municípios == 'Gameleira')|
                            (Municípios == 'Jaqueira')|(Municípios == 'Primavera')|
                            (Municípios == 'Rio Formoso')|(Municípios == 'São Bento do Sul')|
                            (Municípios == 'Tamandaré')|(Municípios == 'Bom Conselho')|
                            (Municípios == 'Correntes')|(Municípios == 'Aliança')|
                            (Municípios == 'Condado')|(Municípios == 'Itambé')|(Municípios == 'São Vicente Ferrer')|
                            (Municípios == 'Timaúba')~ 1, TRUE ~ 0))%>%
  mutate(trat = case_when((trat == 0)~ "Não Tratatdo",
                          (trat == 1)~ "tratado"))



# Criação dos grupos de tratamento - Segunda Fase

# Esquistossomose 

Esqst <- Esqst %>%
  mutate(trat2 = case_when((Municípios == 'Araçoiaba')|(Municípios == 'Moreno')|
                            (Municípios == 'São Lourenço da Mata')|(Municípios == 'Vitória de Santo Antão')|
                            (Municípios == 'Carpina')|(Municípios == 'Machados')|(Municípios == 'Tracunhaém')|
                            (Municípios == 'Vicência')|(Municípios == 'Água Preta')|(Municípios == 'Escada')|
                            (Municípios == 'Gameleira')|(Municípios == 'Jaqueira')|
                            (Municípios == 'Lagoa dos Gatos')|
                            (Municípios == 'Palmares')|(Municípios == 'Primavera')|
                            (Municípios == 'Quipapá')|(Municípios == 'Ribeirão')|
                            (Municípios == 'Rio Formoso')|(Municípios == 'São Beneditodo Sul')|
                            (Municípios == 'Sirinhaém')|(Municípios == 'Xexéu')|
                            (Municípios == 'Brejão')|(Municípios == 'Aliança')|(Municípios == 'Goiana')|
                            (Municípios == 'Itaquitinga')|(Municípios == 'Timbaúba')~ 1, TRUE ~ 0))%>%
  mutate(trat2 = case_when((trat2 == 0)~ "Não Tratatdo",
                          (trat2 == 1)~ "tratado"))





## Gráfico de Linha 


### Configuração da Base para Gráfico de linha 

Esqst$Data <- format(Esqst$Ano, format = "%Y")


Esqst %>%
  filter(Examtx >= 0)%>%
  group_by(Ano)%>%
  summarise(taxa = mean(Examtx, na.rm =Examtx ))%>%
  ggplot(aes(x=Ano, y=taxa)) + geom_line(size = 1.0, color = 'gray') +
  theme_ipsum(base_size = 10)+
  scale_color_ft(name = "Medida:")+
  xlab('Ano')+ylab('Taxa')+
  scale_y_continuous(breaks = seq(0.0, 80, 10), limits = c(0, 80))+
  scale_x_continuous(breaks = seq(2001, 2016, 1), limits = c(2001, 2016))+
  labs(title = paste('Gráfico 01 - Taxa de Exames 1000 hab (2001 - 2016)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1))


### Tratamento Primeira Fase

Esqst %>%
  filter(Examtx >= 0)%>%
  group_by(Ano, trat)%>%
  summarise(taxa = mean(Examtx))%>%
  ggplot(aes(x=Ano, y=taxa, group=trat, color=trat)) + geom_line(size = 1.0) +
  theme_ipsum(base_size = 10)+
  scale_color_manual(labels = c("Não Tratado", "Tratado"),
                     values=c("#AEB6BF", "#5D6D7E")) + 
  xlab('Ano')+ylab('Taxa')+
  scale_color_ft(name = "Grupo:")+
  scale_y_continuous(breaks = seq(0.0, 150, 10), limits = c(0, 150))+
  scale_x_continuous(breaks = seq(2001, 2016, 1), limits = c(2001, 2016))+
  labs(title = paste('Gráfico 02 - Taxa de Exames 1000 hab por Grupos (2001 - 2016)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")


### Tratamento Segunda Fase

Esqst %>%
  filter(Examtx >= 0)%>%
  group_by(Ano, trat2)%>%
  summarise(taxa = mean(Examtx))%>%
  ggplot(aes(x=Ano, y=taxa, group=trat2, color=trat2)) + geom_line(size = 1.0) +
  theme_ipsum(base_size = 10)+
  scale_color_manual(labels = c("Não Tratado", "Tratado"),
                     values=c("#AEB6BF", "#5D6D7E")) + 
  xlab('Ano')+ylab('Taxa')+
  scale_color_ft(name = "Grupo:")+
  scale_y_continuous(breaks = seq(0.0, 150, 10), limits = c(0, 150))+
  scale_x_continuous(breaks = seq(2001, 2016, 1), limits = c(2001, 2016))+
  labs(title = paste('Gráfico 02 - Taxa de Exames 1000 hab por Grupos (2001 - 2016)'))+
  theme(axis.text.x = element_text(size=10, angle = 90,hjust = 1), legend.position="bottom")
