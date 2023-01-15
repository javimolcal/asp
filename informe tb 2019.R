#install.packages("writexl")
#install.packages("expss")
library(foreign)
library(haven)
library(dplyr)
library(readxl)
library(tidyverse)
library(summarytools)
library(pastecs)
library(expss)
library(lubridate)
library(writexl)
library(expss)

#cargar archivo '.sav' (archivo de SPSS) en R
setwd("//aspblfs/usuaris$/ext_jmolero/Escritorio//Informe TB/Tuberculosi")
setwd("C:/Users/Usuario/Desktop/Informes TB")
getwd()
bbdd_tb <- read.spss("tb07032022.sav", to.data.frame = TRUE)
warnings()
str(bbdd_tb)

#Nombre de casos declarats cada any (2019 i 2020)
summarytools::freq(bbdd_tb$anydecla)

#agafar només els que inici de tractament sigui 2019 i que siguin casos
casos_t2019 <- filter(bbdd_tb, anytra == 2019 & CONCLUFIN != 'NO TBC')
summarytools::freq(casos_t2019$CONCLUFIN)

#Nombre de casos residents a BCN i no residents a BCN tractats el 2019
summarytools::freq(casos_t2019$BARCEL)

#Crear la taula 1 en un data frame
explota.llibre.2019 <- data.frame(summarytools::freq(casos_t2019$MUNICIPI))
#afegir el nom dels municipis a una columna
explota.llibre.2019 <- rownames_to_column(explota.llibre.2019, var = "Població")

#exportar la taula 1 a excel
#write_xlsx(explota.llibre.2019,"C:/Users/Usuario/Desktop/Informe TB//explota.llibre.2019.xlsx")
#write_xlsx(explota.llibre.2019,"//aspblfs/usuaris$/ext_jmolero/Escritorio//explota.llibre.2019.xlsx")

#Crear la taula 2 en un data frame i exportar-la a excel
taula2 <- data.frame(cross_cases(casos_t2019, CENTREING, BARCEL))
#write_xlsx(taula2,"C:/Users/Usuario/Desktop/Informe TB//taula2_2019.xlsx")
#write_xlsx(taula2,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula2_2019.xlsx")

#agafar només els que inici de tractament sigui 2019 i que siguin casos residents a BCN
casos_t2019_bcn <- filter(bbdd_tb, anytra == 2019 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA')
summarytools::freq(casos_t2019_bcn$BARCEL)

#Casos per grup d'edat, sexe i districte i origen (taules 3 i 4)
#aumentar las filas máximas representadas en las tablas cruzadas
options(max.print=1000000)
cross_cases(casos_t2019_bcn, EDATEMI, SEXE)
cross_cases(casos_t2019_bcn, EDATEMI, SEXE, PAISR)
cross_cases(casos_t2019_bcn, EDATEMI, SEXE, DM)
cross_cases(casos_t2019_bcn, EDATEMI, DM, PAISR)

#como solo hace tablas con 3 variables máximo, hacemos subsets según la variable SEXE
casos_t2019_bcn_hom <- filter(casos_t2019_bcn, SEXE == 'MASCULINO')
casos_t2019_bcn_don <- filter(casos_t2019_bcn, SEXE == 'FEMENINO')
cross_cases(casos_t2019_bcn_hom, EDATEMI, PAISR, DM)
cross_cases(casos_t2019_bcn_don, EDATEMI, PAISR, DM)

#No determinats taula 3, comprovar que coincideixen els valors amb les els de la taula 3
cross_cases(casos_t2019_bcn, DM, SEXE)

#No determinats taula 4, comprovar que coincideixen els valors amb les els de la taula 4
cross_cases(casos_t2019_bcn, DM, PAISR)

#taula 6, distribució per barri
summarytools::freq(casos_t2019_bcn$BARRI1)
taula6 <- data.frame(summarytools::freq(casos_t2019_bcn$BARRI1))
taula6 <- rownames_to_column(taula6, var = "Barri")
#write_xlsx(taula6,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula6_2019.xlsx")

#taula 6.5, casos segons barri i sexe
cross_cases(casos_t2019_bcn, BARRI1, SEXE)
taula6.5_sexe <- data.frame(cross_cases(casos_t2019_bcn, BARRI1, SEXE))
#write_xlsx(taula6.5_sexe, path = "//aspblfs/usuaris$/ext_jmolero/Escritorio//taula6.5_sexe.xlsx")

#carregar la taula 6.5 d'excel que hem creat anteriorment, afegint la RDHpc per cada barri (web), població per sexe i barri (web) i casos per sexe i barri (taula6.5_sexe)
#la RDHpc s'obté de la següent web
#https://ajuntament.barcelona.cat/estadistica/castella/Estadistiques_per_temes/Economia/Renda_i_tributs/Renda_disponible_llars/T022.htm
setwd("//aspblfs/usuaris$/ext_jmolero/Escritorio//Informe TB/2019")
#setwd("C:/Users/Usuario/Desktop/Informe TB")
getwd()
renda <- read_xlsx(path = "taula6.5_2019_renda_sexe.xlsx",
                       range = 'A1:H66')

#categoritzar la variable RDHpc
renda$RDHpc.2018 <- as.numeric(renda$RDHpc.2018)
nivell_renda <- cut(renda$RDHpc.2018, breaks = c(-Inf, 0.69, 0.89, 1.09, Inf),
                    labels = c("Baixa", "Mitjana-baixa", "Mitjana-alta", "Alta-Molt alta"),
                    include.lowest = TRUE)

renda <- data.frame(nivell_renda, renda)

summary(nivell_renda)

#write_xlsx(renda, path = "//aspblfs/usuaris$/ext_jmolero/Escritorio//taula6.5_2019_final.xlsx")

#Taula 8, grup d'edat i sexe
taula8 <- data.frame(cross_cases(casos_t2019_bcn, EDATFIS, SEXE))
#write_xlsx(taula8,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula8_2019.xlsx")

#Taula 9, grup d'edat i origen
taula9 <- data.frame(cross_cases(casos_t2019_bcn, EDATEMI, SEXE))
#write_xlsx(taula9,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula9_2019.xlsx")

#Taula 11, país de naixement
taula11 <- data.frame(summarytools::freq(casos_t2019_bcn$PAIS))
taula11 <- rownames_to_column(taula11)
#write_xlsx(taula11,"C:/Users/Usuario/Desktop//taula11_2019.xlsx")

#Taula 12, ocupació 2013-2019
casos_t2013_2019_bcn <- filter(bbdd_tb, anytra >= 2013 & anytra <= 2019 &
                                 CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA')
summarytools::freq(casos_t2013_2019_bcn$anytra)
summarytools::freq(casos_t2013_2019_bcn$BARCEL)
summarytools::freq(casos_t2013_2019_bcn$CONCLUFIN)

taula12 <- data.frame(cross_cases(casos_t2013_2019_bcn, GRUPOCUPA, SEXE))
#write_xlsx(taula12,"C:/Users/Usuario/Desktop//taula12_2019.xlsx")

#Taula 13, tipus de feina de les persones en actiu, 2013-2019
taula13 <- data.frame(cross_cases(casos_t2013_2019_bcn, COD1DIG, SEXE))
#write_xlsx(taula13,"C:/Users/Usuario/Desktop//taula13_2019.xlsx")

#Taula 14, formes clíniques de TB
summarytools::freq(casos_t2019_bcn$PLEURAL)
summarytools::freq(casos_t2019_bcn$PLEUROPULM)
summarytools::freq(casos_t2019_bcn$PULMONAR)
summarytools::freq(casos_t2019_bcn$LARINGEA)
summarytools::freq(casos_t2019_bcn$LIMFATICA)
summarytools::freq(casos_t2019_bcn$MILIAR)
summarytools::freq(casos_t2019_bcn$OSTEOART)
summarytools::freq(casos_t2019_bcn$RENAL)
summarytools::freq(casos_t2019_bcn$MENINGEA)
summarytools::freq(casos_t2019_bcn$ALTRESLOC)
a <- data.frame(summarytools::freq(casos_t2019_bcn$ALTRESLOQ))
a <- rownames_to_column(a)
summarytools::freq(casos_t2019_bcn$PULM)
#write_xlsx(a,"//aspblfs/usuaris$/ext_jmolero/Escritorio//a.xlsx")
cross_cases(casos_t2020_bcn, ALTRESLOQ, PULMONAR)

#Taula 15, evolució de les formes clíniques
taula15 <- data.frame(summarytools::freq(casos_t2019_bcn$PULM))
taula15 <- rownames_to_column(taula15)
#write_xlsx(taula15,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula15_2019.xlsx")

#Taula 16, factors de risc segons sexe
a <- data.frame(cross_cases(casos_t2019_bcn, ADVP, SEXE))
b <- data.frame(cross_cases(casos_t2019_bcn, alcoholis2cat, SEXE))
c <- data.frame(cross_cases(casos_t2019_bcn, diabetes2cat, SEXE))
d <- data.frame(cross_cases(casos_t2019_bcn, embaras2cat, SEXE))
e <- data.frame(cross_cases(casos_t2019_bcn, HIV1, SEXE))
f <- data.frame(cross_cases(casos_t2019_bcn, indigent2cat, SEXE))
g <- data.frame(cross_cases(casos_t2019_bcn, medicinmu2cat, SEXE))
h <- data.frame(cross_cases(casos_t2019_bcn, CASNOU, SEXE))
i <- data.frame(cross_cases(casos_t2019_bcn, cprison2cat, SEXE))
j <- data.frame(cross_cases(casos_t2019_bcn, RENAL, SEXE))
k <- data.frame(cross_cases(casos_t2019_bcn, SILICOSI, SEXE))
l <- data.frame(cross_cases(casos_t2019_bcn, tabac2cat, SEXE))
m <- data.frame(cross_cases(casos_t2019_bcn, GASTRECT, SEXE))

taula16 <- rbind(a, b, c, d, e, f, g, h, i, j, k, l, m)

summarytools::freq(casos_t2019_bcn$SEXE)

#write_xlsx(taula16,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula16_2019.xlsx")

#taula 18, resistències
summarytools::freq(casos_t2019_bcn$ABFET)
summarytools::freq(casos_t2019_bcn$cultiu)
summarytools::freq(casos_t2019_bcn$CASNOU)
casos_t2019_bcn_bk_ab <- filter(bbdd_tb, anytra == 2019 &
                                 CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA' &
                                 cultiu == 'positiu' & ABFET == 'antibiograma fet')
summarytools::freq(casos_t2019_bcn_bk_ab$RESTREPTO)
summarytools::freq(casos_t2019_bcn_bk_ab$RETAM)
summarytools::freq(casos_t2019_bcn_bk_ab$RINH)
summarytools::freq(casos_t2019_bcn_bk_ab$RRIFAM)
summarytools::freq(casos_t2019_bcn_bk_ab$RPRZ)
summarytools::freq(casos_t2019_bcn_bk_ab$MDR)

casos_t2019_bcn_bk_ab_cas9 <- filter(bbdd_tb, anytra == 2019 &
                                  CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA' &
                                  cultiu == 'positiu' & ABFET == 'antibiograma fet' &
                                  CASNOU == 1)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$RESTREPTO)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$RETAM)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$RINH)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$RRIFAM)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$RPRZ)
summarytools::freq(casos_t2019_bcn_bk_ab_cas9$MDR)

#taula19, resistències segons PAISR
cross_cases(casos_t2019_bcn_bk_ab, RINH, PAISR)
cross_cases(casos_t2019_bcn_bk_ab, MDR, PAISR)

#taula 20, retard dx
summarytools::freq(casos_t2019_bcn$retardiag)

#taula 23, conclusió final dels malalts
taula23 <- data.frame(summarytools::freq(casos_t2019_bcn$CONCLUFIN))
taula23 <- rownames_to_column(taula23)
#write_xlsx(taula23,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula23_2019.xlsx")

#taula 24, conclusió malalts 2018 segons tipus TB
casos_t2018_bcn <- filter(bbdd_tb, anytra == 2018 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA')
summarytools::freq(casos_t2018_bcn$CONCLUFIN)
summarytools::freq(casos_t2018_bcn$pulmoextra)
summarytools::freq(casos_t2018_bcn$pulmobkposi)
casos_t2018_bcn_pulm <- filter(bbdd_tb, anytra == 2018 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA' & pulmoextra == 'tb pulmonar 2 tb extrapulmonar')
summarytools::freq(casos_t2018_bcn_pulm$CONCLUFIN)
casos_t2018_bcn_bk <- filter(bbdd_tb, anytra == 2018 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA' & pulmobkposi == 'pulmonar bacilifer')
summarytools::freq(casos_t2018_bcn_bk$CONCLUFIN)

taula24 <- data.frame(summarytools::freq(casos_t2018_bcn$CONCLUFIN),
                      summarytools::freq(casos_t2018_bcn_pulm$CONCLUFIN),
                      summarytools::freq(casos_t2018_bcn_bk$CONCLUFIN))
taula24 <- rownames_to_column(taula24)

#write_xlsx(taula24,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula24_2019.xlsx")

#taula 25, resultat tractament per diferents variables
casos_t2017_bcn <- filter(bbdd_tb, anytra == 2017 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA')

a <- data.frame(cross_cases(casos_t2018_bcn, SEXE, CONCLUFIN))
b <- data.frame(cross_cases(casos_t2018_bcn, EDATEMI, CONCLUFIN))
c <- data.frame(cross_cases(casos_t2018_bcn, PAISR, CONCLUFIN))
d <- data.frame(cross_cases(casos_t2018_bcn, alcoholis2cat, CONCLUFIN))
e <- data.frame(cross_cases(casos_t2018_bcn, ADVP, CONCLUFIN))
f <- data.frame(cross_cases(casos_t2018_bcn, HIV1, CONCLUFIN))
g <- data.frame(cross_cases(casos_t2018_bcn, indigent2cat, CONCLUFIN))
h <- data.frame(cross_cases(casos_t2018_bcn, cprison2cat, CONCLUFIN))
i <- data.frame(cross_cases(casos_t2018_bcn, MonINH, CONCLUFIN))
#els MDR fan 18 mesos de tto, per això hem d'agafar els de la cohort del 2017
j <- data.frame(cross_cases(casos_t2017_bcn, MDR, CONCLUFIN))

taula25 <- rbind(a, b, c, d, e, f, g, h, i, j)

#write_xlsx(taula25,"//aspblfs/usuaris$/ext_jmolero/Escritorio//taula25_2019.xlsx")

#taules 20, 21 i 21bis retard dx
bbdd_retard_dx <- read.spss("BD_retard_TBC.sav", to.data.frame = TRUE)

# merge two data frames by num_reg
bbdd_fusio_ret <- merge(bbdd_tb, bbdd_retard_dx, by.x="NUM_REG",by.y="num_reg",all.x=TRUE,all.y=TRUE)
casos_t2019_bcn_ret <- filter(bbdd_fusio_ret, anytra == 2019 & CONCLUFIN != 'NO TBC' & BARCEL == 'RESIDENTS BARCELONA')
#write_xlsx(casos_t2019_bcn_ret,"//aspblfs/usuaris$/ext_jmolero/Escritorio//ret2019.xlsx")
