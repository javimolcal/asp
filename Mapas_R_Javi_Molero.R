#CARREGAR AL R LLIBRERIES NECESS?RIES
install.packages('rgdal')
library(rgdal)
library(foreign)
library(readxl)
library(Hmisc)
library(RColorBrewer)
library(dplyr)

#Llegim el mapa de barris. Cal marcar la ruta on estan tots els fitxers i la capa
#'layer' ?s el shape file. La variable 'BARRI'assigna un n?mero a cada barri.
barris <- readOGR(dsn = "C:/Users/Usuario/Desktop/Mapes QGIS/Barcelona/Unitats administratives BCN/BCN Unitats Adm", layer = "0301040100_Barris_UNITATS_ADM")
barris$BARRI

#Llegim la BD amb les taxes d'esperan?a de vida al n?ixer
taxes <- read_excel("C:/Users/Usuario/Desktop/Mapes BCN 2019/Dades/Esperan?a de vida/Copia de SISalutIndCalc_EsperanzaVida_OLAP_InfoBarri73_sis_18-00-22_000000-2.xls")
#fem un subset amb nom?s les dones (2) i amb els barris (excl. 99999)
taxes_d <- filter(taxes, SEXE == '2' & FACTORN1 != '99999')
summary(taxes_d)

#Les taxes no me les ha llegit com a num?riques, li dic que s?n num?riques
taxes_d$EX00_01 <- as.numeric(taxes_d$EX00_01)

#Unir el df amb les taxes amb el df del mapa segons el codi del barri
#Els 9 primers barris tenen un codi diferent (ex. "01" vs. "1")
#per poder unir-los, convertim aquestes dues variables de character a number
taxes_d$FACTORN1 <- as.numeric(taxes_d$FACTORN1)
barris$BARRI <- as.numeric(barris$BARRI)

taxes_d$FACTORN1 
barris$BARRI

barris <- merge(barris,taxes_d,by.x="BARRI",by.y="FACTORN1",all.x=TRUE,all.y=TRUE)

#Mapa de les taxes dels barris

#pdf("C:/Users/Usuario/Desktop/Mapa_Esperan?a_vida_Barris_D_2019.pdf", width=10, height=10)
jpeg("C:/Users/Usuario/Desktop/Mapa_Esperan?a_vida_Barris_D_2019.jpeg", width=10, height=10)

#par sirve para modificar elementos del gráfico: https://fhernanb.github.io/Graficos-con-R/par.html#par%C3%A1metro-cex
par(mar=c(1,1,1,1))
#Li dic que m'ha de fer quintils de les taxes
Cortes <- cut2(barris$EX00_01, g=5, digits=5)
valores <- as.numeric(Cortes)
mypalette<-brewer.pal(5,"Oranges")
fgs<-mypalette[valores]
plot(barris, col=fgs, border="grey", lwd=0.001, xlab="", ylab="", axes=F)
#title("", sub = "", cex.main = 1.1, font.main= 2, col.main= "black", cex.sub = 0.90, font.sub = 3, col.sub = "black", line=0)
#decimales en la leyenda
legend("bottomright", title="",legend=levels(Cortes), fill=mypalette, y.intersp=0.8, cex=0.8, bty="n", inset=0.05)
#poner nombres de distritos 
text()

#dev.off sirve para crear definitivamente el archivo
dev.off()
