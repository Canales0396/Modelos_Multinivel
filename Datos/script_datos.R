library(haven)
library(foreign)
library(dplyr)

#####################################################################################
#               Datos para el 2021
#####################################################################################
ECV2021 <- read.spss("~/Documents/Github/Modelos_Multinivel/Datos/Base de la ECV 2021.sav")
ECV2021 <- data.frame(ECV2021)
ECV2021$P04.1 = as.character(ECV2021$P04)

## Zona visitada
#ECV2021$zona <- "Resto del Mundo"

ECV2021$zona[ECV2021$P04.1 %in% c("Estados Unidos de América",
                                  "México","Canadá")]  <- "Norteamérica"

ECV2021$zona[ECV2021$P04.1 %in% c("El Salvador", "Guatemala", "Nicaragua", "Costa Rica",
                              "Panamá", "Belice")]  <- "Centroamérica"

ECV2021$zona[ECV2021$P04.1 %in% c("Colombia", "Brasil", "Ecuador", "Argentina", "Perú", "Uruguay",
                              "Bolivia", "Paraguay", "Chile")] <- "Suramérica"
  
ECV2021$zona[ECV2021$P04.1 %in% c("Islas Caimán", "República Dominicana", 
                                  "Puerto Rico","Cuba")]  <- "Caribe"

ECV2021$zona[ECV2021$P04.1 %in% c("España", "Alemania", "Francia", "Italia", "Suiza", "Reino Unido" , 
                              "Países Bajos", "Polonia", "Portugal", "República Checa", "Grecia" ,
                              "Lituania", "Eslovenia", "Austria", "Dinamarca" , "Irlanda", "Noruega",
                              "Ucrania", "Bélgica","Israel", "Turquía", "Rusia (Federación de)","Nueva Zelanda")]  <- "Europa"
  
#ECV2021$zona[ECV2021$P04.1 %in% c("Israel", "Turquía", "Rusia (Federación de)")] 
####<- "Resto del Mundo"


## Filtrado de las variables que se necesitan para la estimación 
ECV2021N <- subset(ECV2021, select = c(Validas, Mes, Trimestre, Procedencia, CodCiuRes,
                                       P04_RegionVA, P04_RegionVF, P10A, P10D, P11_Zona1,
                                       GruGasto, PGastoTotal, TipVisitante, Hotel, Amigos, 
                                       CasaP, Ninguno))

ECV2021N$Hotel[is.na(ECV2021N$Hotel)] <- 0
ECV2021N$Amigos[is.na(ECV2021N$Amigos)] <- 0
ECV2021N$CasaP[is.na(ECV2021N$CasaP)] <- 0
ECV2021N$Ninguno[is.na(ECV2021N$Ninguno)] <- 0

## Filtrado solo para los datos de Gasto y Perfil
ECV2021N <- ECV2021N[!is.na(ECV2021N$PGastoTotal), ]
ECV2021N <- subset(ECV2021N, PGastoTotal > 0)

## Filtrado solo para todas las zona menos la desconocida
ECV2021NF <- subset(ECV2021N, P11_Zona1 %in% c("Zona Centro","Zona Insular",
                                               "Zona Norte","Zona Occidental",
                                               "Zona Oriental","Zona Sur"))


ECV2021NF<- subset(ECV2021NF, !is.na(Procedencia))
## Conversión del Gasto Fin a escala Logarítmica
GastoTotal= ECV2021NF$PGastoTotal
LogGTN = na.exclude(log(GastoTotal))

glevels1 = factor(ECV2021NF$P11_Zona1)
glevels2 = factor(ECV2021NF$Procedencia)
table(glevels1)
table(glevels2)

gl1 = as.numeric(glevels1[!is.na(log(GastoTotal))])
gl2 = as.numeric(glevels2[!is.na(log(GastoTotal))])

## Niveles combinados
glevels3 = factor(paste(ECV2021NF$P11_Zona1,ECV2021NF$Procedencia))
table(glevels3)
gl3 = as.numeric(glevels3[!is.na(log(GastoTotal))])
gl3

# setwd("Modelos_Multinivel/Datos")
save.image("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021.RData")
rm(list = ls())

#####################################################################################
#               Datos para el 2016
#####################################################################################

EGYPV2016Turistas <- read.spss("~/Documents/Modelos_Multinivel/Datos/EGYPV 2016 F01 - Turistas.sav")
EGYPV2016Turistas <-data.frame(EGYPV2016Turistas)

## Filtrado de las variables que se necesitan para la estimación 
EGYPV2016TN <- subset(EGYPV2016Turistas, select = c(Validas, TipViajero, TipVisitante, Mes, CodRes, RegRes, CodPrimVisita,P29_Mu4,
                                                    GruViaje, P10_3NumNoch, Pernocto, GruNoch, P11_THoteles, P11_TAmigos,
                                                    P11_TAlojP, CodTipMot, CodModViaje, P28_Total, CodEstCivil, P9_GruViaje, 
                                                    P10_Pernocto, GastoTotalXPers, GastoFin, GastoTotalXPersXDia, Ciudad1, Zona1))

## Filtrado solo para los datos de Gasto y Perfil
EGYPV2016TNF <- subset(EGYPV2016TN,Validas == "Gasto y Perfil")
## Filtrado solo para todas las zona menos la desconocida
EGYPV2016TNF2 <- subset(EGYPV2016TNF, Zona1 %in% c("Zona Centro","Zona Insular",
                                                   "Zona Norte","Zona Occidental",
                                                   "Zona Oriental","Zona Sur"))

# Variables adicionales
GastoFinN = EGYPV2016TNF2$GastoFin
glevels = factor(EGYPV2016TNF2$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
gl
LogGFN = na.exclude(log(GastoFinN))

save.image("~/Documents/Modelos_Multinivel/Datos/Datos2016.RData")


#####################################################################################
#               Datos para el 2021 titulos en ingles
#####################################################################################
ECV2021 <- read.spss("~/Documents/Modelos_Multinivel/Datos/Base de la ECV 2021.sav")
ECV2021 <- data.frame(ECV2021)
## Filtrado de las variables que se necesitan para la estimación 

ECV2021N <- subset(ECV2021, select = c(Validas,Mes,Trimestre,P04,CodCiuRes,
                                       P04_RegionVA,P04_RegionVF,P10D, P11_Zona1,PGastoTotal))

## Filtrado solo para los datos de Gasto y Perfil
ECV2021N <- ECV2021N[!is.na(ECV2021N$PGastoTotal), ]
ECV2021N <- subset(ECV2021N, PGastoTotal > 0)

## Filtrado solo para todas las zona menos la desconocida
ECV2021NF <- subset(ECV2021N, P11_Zona1 %in% c("Zona Centro","Zona Insular",
                                               "Zona Norte","Zona Occidental",
                                               "Zona Oriental","Zona Sur"))
ECV2021NF <- ECV2021NF %>%
  mutate(P11_Zona1 = recode(P11_Zona1, 
                            "Zona Centro" = "Central Zone",
                            "Zona Insular" = "Insular Zone",
                            "Zona Norte" = "North Zone",
                            "Zona Occidental" = "Occidental Zone",
                            "Zona Oriental" = "Oriental Zone",
                            "Zona Sur" = "South Zone"))


## Conversión del Gasto Fin a escala Logarítmica
GastoTotal= ECV2021NF$PGastoTotal
glevels = factor(ECV2021NF$P11_Zona1)

gl = as.numeric(glevels[!is.na(log(GastoTotal))])
gl
LogGTN = na.exclude(log(GastoTotal))

# setwd("Modelos_Multinivel/Datos")
save.image("~/Documents/Modelos_Multinivel/Datos/Datos2021Ingles.RData")
rm(list = ls())
