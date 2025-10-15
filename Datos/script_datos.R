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

EGYPV2016Turistas <- read.spss("~/Documents/Github/Modelos_Multinivel/Datos/EGYPV 2016 F01 - Turistas.sav")
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

#####################################################################################
#               Datos para el 2016 para la regresión 2025
#####################################################################################

EGYPV2016 <- read.spss("~/Documents/Github/Modelos_Multinivel/Datos/EGYPV 2016 F01 - Turistas.sav")
EGYPV2016<-data.frame(EGYPV2016)
EGYPV2016$CodResOMT.1= as.character(EGYPV2016$CodResOMT)

## Zona visitada
EGYPV2016$Procedencia<- "Resto del Mundo"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1%in% c("Estados Unidos de América",
                                            "México","Canadá")]  <- "Norteamérica"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("El Salvador", "Guatemala", "Nicaragua", "Costa Rica", 
                                                   "Panamá", "Belice")] <- "Centroamérica"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador", 
                                                   "Paraguay", "Perú", "Uruguay", 
                                                   "Venezuela (República Bolivariana de)")] <- "Suramérica"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("Alemania", "Austria", "Bélgica", "Dinamarca", "España", "Finlandia", "Francia", 
                                                   "Irlanda", "Islandia", "Italia", "Lituania", "Luxemburgo", "Noruega", "Países Bajos", 
                                                   "Polonia", "Portugal", "Reino Unido", "República Checa", "Rumania", 
                                                   "Rusia (Federación de)", "Suecia", "Suiza", "Eslovenia","Israel")] <- "Europa"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("Cuba", "Dominica", "República Dominicana", "Puerto Rico", 
                                                   "San Vicente y las Granadinas","Haití")] <- "Caribe"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("China", "Provincia china de Taiwán", "India","Japón", 
                                                   "Corea del Norte (República Popular Democrática de)", "Corea del Sur (República de)", 
                                                   "Brunei Darussalam", "Nueva Zelandia","Australia")] <- "Asia"

EGYPV2016$Procedencia[EGYPV2016$CodResOMT.1 %in% c("Congo", "Sudáfrica")] <- "África"


## Filtrado de las variables que se necesitan para la estimación 
EGYPV2016N <- subset(EGYPV2016, select = c(Validas, TipViajero, TipVisitante, Mes, CodRes, RegRes, CodPrimVisita,P29_Mu4,
                                                    GruViaje, P10_3NumNoch, Pernocto, GruNoch, P11_THoteles, P11_TAmigos,
                                                    P11_TAlojP, CodTipMot, CodModViaje, P28_Total, CodEstCivil, P9_GruViaje, 
                                                    P10_Pernocto, GastoTotalXPers, GastoFin, GastoTotalXPersXDia, 
                                                    Ciudad1, Zona1,Procedencia,CodResOMT.1,ViaEnt, P9_NumPers,
                                                    P11_TotNoches,CodMotivo,CodAct1,CodEduca,P31_NR,CodIngreso,CodOcupa,TipoAloja,RecuentoGastos))


EGYPV2016N$P11_THoteles[is.na(EGYPV2016N$P11_THoteles)] <- 0
EGYPV2016N$P11_TAmigos[is.na(EGYPV2016N$P11_TAmigos)] <- 0
EGYPV2016N$P11_TAlojP[is.na(EGYPV2016N$P11_TAlojP)] <- 0
names(EGYPV2016N)[names(EGYPV2016N) == "RecuentoGastos"] <- "GruGasto"

## Filtrado solo para los datos de Gasto y Perfil
EGYPV2016NR<- subset(EGYPV2016N,Validas == "Gasto y Perfil")
## Filtrado solo para todas las zona menos la desconocida, igual para procedencia 

EGYPV2016NRF <- subset(EGYPV2016NR, 
                       Procedencia %in% c("Norteamérica", "Centroamérica", "Suramérica", "Europa", "Caribe") &
                        Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte", "Zona Occidental", "Zona Oriental", "Zona Sur"))
                       
#Creaciacion de variables
EGYPV2016NRF$Trimestre<- "Tri"
EGYPV2016NRF$Trimestre[EGYPV2016NRF$Mes %in% c("Enero", "Febrero", "Marzo")] <- "Trimestre 1"
EGYPV2016NRF$Trimestre[EGYPV2016NRF$Mes %in% c("Abril", "Mayo", "Junio")] <- "Trimestre 2"
EGYPV2016NRF$Trimestre[EGYPV2016NRF$Mes%in% c("Julio", "Agosto", "Septiembre")] <- "Trimestre 3"
EGYPV2016NRF$Trimestre[EGYPV2016NRF$Mes %in% c("Octubre", "Noviembre", "Diciembre")] <- "Trimestre 4"

#Creacion de variables para identificar si uso hotel o no
EGYPV2016NRF$Hotel<- ifelse(is.na(EGYPV2016NRF$P11_THoteles), NA,
                                 ifelse(EGYPV2016NRF$P11_THoteles > 0, 1, 0))
#Creacion de variables para identificar si uso casa de amigos
EGYPV2016NRF$Amigos<- ifelse(is.na(EGYPV2016NRF$P11_TAmigos), NA,
                                 ifelse(EGYPV2016NRF$P11_TAmigos > 0, 1, 0))
#Creacion de variables para identificar si uso hotel o no
EGYPV2016NRF$CasaP <- ifelse(is.na(EGYPV2016NRF$P11_TAlojP), NA,
                                 ifelse(EGYPV2016NRF$P11_TAlojP > 0, 1, 0))

EGYPV2016NRF$HotelUso  <- factor(ifelse(EGYPV2016NRF$Hotel == 1, "Sí",
                                        ifelse(EGYPV2016NRF$Hotel == 0, "No", NA)),
                                 levels = c("No", "Sí"))

EGYPV2016NRF$AmigosUso <- factor(ifelse(EGYPV2016NRF$Amigos == 1, "Sí",
                                        ifelse(EGYPV2016NRF$Amigos == 0, "No", NA)),
                                 levels = c("No", "Sí"))

EGYPV2016NRF$CasaPUso  <- factor(ifelse(EGYPV2016NRF$CasaP == 1, "Sí",
                                        ifelse(EGYPV2016NRF$CasaP == 0, "No", NA)),
                                 levels = c("No", "Sí"))


EGYPV2016NRF$gruviaje<- as.numeric(factor(EGYPV2016NRF$GruViaje,
                                           levels = c("Solo", "Con su pareja", "En familia", "En grupo")))
EGYPV2016NRF$PrimeraVisita<- as.numeric(factor(EGYPV2016NRF$CodPrimVisita,
                                          levels = c("Si", "No")))
EGYPV2016NRF$grunoche <- as.numeric(factor(EGYPV2016NRF$GruNoch,
                                           levels = c("De 1 a 3 Noches", 
                                                      "De 4 a 7 Noches", 
                                                      "De 8 a 10 Noches", 
                                                      "De 11 a 14 Noches", 
                                                      "De 15 a 28 Noches", 
                                                      "De 29 a 364 Noches"),
                                           ordered = TRUE))
# Eliminar NAs en el gasto FIN
EGYPV2016NRF<- EGYPV2016NRF[!is.na(EGYPV2016NRF$GastoFin), ]
EGYPV2016NRF <- EGYPV2016NRF[!is.na(EGYPV2016NRF$P10_3NumNoch),]
table(is.na(EGYPV2016NRF$P10_3NumNoch))

# Eliminar registros sin gasto (0 o negativos)
EGYPV2016NRF <- subset(EGYPV2016NRF, GastoFin > 0)
EGYPV2016NRF <- subset(EGYPV2016NRF, P10_3NumNoch > 0)
EGYPV2016NRF <- subset(EGYPV2016NRF, !is.na(Procedencia))
###Creacion de grugasto2
EGYPV2016NRF$grugasto2 <- cut(EGYPV2016NRF$GastoFin,
                              breaks = quantile(EGYPV2016NRF$GastoFin, probs = seq(0, 1, 1/3), na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = c("Gasto Bajo", "Gasto Medio", "Gasto Alto"))

table(EGYPV2016NRF$grugasto2)

### Base Final
EGYPV2016REG <- subset(EGYPV2016NRF, select = c(Validas,Mes,Trimestre,CodResOMT.1,Procedencia,Ciudad1, Zona1, 
                                                Grugasto, GastoTotalXPers, GastoFin, GastoTotalXPersXDia,grugasto2, 
                                                P9_NumPers, GruViaje, gruviaje,P10_3NumNoch, GruNoch, grunoche, 
                                                CodPrimVisita, PrimeraVisita, P11_THoteles, Hotel, HotelUso, 
                                                P11_TAmigos, Amigos, AmigosUso, P11_TAlojP, CasaP, CasaPUso,
                                                CodIngreso,CodOcupa,CodEduca, CodEstCivil, CodTipMot, CodModViaje,
                                                P28_Total,ViaEnt,P11_TotNoches))
                                                

## Conversión del Gasto Fin a escala Logarítmica
GastoTotal= EGYPV2016REG$GastoFin
LogGTN = na.exclude(log(GastoTotal))

#### Base Final para realize la regression 

glevels1 = factor(EGYPV2016REG$Zona1)
glevels2 = factor(EGYPV2016REG$Procedencia)
table(glevels1)
table(glevels2)
gl1 = as.numeric(glevels1[!is.na(log(GastoTotal))])
gl2 = as.numeric(glevels2[!is.na(log(GastoTotal))])

## Niveles combinados
glevels3 = factor(paste(EGYPV2016REG$Zona1,EGYPV2016REG$Procedencia))
table(glevels3)
gl3 = as.numeric(glevels3[!is.na(log(GastoTotal))])
gl3

## Variables para la regression
NumNoches=as.numeric(EGYPV2016REG$P10_3NumNoch)
NumPersonas=as.numeric(EGYPV2016REG$P9_NumPers)

# setwd("Modelos_Multinivel/Datos")
save.image("~/Documents/Github/Modelos_Multinivel/Datos/DatosRegresion2016.RData")
rm(list = ls())
