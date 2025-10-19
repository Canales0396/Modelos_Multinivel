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


#####################################################################################
#               Datos Regresion para el 2021
#####################################################################################
ECV2021R <- read.spss("~/Documents/Github/Modelos_Multinivel/Datos/Base de la ECV 2021Completa.sav")
ECV2021R <- data.frame(ECV2021R)
ECV2021R$P04.1 = as.character(ECV2021R$P04)

## Procedencia
ECV2021R$Procedencia <- "Proce"

ECV2021R$Procedencia[ECV2021R$P04.1 %in% c("Estados Unidos de América",
                                  "México","Canadá")]  <- "Norteamérica"

ECV2021R$Procedencia[ECV2021R$P04.1 %in% c("El Salvador", "Guatemala", "Nicaragua", "Costa Rica",
                                  "Panamá", "Belice")]  <- "Centroamérica"

ECV2021R$Procedencia[ECV2021R$P04.1 %in% c("Colombia", "Brasil", "Ecuador", "Argentina", "Perú", "Uruguay",
                                  "Bolivia", "Paraguay", "Chile")] <- "Suramérica"

ECV2021R$Procedencia[ECV2021R$P04.1 %in% c("Islas Caimán", "República Dominicana", 
                                  "Puerto Rico","Cuba")]  <- "Caribe"

ECV2021R$Procedencia[ECV2021R$P04.1 %in% c("España", "Alemania", "Francia", "Italia", "Suiza", "Reino Unido" , 
                                  "Países Bajos", "Polonia", "Portugal", "República Checa", "Grecia" ,
                                  "Lituania", "Eslovenia", "Austria", "Dinamarca" , "Irlanda", "Noruega",
                                  "Ucrania", "Bélgica","Israel", "Turquía", "Rusia (Federación de)","Nueva Zelanda")]  <- "Europa"

#ECV2021$zona[ECV2021$P04.1 %in% c("Israel", "Turquía", "Rusia (Federación de)")] 
####<- "Resto del Mundo"

ECV2021R$P11_1Hotel[is.na(ECV2021R$P11_1Hotel)] <- 0
ECV2021R$P11_1Amigos[is.na(ECV2021R$P11_1Amigos)] <- 0
ECV2021R$P11_1CasaP[is.na(ECV2021R$P11_1CasaP)] <- 0

## Filtrado solo para los datos de Gasto y Perfil
ECV2021NR<- subset(ECV2021R,Validas == "Gasto y Perfil")
## Filtrado solo para todas las zona menos la desconocida, igual para procedencia 

ECV2021NR <- subset(ECV2021NR, 
                       Procedencia %in% c("Norteamérica", "Centroamérica", "Suramérica", "Europa", "Caribe") &
                         P11_Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte", "Zona Occidental", "Zona Oriental", "Zona Sur"))

#Creaciacion de variables para hacer el modelo
ECV2021NR <- subset(ECV2021NR, P11_Zona1 != "Desconocido")
#Creacion de variables para identificar si uso hotel o no
ECV2021NR$Hotel<- ifelse(is.na(ECV2021NR$P11_1Hotel), NA,
                            ifelse(ECV2021NR$P11_1Hotel > 0, 1, 0))
#Creacion de variables para identificar si uso casa de amigos
ECV2021NR$Amigos<- ifelse(is.na(ECV2021NR$P11_1Amigos), NA,
                             ifelse(ECV2021NR$P11_1Amigos > 0, 1, 0))
#Creacion de variables para identificar si uso hotel o no
ECV2021NR$CasaP <- ifelse(is.na(ECV2021NR$P11_1CasaP), NA,
                             ifelse(ECV2021NR$P11_1CasaP > 0, 1, 0))

ECV2021NR$Uso_Hotel  <- factor(ifelse(ECV2021NR$Hotel == 1, "Sí",
                                        ifelse(ECV2021NR$Hotel == 0, "No", NA)),
                                 levels = c("No", "Sí"))

ECV2021NR$Uso_Amigos <- factor(ifelse(ECV2021NR$Amigos  == 1, "Sí",
                                        ifelse(ECV2021NR$Amigos == 0, "No", NA)),
                                 levels = c("No", "Sí"))

ECV2021NR$Uso_CasaP <- factor(ifelse(ECV2021NR$CasaP == 1, "Sí",
                                        ifelse(ECV2021NR$CasaP == 0, "No", NA)),
                                 levels = c("No", "Sí"))


ECV2021NR$gruviaje<- as.numeric(factor(ECV2021NR$gruviaje,
                                          levels = c("Viaja solo", "Con su pareja", "En familia", "En grupo")))
ECV2021NR$PrimeraVezHonduras<- as.numeric(factor(ECV2021NR$PrimeraVezHonduras,
                                               levels = c("Si", "No")))
ECV2021NR$IngresoFamiliar <- as.numeric(factor(ECV2021NR$IngresoFamiliar,
                                                  levels = c("US$ 5,000 o menos", 
                                                             "De US$ 5,001 a US$ 10,000", 
                                                             "De US$ 10,001 a US$ 20,000",
                                                             "De US$ 20,001 a US$ 30,000",
                                                             "De US$ 30,001 a US$ 50,000",
                                                             "De US$ 50,001 a US$ 70,000",
                                                             "De US$ 70,001 o más",
                                                             "No Responde"),
                                                  ordered = TRUE))

ECV2021NR$PuestoTrabajo <- as.numeric(factor(ECV2021NR$PuestoTrabajo,
                                                levels = c("Ama de Casa", "Desempleado", "Estudiante", "Jubilado",
                                                           "Operario, artesano u otro oficio", "Otro", "No Responde",
                                                           "Trabajador de servicios o vendedor", "Trabajador de servicios y vendedor",
                                                           "Personal de apoyo administrativo", "Técnico o profesional de nivel medio",
                                                           "Profesional", "Director o Gerente")))

# Eliminar NAs en el gasto FIN
ECV2021NR<- ECV2021NR[!is.na(ECV2021NR$PGastoTotal), ]
ECV2021NR <- ECV2021NR[!is.na(ECV2021NR$P10D),]
table(is.na(ECV2021NR$P10D))

# Eliminar registros sin gasto (0 o negativos)
ECV2021NR <- ECV2021NR[ECV2021NR$PGastoTotal > 0, ]
ECV2021NR <- ECV2021NR[ECV2021NR$P10D > 0, ]
ECV2021NR <- subset(ECV2021NR, !is.na(Procedencia))
ECV2021NR <- subset(ECV2021NR, P11_Zona1 != "Desconocido")

### Base Final
ECV2021REG <- subset(ECV2021NR, select = c(Validas, Mes, Trimestre, Procedencia, CodCiuRes,
                                                    P04_RegionVA, P04_RegionVF, P10A, P10D, P11_Zona1,
                                                    GruGasto, PGastoTotal, TipVisitante, Hotel,Uso_Hotel , Amigos,Uso_Amigos,
                                                    CasaP,Uso_CasaP, Ninguno,NumeroPersonas,gruviaje,IngresoFamiliar, 
                                                    MotivoViaje, P11_1Hotel,P11_1Amigos, P11_1CasaP,PuestoTrabajo))


ECV2021REG $Hotel[is.na(ECV2021REG $Hotel)] <- 0
ECV2021REG $Amigos[is.na(ECV2021REG $Amigos)] <- 0
ECV2021REG $CasaP[is.na(ECV2021REG $CasaP)] <- 0
ECV2021REG $Ninguno[is.na(ECV2021REG $Ninguno)] <- 0
## Conversión del Gasto Fin a escala Logarítmica

## Conversión del Gasto Fin a escala Logarítmica
GastoTotal= ECV2021REG$PGastoTotal
LogGTN = na.exclude(log(GastoTotal))

glevels1 = factor(ECV2021REG$P11_Zona1)
glevels2 = factor(ECV2021REG$Procedencia)
table(glevels1)
table(glevels2)

gl1 = as.numeric(glevels1[!is.na(log(GastoTotal))])
gl2 = as.numeric(glevels2[!is.na(log(GastoTotal))])

## Niveles combinados
glevels3 = factor(paste(ECV2021REG$P11_Zona1,ECV2021REG$Procedencia))
table(glevels3)
gl3 = as.numeric(glevels3[!is.na(log(GastoTotal))])
gl3
save.image("~/Documents/Github/Modelos_Multinivel/Datos/DatosRegresion2021.RData")
## Dejar solo lo niveles ue tengan ms datos para evitar problemas del modelo
ECV2021REG$glevels1<-glevels1
ECV2021REG$glevels2<-glevels2
ECV2021REG$glevels3<-glevels3

#onteo de grupos

conteos<-ECV2021REG %>%
  group_by(glevels3)%>%
  summarise(n=n())%>%
  arrange(desc(n))

grup_validos<-conteos%>%
  filter(n >=10) %>%
  pull(glevels3)



ECV2021REG <- ECV2021REG %>%
  filter(glevels3 %in% grup_validos)

## Conversión del Gasto Fin a escala Logarítmica con los nuevos grupos
GastoTotal= ECV2021REG$PGastoTotal
LogGTN = na.exclude(log(GastoTotal))

glevels1 = factor(ECV2021REG$P11_Zona1)
glevels2 = factor(ECV2021REG$Procedencia)
table(glevels1)
table(glevels2)

gl1 = as.numeric(glevels1[!is.na(log(GastoTotal))])
gl2 = as.numeric(glevels2[!is.na(log(GastoTotal))])

## Niveles combinados
glevels3 = factor(paste(ECV2021REG$P11_Zona1,ECV2021REG$Procedencia))
table(glevels3)
gl3 = as.numeric(glevels3[!is.na(log(GastoTotal))])
gl3

# setwd("Modelos_Multinivel/Datos")
save.image("~/Documents/Github/Modelos_Multinivel/Datos/DatosRegresionV22021.RData")
rm(list = ls())

