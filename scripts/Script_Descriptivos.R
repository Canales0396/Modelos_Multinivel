library(xtable)
library(moments)
## Conversión del Gasto Fin a escala Logarítmica
GastoFinN = EGYPV2016TNF2$GastoFin
glevels = factor(EGYPV2016TNF2$Zona1)

gl = as.numeric(glevels[!is.na(log(GastoFinN))])
LogGFN = na.exclude(log(GastoFinN))
### Calculo de las estadísticas descriptivas de la variables Gasto Fin
### y por Zonas respectivamente

summary(EGYPV2016TNF2$GastoFin)
##Desviacion
sd(EGYPV2016TNF2$GastoFin,na.rm = TRUE)
## Asimetria
skewness(EGYPV2016TNF2$GastoFin,na.rm = TRUE)
## Curtosis
kurtosis(EGYPV2016TNF2$GastoFin,na.rm = TRUE)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(EGYPV2016TNF2$P10_3NumNoch)
summary(ECV2021NF$P10D)

## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP1<-subset(EGYPV2016TNF2,GastoFin > 700)
EGYP2<-subset(EGYPV2016TNF2,GastoFin <= 700)

## Estadística Descriptivas por Zonas
resumen_zonas <- EGYPV2016TNF2 %>%
  group_by(Zona1) %>%
  summarise(
    q1 = quantile(GastoFin, 0.25,na.rm = TRUE),
    Media = mean(GastoFin, na.rm = TRUE),
    Mediana = median(GastoFin, na.rm = TRUE),
    DesvEstandar = sd(GastoFin, na.rm = TRUE),
    q3 = quantile(GastoFin, 0.75,na.rm = TRUE),
    Minimo = min(GastoFin, na.rm = TRUE),
    Maximo = max(GastoFin, na.rm = TRUE),
    asimetria=skewness(GastoFin,na.rm = TRUE),
    Curtosis=kurtosis(GastoFin,na.rm=TRUE)
  )
print(resumen_zonas)
##Codigo latex de la tabla
xtable(print(resumen_zonas,simplify = FALSE, digits = 2))
resumen_zonas1 <- EGYPV2016TNF2 %>%
  group_by(Zona1) %>%
  count()
print(resumen_zonas1)
##Codigo latex de la tabla
xtable(print(resumen_zonas1,simplify = FALSE, digits = 2))
## Porcentaje de valores atipicos
Q1 <- quantile(GastoFinN, 0.25,na.rm = TRUE)
Q3 <- quantile(GastoFinN, 0.75,na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para detectar valores atípicos
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

# Calcular porcentaje de valores atípicos
valores_atipicos <- GastoFinN[GastoFinN < lim_inf | GastoFinN > lim_sup]
porcentaje_atipicos <- (length(valores_atipicos) / length(GastoFinN)) * 100

cat("Porcentaje de valores atípicos del gasto:", porcentaje_atipicos, "%\n")



#####################################################################################
#               Datos para el 2021
#####################################################################################

### Calculo de las estadísticas descriptivas de la variables Gasto Fin
### y por Zonas respectivamente

summary(ECV2021NF$PGastoTotal)
##Desviacion
sd(ECV2021NF$PGastoTotal,na.rm = TRUE)
## Asimetria
skewness(ECV2021NF$PGastoTotal)
## Curtosis
kurtosis(ECV2021NF$PGastoTotal,na.rm = TRUE)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(ECV2021NF$P10D)

## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP11<-subset(ECV2021NF,PGastoTotal > 700)
EGYP22<-subset(ECV2021NF,PGastoTotal<=700)

## Estadística Descriptivas por Zonas
resumen_zonas <- ECV2021NF %>%
  group_by(P11_Zona1) %>%
  summarise(
    q1 = quantile(PGastoTotal , 0.25,na.rm = TRUE),
    Media = mean(PGastoTotal , na.rm = TRUE),
    Mediana = median(PGastoTotal , na.rm = TRUE),
    DesvEstandar = sd(PGastoTotal , na.rm = TRUE),
    q3 = quantile(PGastoTotal , 0.75,na.rm = TRUE),
    Minimo = min(PGastoTotal , na.rm = TRUE),
    Maximo = max(PGastoTotal , na.rm = TRUE),
    asimetria=skewness(PGastoTotal),
    Curtosis=kurtosis(PGastoTotal ,na.rm=TRUE)
  )
print(resumen_zonas)

##Codigo latex de la tabla
xtable(print(resumen_zonas,simplify = FALSE, digits = 2))
resumen_zonas1 <- ECV2021NF%>%
  group_by(P11_Zona1) %>%
  count()
print(resumen_zonas1)
##Codigo latex de la tabla
xtable(print(resumen_zonas1,simplify = FALSE, digits = 2))
## Porcentaje de valores atipicos
Q1 <- quantile(ECV2021NF$PGastoTotal , 0.25,na.rm = TRUE)
Q3 <- quantile(ECV2021NF$PGastoTotal , 0.75,na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para detectar valores atípicos
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

# Calcular porcentaje de valores atípicos
valores_atipicos <- ECV2021NF$PGastoTotal[ECV2021NF$PGastoTotal< lim_inf | ECV2021NF$PGastoTotal> lim_sup]
porcentaje_atipicos <- (length(valores_atipicos) / length(ECV2021NF$PGastoTotal)) * 100

cat("Porcentaje de valores atípicos del gasto:", porcentaje_atipicos, "%\n")

q4 = quantile(ECV2021NF$PGastoTotal, 0.87,na.rm = TRUE)
q4

### Calculo de las estadísticas descriptivas de la variables Gasto Fin
### y por Procedencia respectivamente

summary(ECV2021NF$PGastoTotal)
##Desviacion
sd(ECV2021NF$PGastoTotal,na.rm = TRUE)
## Asimetria
skewness(ECV2021NF$PGastoTotal)
## Curtosis
kurtosis(ECV2021NF$PGastoTotal,na.rm = TRUE)


## Resumen de los turisticas de cuantas noches pernotaron en el pais
summary(ECV2021NF$P10D)

pn<-summarise(ECV2021NF$P10D)
xtable(print(pn),simplify=FALSE,digits = 2)
## Base para identificar los turistas que gastaron arriba y menos de 700 dolares

EGYP11<-subset(ECV2021NF,PGastoTotal > 700)
EGYP22<-subset(ECV2021NF,PGastoTotal<=700)

## Estadística Descriptivas por procedencia
resumen_procedencia <- ECV2021NF %>%
  group_by(Procedencia) %>%
  summarise(
    q1 = quantile(PGastoTotal , 0.25,na.rm = TRUE),
    Media = mean(PGastoTotal , na.rm = TRUE),
    Mediana = median(PGastoTotal , na.rm = TRUE),
    DesvEstandar = sd(PGastoTotal , na.rm = TRUE),
    q3 = quantile(PGastoTotal , 0.75,na.rm = TRUE),
    Minimo = min(PGastoTotal , na.rm = TRUE),
    Maximo = max(PGastoTotal , na.rm = TRUE),
    asimetria=skewness(PGastoTotal),
    Curtosis=kurtosis(PGastoTotal ,na.rm=TRUE)
  )
print(resumen_procedencia)

##Codigo latex de la tabla
xtable(print(resumen_procedencia,simplify = FALSE, digits = 2))
resumen_procendecia1 <- ECV2021NF%>%
  group_by(Procedencia) %>%
  count()
print(resumen_procendecia1)
##Codigo latex de la tabla
xtable(print(resumen_procendecia1,simplify = FALSE, digits = 2))
## Porcentaje de valores atipicos
Q1 <- quantile(ECV2021NF$PGastoTotal , 0.25,na.rm = TRUE)
Q3 <- quantile(ECV2021NF$PGastoTotal , 0.75,na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para detectar valores atípicos
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

# Calcular porcentaje de valores atípicos
valores_atipicos <- ECV2021NF$PGastoTotal[ECV2021NF$PGastoTotal< lim_inf | ECV2021NF$PGastoTotal> lim_sup]
porcentaje_atipicos <- (length(valores_atipicos) / length(ECV2021NF$PGastoTotal)) * 100

cat("Porcentaje de valores atípicos del gasto:", porcentaje_atipicos, "%\n")

q4 = quantile(ECV2021NF$PGastoTotal, 0.87,na.rm = TRUE)
q4

########
## Estadística Descriptivas por Zonas (Gasto logarítmico)
resumen_zonas_log <- ECV2021NF %>%
  group_by(P11_Zona1) %>%
  summarise(
    q1 = quantile(log(PGastoTotal), 0.25, na.rm = TRUE),
    Mediana = median(log(PGastoTotal), na.rm = TRUE),
    DesvEstandar = sd(log(PGastoTotal), na.rm = TRUE),
    q3 = quantile(log(PGastoTotal), 0.75, na.rm = TRUE),
    Minimo = min(log(PGastoTotal), na.rm = TRUE),
    Maximo = max(log(PGastoTotal), na.rm = TRUE),
    asimetria = skewness(log(PGastoTotal), na.rm = TRUE),
    Curtosis = kurtosis(log(PGastoTotal), na.rm = TRUE)
  )
print(resumen_zonas_log)

## Código LaTeX de la tabla
## Estadística Descriptivas por Zonas (Gasto logarítmico)
xtable(print(resumen_zonas_log, simplify = FALSE, digits = 2))

## Estadística Descriptivas por Procedencia (Gasto logarítmico)
resumen_procedencia_log <- ECV2021NF %>%
  group_by(Procedencia) %>%
  summarise(
    q1 = quantile(log(PGastoTotal), 0.25, na.rm = TRUE),
    Mediana = median(log(PGastoTotal), na.rm = TRUE),
    DesvEstandar = sd(log(PGastoTotal), na.rm = TRUE),
    q3 = quantile(log(PGastoTotal), 0.75, na.rm = TRUE),
    Minimo = min(log(PGastoTotal), na.rm = TRUE),
    Maximo = max(log(PGastoTotal), na.rm = TRUE),
    asimetria = skewness(log(PGastoTotal), na.rm = TRUE),
    Curtosis = kurtosis(log(PGastoTotal), na.rm = TRUE)
  )
print(resumen_procedencia_log)
## Estadística Descriptivas por Procedencia (Gasto logarítmico)
xtable(print(resumen_procedencia_log, simplify = FALSE, digits = 2))
