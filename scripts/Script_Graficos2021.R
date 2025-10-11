library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

load("~/Documents/GitHub/Modelos_Multinivel/Datos/Datos2021.RData")
## Grafico de Caja del Gasto y Densidad
ECV2021NF %>% 
  filter(PGastoTotal>= 200, PGastoTotal <= 10000) %>%
  ggplot(aes(x = "", y = PGastoTotal))+
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha=0.7,
    fill="#2297E7"
  )+
  geom_boxplot(
    width = 0.12,
    alpha = 0.7,
    fill="#2297E7",
    outlier.color = "red"
  )+
  theme_tq() +
  labs(
    title = "Gráfico de densidad y caja para el gasto \n por estadía, 2021.",
    x = "",
    y = "Gasto por estadía",
  ) +
  coord_flip()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.45))+
  theme(text = element_text(family = "Times New Roman"))

## Grafico por zonas
Zona_1<-ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                          "Zona Oriental","Zona Sur"),
         PGastoTotal>=200, PGastoTotal<= 5000) %>% 
  ggplot(aes(x = factor(P11_Zona1), y = PGastoTotal, fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plots del gasto turístico por zona",
    x = "Zonas",
    y = "Gasto turîstico",
    fill = "Zonas"
  )+ 
  coord_flip()+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.44))+
  theme(text = element_text(family = "Times New Roman"))
library(ggplot2)
library(ggdist)

## Otro grafico revisar esto
## Grafico por zonas gasto logaritmico
Zona_log<-ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Zona Centro", "Zona Insular", "Zona Norte","Zona Occidental",
                          "Zona Oriental","Zona Sur"))%>% 
  ggplot(aes(x = factor(P11_Zona1), y =log(PGastoTotal), fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA)+
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7)+
  theme_tq() +
  labs(title = "Densidades y Box-plots del gasto turístico por zona (escala logarítmica)", 
       x = "Zonas", y = "Logaritmo del gasto", fill = "Zonas")+ 
  coord_flip()+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(family = "Times New Roman"))

# Grafico por procedencia del turista
ProceGlobal<-
  
  ECV2021NF %>% 
  filter(Procedencia %in% c("Caribe", "Centroamérica", "Europa", 
                            "Norteamérica", "Suramérica"),
         PGastoTotal>=200, PGastoTotal<= 5000) %>% 
  ggplot(aes(x = factor(Procedencia), y = PGastoTotal, fill = factor(Procedencia)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = ,
    x = "",
    y = expression(y[ij]),
    fill = 
  )+ 
  coord_flip()+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.44))+
  theme(text = element_text(family = "Times New Roman"))

# Grafico por procedencia del turista logaritmico
ProceLog<-ECV2021NF %>% 
  filter(Procedencia %in% c("Caribe", "Centroamérica", "Europa", 
                            "Norteamérica", "Suramérica"),
  ) %>% 
  ggplot(aes(x = factor(Procedencia), y = log(PGastoTotal), fill = factor(Procedencia)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = "Densidades y Box-plots del gasto turístico por procedencia (escala logaritmica)",
    x = "Procedencia",
    y = expression(log(y[ij])),
    fill = "Procedencia"
  )+ 
  coord_flip()+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.44))+
  theme(text = element_text(family = "Times New Roman"))


########################################################################
## Graficos en Ingles
#######################################################################
load("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021Ingles.RData")

## Grafico de Caja del Gasto y Densidad
ECV2021NF %>% 
  filter(PGastoTotal>= 200, PGastoTotal <= 10000) %>%
  ggplot(aes(x = "", y = PGastoTotal))+
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha=0.7,
    fill="#2297E7"
  )+
  geom_boxplot(
    width = 0.12,
    alpha = 0.7,
    fill="#2297E7",
    outlier.color = "red"
  )+
  theme_tq() +
  labs(
    title = "Density and box chart for the of spend per stay, 2021.",
    x = "",
    y = "Spend per stay",
  ) +
  coord_flip()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.45))+
  theme(text = element_text(family = "Times New Roman"))
## Grafico por zonas
ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Central Zone", "Insular Zone", "North Zone","Occidental Zone",
                          "Oriental Zone","South Zone"),
         PGastoTotal>=200, PGastoTotal<= 5000) %>% 
  ggplot(aes(x = factor(P11_Zona1), y = PGastoTotal, fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.7
  )+
  theme_tq() +
  labs(
    title = "Graph of densities and boxes of the expenditure per stay.",
    x = "",
    y = "Spend per stay",
    fill = "Zones"
  )+ 
  coord_flip()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.44))+
  theme(text = element_text(family = "Times New Roman"))
library(ggplot2)
library(ggdist)

## Otro grafico revisar esto
## Grafico por zonas gasto logaritmico
ECV2021NF %>% 
  filter(P11_Zona1 %in% c("Central Zone", "Insular Zone", "North Zone","Occidental Zone",
                          "Oriental Zone","South Zone"))%>% 
  ggplot(aes(x = factor(P11_Zona1), y =log(PGastoTotal), fill = factor(P11_Zona1)))+
  # add half-violin from {ggdist} package
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA)+
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7)+
  theme_tq() +
  labs(title = "Graph of densities and boxes of the expenditure  per stay, logarithmic scale.", 
       x = "", y = "Logarithm of expenditure", fill = "Zones")+ 
  coord_flip()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(family = "Times New Roman"))




