library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyquant)
library(forcats)
library(scales)

base_family <- "Times New Roman"

# Mapas y niveles
niveles_zona_raw <- c("Zona Centro","Zona Insular","Zona Norte","Zona Occidental","Zona Oriental","Zona Sur")
niveles_zona_lab <- c("Centro","Insular","Norte","Occidental","Oriental","Sur")
map_zona <- setNames(niveles_zona_lab, niveles_zona_raw)

niveles_proc <- c("Caribe","Centroamérica","Europa","Norteamérica","Suramérica")

# Datos limpios + recorte + quitar combos con muy pocos casos
df <- ECV2021NF %>%
  filter(P11_Zona1 %in% niveles_zona_raw,
         Procedencia %in% niveles_proc,
         !is.na(PGastoTotal)) %>%
  transmute(
    Zona = factor(recode(P11_Zona1, !!!map_zona), levels = niveles_zona_lab),
    Procedencia = factor(Procedencia, levels = niveles_proc),
    Valor = as.numeric(PGastoTotal)
  ) %>%
  filter(Valor >= 200, Valor <= 6000)

# filtra combos con al menos 3 observaciones (ajusta a tu gusto)
df <- df %>%
  add_count(Zona, Procedencia, name = "n") %>%
  filter(n >= 3)

# Ridgeline: Y = Zona, colores = Procedencia (UNA sola leyenda)
g <- ggplot(df, aes(x = Valor, y = Zona, fill = Procedencia)) +
  geom_density_ridges(
    alpha = 0.35,
    scale = 0.95,             # un poco menor para menos solapamiento
    rel_min_height = 0.01,
    color = "white",          # contorno sutil
    linewidth = 0.3,          # <- reemplaza 'size'
    position = "identity",
    bw = "SJ",                # ancho de banda "Sheather-Jones" (robusto); opcional
    na.rm = TRUE              # silencia warnings por NA
  ) +
  # Paleta bonita: elige una y comenta la otra
  scale_fill_brewer(palette = "Set2") +
  # scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Densidades del gasto turítico por zona según procedencia",
    x = "Gasto turístico", y = "Zonas", fill = "Procedencia"
  ) +
  theme_tq(base_family = base_family) +
  theme(
    legend.position = "right",          # UNA sola leyenda abajo
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
  scale_x_continuous(labels = comma)

g



library(ggplot2)
library(ggridges)
library(scales)
library(dplyr)

# Paleta consistente
pal <- scales::hue_pal()(6)

# Niveles ordenados
niveles_zona_raw <- c("Zona Centro","Zona Insular","Zona Norte","Zona Occidental","Zona Oriental","Zona Sur")
niveles_zona_lab <- c("Centro","Insular","Norte","Occidental","Oriental","Sur")
map_zona <- setNames(niveles_zona_lab, niveles_zona_raw)

niveles_proc <- c("Caribe","Centroamérica","Europa","Norteamérica","Suramérica")

# Filtrar y preparar datos
df <- ECV2021NF %>%
  filter(P11_Zona1 %in% niveles_zona_raw,
         Procedencia %in% niveles_proc,
         !is.na(PGastoTotal)) %>%
  transmute(
    Zona = factor(recode(P11_Zona1, !!!map_zona), levels = niveles_zona_lab),
    Procedencia = factor(Procedencia, levels = niveles_proc),
    Valor = as.numeric(PGastoTotal)
  ) %>%
  filter(Valor >= 200, Valor <= 6000)

# Gráfico ridgeline
g <- ggplot(df, aes(x = Valor, y = Zona, fill = Procedencia)) +
  geom_density_ridges(
    alpha = 0.45,
    scale = 0.95,
    rel_min_height = 0.01,
    color = "white",
    linewidth = 0.3,
    position = "identity",
   # adjust = 1.5,   # en lugar de 'bw'
    na.rm = TRUE
  ) +
  scale_fill_manual(values = pal) +
  labs(
    title = "",
    x = expression(y[ij]), 
    y = "Zonas", 
    fill = "Procedencia"   # se muestra bien
  ) +
  theme_tq(base_family = base_family) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold",hjust = 0.6)
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
  scale_x_continuous(labels = scales::comma)

g

    


