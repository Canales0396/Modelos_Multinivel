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

niveles_proc <- c("Caribe","Centro-América","Europa","Norte-América","Resto del Mundo","Sur-América")

# --- Filtro en escala original y luego LOG del gasto ---
df1 <- ECV2021NF %>%
  filter(P11_Zona1 %in% niveles_zona_raw,
         Procedencia %in% niveles_proc,
         !is.na(PGastoTotal),
         PGastoTotal >= 200, PGastoTotal <= 6000) %>%                 # recorte original
  transmute(
    Zona        = factor(recode(P11_Zona1, !!!map_zona), levels = niveles_zona_lab),
    Procedencia = factor(Procedencia, levels = niveles_proc),
    Valor       = log(as.numeric(PGastoTotal))                        # <-- LOG del gasto
  ) %>%
  add_count(Zona, Procedencia, name = "n") %>%
  filter(n >= 3)

# Ridgeline: Y = Zona, colores = Procedencia (UNA sola leyenda)
g2 <- ggplot(df1, aes(x = Valor, y = Zona, fill = Procedencia)) +
  geom_density_ridges(
    alpha = 0.35,
    scale = 0.95,             # un poco menor para menos solapamiento
    rel_min_height = 0.01,
    color = "white",          # contorno sutil
    linewidth = 0.3,          # <- reemplaza 'size'
    position = "identity",
    #bw = "SJ",                # ancho de banda "Sheather-Jones" (robusto); opcional
    na.rm = TRUE              # silencia warnings por NA
  ) +
  scale_fill_manual(values = pal) +   # (o tu paleta manual)
  labs(
    title = "",
    x = expression(log(y[ij])), y = "", fill = "Procedencia"
  ) +
  theme_tq(base_family = base_family) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face="bold",hjust = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE), legend.title = element_text(face = "bold")) +
  scale_x_continuous(labels = comma)  # (etiquetas numéricas; puedes quitar 'comma' si prefieres)

g2



# Panel con ambos gráficos
panels <- plot_grid(
  g + theme(legend.position = "none"),   # sin leyenda en el primero
  g2 + theme(legend.position = "none"),  # sin leyenda en el segundo
  labels = c("", ""), 
  ncol = 2, 
  align = "h"
)

# Extraer la leyenda de uno de los gráficos
legend_proc <- get_legend(
  g + theme(legend.position = "right")
)

# Combinar paneles + leyenda
with_legend <- plot_grid(
  panels, legend_proc, 
  ncol = 2, rel_widths = c(1, 0.18)
)

# Título en negrita, centrado
title_proc <- ggdraw() + draw_text(
  "Densidades del gasto turístico por zona según procedencia",
  fontface = 'bold',
  x = 0.5, hjust = 0.5, family = "Times New Roman"
)

# Figura final
fig_final <- plot_grid(
  title_proc, with_legend, 
  ncol = 1, rel_heights = c(0.12, 1)
)

fig_final

