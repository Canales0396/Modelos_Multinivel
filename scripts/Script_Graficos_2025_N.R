# ---------------------------
# Librerías
# ---------------------------
library(dplyr)
library(ggplot2)
library(ggdist)
library(cowplot)
library(scales)
library(tidyquant) # para theme_tq


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

# ---------------------------
# Helpers y sets
# ---------------------------
proced_sel <- c("Caribe", "Centroamérica", "Europa", "Norteamérica", "Suramérica")
zona_sel   <- c("Zona Centro", "Zona Insular", "Zona Norte",
                "Zona Occidental", "Zona Oriental", "Zona Sur")

# Paleta (opcional, para consistencia visual)
pal <- scales::hue_pal()(6)  # paleta de tidyquant

# ---------------------------
# 1) Figuras por ZONA
# ---------------------------

# (A) Log-escala: log(y_ij)
g_zona_log <- ECV2021NF %>% 
  filter(P11_Zona1 %in% zona_sel) %>% 
  ggplot(aes(x = factor(P11_Zona1, levels = zona_sel),
             y = log(PGastoTotal),
             fill = factor(P11_Zona1, levels = zona_sel))) +
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7) +
  theme_tq() +
  labs(x = "", y = expression(log(y[ij]))) +
  coord_flip() +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"))

# (B) Escala real: y_ij (con recorte de rango como en tu código original)
g_zona_real <- ECV2021NF %>% 
  filter(P11_Zona1 %in% zona_sel,
         PGastoTotal >= 200, PGastoTotal <= 5000) %>% 
  ggplot(aes(x = factor(P11_Zona1, levels = zona_sel),
             y = PGastoTotal,
             fill = factor(P11_Zona1, levels = zona_sel))) +
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7) +
  theme_tq() +
  labs(x = "Zonas", y = expression(y[ij]), fill = "Zonas") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"))

# Extraer leyenda y limpiar duplicados
legend_zona <- get_legend(g_zona_real)
g_zona_real_clean <- g_zona_real + theme(legend.position = "none")

# Unir paneles + leyenda + título
zona_panels <- plot_grid(g_zona_real_clean,g_zona_log, labels = c("", ""), ncol = 2, align = "H")
zona_with_legend <- plot_grid(zona_panels, legend_zona, ncol = 2, rel_widths = c(1, 0.18))

title_zona <- ggdraw() + draw_text(
  "Densidades y Box-plots del gasto turístico por zona",
  fontface = 'bold', x = 0.5, hjust = 0.5, family = "Times New Roman"
)

fig_zona_final <- plot_grid(title_zona, zona_with_legend, ncol = 1, rel_heights = c(0.12, 1))
fig_zona_final
# Guardar
ggsave("Fig_Zonas_Log_vs_Real.png", fig_zona_final, width = 12, height = 6, dpi = 300)
ggsave("Fig_Zonas_Log_vs_Real.pdf", fig_zona_final, width = 12, height = 6)


# ---------------------------
# 2) Figuras por PROCEDENCIA
# ---------------------------

# (A) Log-escala: log(y_ij)
g_proc_log <- ECV2021NF %>% 
  filter(Procedencia %in% proced_sel) %>% 
  ggplot(aes(x = factor(Procedencia, levels = proced_sel),
             y = log(PGastoTotal),
             fill = factor(Procedencia, levels = proced_sel))) +
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7) +
  theme_tq() +
  labs(x = "", y = expression(log(y[ij]))) +
  coord_flip() +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"))

# (B) Escala real: y_ij (con tu recorte original para hacer foco)
g_proc_real <- ECV2021NF %>% 
  filter(Procedencia %in% proced_sel,
         PGastoTotal >= 200, PGastoTotal <= 5000) %>% 
  ggplot(aes(x = factor(Procedencia, levels = proced_sel),
             y = PGastoTotal,
             fill = factor(Procedencia, levels = proced_sel))) +
  stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.7) +
  theme_tq() +
  labs(x = "Procedencia", y = expression(y[ij]), fill = "Procedencia") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"))

legend_proc <- get_legend(g_proc_real)
g_proc_real_clean <- g_proc_real + theme(legend.position = "none")

proc_panels <- plot_grid(g_proc_real_clean, g_proc_log,labels = c("", ""), ncol = 2, align = "H")
proc_with_legend <- plot_grid(proc_panels, legend_proc, ncol = 2, rel_widths = c(1, 0.18))

title_proc <- ggdraw() + draw_text(
  "Densidades y Box-plots del gasto turístico por procedencia",
  fontface = 'bold', x = 0.5, hjust = 0.5, family = "Times New Roman"
)

fig_proc_final <- plot_grid(title_proc, proc_with_legend, ncol = 1, rel_heights = c(0.12, 1))
fig_proc_final
# Guardar
ggsave("Fig_Procedencia_Log_vs_Real.png", fig_proc_final, width = 12, height = 6, dpi = 300)
ggsave("Fig_Procedencia_Log_vs_Real.pdf", fig_proc_final, width = 12, height = 6)
