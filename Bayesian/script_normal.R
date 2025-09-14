library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Github//Modelos_Multinivel/Datos/Datos2021.RData")

sf <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_N.stan"
sm <- cmdstan_model(sf)

# La lista de datos que Stan necesita para hacer mcmc
## Global
d1 = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
## Zona visitada
d2 = list(n = length(LogGTN), J=nlevels(glevels1), group = gl1, y = LogGTN)
## Procedencia
d3 = list(n = length(LogGTN), J=nlevels(glevels2), group = gl2, y = LogGTN)
## Procedencia y Zona
d4 = list(n = length(LogGTN), J=nlevels(glevels3), group = gl3, y = LogGTN)

# mcmc para modelo multinivel
fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
fit3 <- sm$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
fit4 <- sm$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)


fv = fit2$draws(variables = c("mu","mu_group","sigma"),format = "matrix")
colnames(fv) = c("mu",levels(glevels1),'sigma')

fv1 = fit3$draws(variables = c("mu","mu_group","sigma"),format = "matrix")
colnames(fv1) = c("mu",levels(glevels2),'sigma')

fv2 = fit4$draws(variables = c("mu","mu_group","sigma"),format = "matrix")
colnames(fv2) = c("mu",levels(glevels3),'sigma')

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))

summarize_draws(fv2)
xtable(print(summarize_draws(fv2),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv2[,1:7],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv2[,8:14])
g3 = mcmc_combo(fv2[,15:21])
g4 = mcmc_combo(fv2[,22:28])
g5 = mcmc_combo(fv2[,29:35])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g3,g4,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g5,ncol = 1,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = fit4$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels3)

# Leave one out modelo multinivel
print(loo_compare(fit1$loo(), fit2$loo(),fit3$loo(),fit4$loo()),simplify = FALSE)
xtable(print(loo_compare(fit1$loo(), fit2$loo(),fit3$loo(),fit4$loo()),simplify = FALSE))
## result = Hierarchical2-zona visitada y Procedencia modelo fit4

# --- Paleta ---
color_scheme_set("blue")

# --- Función para hacer una "página" con hasta 16 parámetros (8 y 8) ---
make_page <- function(mat, ids, title = NULL){
  # primera mitad (hasta 8)
  left_ids  <- ids[seq_len(min(5, length(ids)))]
  # segunda mitad (si hay más de 8)
  right_ids <- if (length(ids) > 5) ids[(5+1):length(ids)] else integer(0)
  
  p_left  <- mcmc_combo(mat[, left_ids,  drop = FALSE],
                        gg_theme = theme(legend.position = "none"))
  p_right <- if (length(right_ids) > 0) {
    mcmc_combo(mat[, right_ids, drop = FALSE])
  } else {
    ggplot() + theme_void()
  }
  
  pg <- cowplot::plot_grid(p_left, p_right, ncol = 1, rel_widths = c(1.1, 1.2))
  if (!is.null(title)) {
    ttl <- ggplot() + theme_void() + ggtitle(title)
    pg  <- cowplot::plot_grid(ttl, pg, ncol = 1, rel_heights = c(0.07, 1))
  }
  pg
}

# --- Partir todos los parámetros en bloques de 16 ---
K <- ncol(fv2)
blocks <- split(seq_len(K), ceiling(seq_len(K) / 5))

# --- Generar todas las páginas ---
pages <- lapply(seq_along(blocks), function(b){
  make_page(fv2, blocks[[b]],
            title = paste0(""))
})

# --- Imprimir todas las páginas (35 → serán 3 páginas: 16 + 16 + 3) ---
for (p in pages) print(p)

