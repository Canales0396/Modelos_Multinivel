library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

# Cargar datos
load("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021.RData")
source("~/Documents/Github/Modelos_Multinivel/scripts/utils.R")

# Compilar el modelo Stan (Gamma)
sf <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_gamma.stan"
sm <- cmdstan_model(sf, force_recompile = TRUE)  # Eliminamos línea redundante

# La lista de datos que Stan necesita para hacer mcmc

## Escenario Global (sin estructura jerárquica)
d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)

## Zona visitada
d2 = list(n = length(GastoTotal), 
          J = nlevels(glevels1), 
          group = as.integer(gl1),  # aseguramos índice numérico
          y = GastoTotal)

## Procedencia del turista
d3 = list(n = length(GastoTotal), 
          J = nlevels(glevels2), 
          group = as.integer(gl2),  #aseguramos índice numérico
          y = GastoTotal)

## Zona + Procedencia
d4 = list(n = length(GastoTotal), 
          J = nlevels(glevels3), 
          group = as.integer(gl3),  # aseguramos índice numérico
          y = GastoTotal)

# Ajustar modelos MCMC para cada escenario
fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500,
                  adapt_delta = 0.9999, max_treedepth = 15)  

fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
fit3 <- sm$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
fit4 <- sm$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)

# Extraer cadenas para escenario Global
fv = fit1$draws(variables = c("mu", "mu_group", "sigma", "alpha", "beta"), format = "matrix")

# Validar orden original antes de renombrar
print(colnames(fv))

# Resumen estadístico de los parámetros
summarize_draws(fv)
xtable(print(summarize_draws(fv), simplify = FALSE, digits = 2))

# Visualización de posteriori
color_scheme_set("blue")
g2 <- mcmc_combo(fv[, c("mu_group[1]", "alpha[1]", "beta[1]")])
g1 <- mcmc_combo(fv[, c("mu", "sigma")], gg_theme = theme(legend.position = "none"))
cowplot::plot_grid(g1, g2, ncol = 2, rel_widths = c(1.1, 1.2))

# -----------------------------------------
# Posterior Predictive Check (ppc)
# -----------------------------------------
sple = sample(1:4000,500)
yrep = fit1$draws(variables = c("y_rep"), format = "matrix")
ppc_dens_overlay(GastoTotal, yrep[sple,])


# -----------------------------------------
# Comparación de modelos con LOO
# -----------------------------------------
# Ganador Gamma Global
loo_res <- loo_compare(fit1$loo(), fit2$loo(), fit3$loo(), fit4$loo())
print(loo_res, simplify = FALSE)
xtable(print(loo_res, simplify = FALSE, digits = 2))



