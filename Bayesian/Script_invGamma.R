library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021.RData")

# Compilar el codigo Stan del modelo multinivel
sf1 <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_invgamma.stan"
sm1 <- cmdstan_model(sf1,force_recompile = TRUE)
# --------------------------------------
# 2. Validaciones previas
# --------------------------------------
stopifnot(all(GastoTotal > 0))  # Gamma requiere y > 0

# Validar índices de agrupamiento
stopifnot(length(GastoTotal) == length(gl1))
stopifnot(all(gl1 >= 1 & gl1 <= 6))

stopifnot(length(GastoTotal) == length(gl2))
stopifnot(all(gl2 >= 1 & gl2 <= 6))

stopifnot(length(GastoTotal) == length(gl3))
stopifnot(all(gl3 >= 1 & gl3 <= 34))
## Global
d1_in = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = 1 /GastoTotal)
## Zona visitada
d2_in = list(n = length(GastoTotal), J = 6, group = gl1, y = 1 /GastoTotal)
## Procedencia
d3_in= list(n = length(GastoTotal), J = 6, group = gl2, y = 1 /GastoTotal)
## Procedencia y Zona
d4_in = list(n = length(GastoTotal), J = 34, group = gl3, y = 1 /GastoTotal)

# mcmc para modelo multinivel GammaInversa
fit1_in <- sm1$sample(data = d1_in, chains = 4, parallel_chains = 4, refresh = 500)
fit2_in <- sm1$sample(data = d2_in, chains = 4, parallel_chains = 4, refresh = 500)
fit3_in<- sm1$sample(data = d3_in, chains = 4, parallel_chains = 4, refresh = 500)
fit4_in <- sm1$sample(data = d4_in, chains = 4, parallel_chains = 4, refresh = 500)

fv = fit2$draws(variables = c("mu","mu_group","sigma","alpha"),
               format = "matrix")

colnames(fv) = c("mu",
                 paste0("mu_",levels(glevels1)),
                 paste0("alpha_",levels(glevels1)),
                 'sigma')

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv[,c(5:8,14)])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
yrep = fit2$draws(variables = c("y_rep"), format = "matrix")
ppc_dens_overlay_grouped(1/GastoTotal, yrep, group = glevels1)

# Leave one out modelo multinivel
print(loo_compare(fit1_in$loo(), fit2_in$loo(), fit3_in$loo()), simplify = FALSE)

## result = Hierarchical-Procedencia fit3