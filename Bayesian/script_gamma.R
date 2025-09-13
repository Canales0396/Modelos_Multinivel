library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021.RData")
source("~/Documents/Github/Modelos_Multinivel/scripts/utils.R")

# Compilar el codigo Stan del modelo multinivel
sf <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_gamma.stan"
sm <- cmdstan_model(sf)
sm <- cmdstan_model(sf, force_recompile = TRUE)

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
d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)
## Zona visitada
d2 = list(n = length(GastoTotal), J = 6, group = gl1, y = GastoTotal)
## Procedencia
d3 = list(n = length(GastoTotal), J = 6, group = gl2, y = GastoTotal)
## Procedencia y Zona
d4 = list(n = length(GastoTotal), J = 34, group = gl3, y = GastoTotal)

# mcmc para modelo multinivel
fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500,adapt_delta = 0.9999, max_treedepth = 15)
#fit1.1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500,adapt_delta = 0.999, max_treedepth = 15)
#fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500, iter_warmup = 1000, iter_sampling = 1000, adapt_delta = 0.99, max_treedepth = 15)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
fit3 <- sm$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
fit4 <- sm$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)

fv = fit4$draws(variables = c("mu","mu_group","sigma","alpha", "beta"),
              format = "matrix")
colnames(fv) = c("mu",
                 paste0("mu_",levels(glevels3)),
                 paste0("alpha_",levels(glevels3)),
                 paste("beta_",levels(glevels3)),
                 'sigma')

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv[,c(5:8,20)])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
yrep = fit4$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(GastoTotal, yrep, group = glevels3)

# Leave one out modelo multinivel con la distribuci
print(loo_compare(fit1$loo(), fit2$loo(), fit3$loo(),fit4$loo()),,simplify = FALSE)
xtable(print(loo_compare(fit1$loo(), fit2$loo(), fit3$loo(),fit4$loo()),,simplify = FALSE,digits=2))
## result = modelo Global fit1