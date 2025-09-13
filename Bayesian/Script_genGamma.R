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
sf2 <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_gG.stan"
sm2 <- cmdstan_model(sf2)

## Global
d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)
## Zona visitada
d2 = list(n = length(GastoTotal), J = 6, group = gl1, y = GastoTotal)
## Procedencia
d3 = list(n = length(GastoTotal), J = 6, group = gl2, y = GastoTotal)
## Procedencia y Zona
d4 = list(n = length(GastoTotal), J = 34, group = gl3, y = GastoTotal)

# mcmc para modelo multinivel
gG_fit1 <- sm2$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
gG_fit2 <- sm2$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
gG_fit3 <- sm2$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
gG_fit4 <- sm2$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)

gG_fv = gG_fit3$draws(variables = c("mu","k","mu_group"),
               format = "matrix")
colnames(gG_fv) = c("mu","k",
                 paste0("mu_group_",levels(glevels2)))

# resumen de las cadenas
summarize_draws(gG_fv)
xtable(print(summarize_draws(gG_fv),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
gG_g1 = mcmc_combo(gG_fv[,1:4],gg_theme = theme(legend.position = "none"))
gG_g2 = mcmc_combo(gG_fv[,c(5:8)])
cowplot::plot_grid(gG_g1,gG_g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = gG_fit3$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(GastoTotal, yrep[sple,], group = glevels2)

# Leave one out modelo multinivel
print(loo_compare(gG_fit1$loo(), gG_fit2$loo(), gG_fit3$loo(), gG_fit4$loo()),simplify = FALSE)
xtable(print(loo_compare(gG_fit1$loo(), gG_fit2$loo(), gG_fit3$loo(), gG_fit4$loo()),simplify = FALSE))
## result = Hierarchical-zona visitada y procedencia fit4

################################
# model 4
################################
gG_fv1 = gG_fit4$draws(variables = c("mu","k","mu_group"),
                      format = "matrix")
colnames(gG_fv1) = c("mu","k",
                    paste0("mu_group_",levels(glevels3)))

# resumen de las cadenas
summarize_draws(gG_fv1)
xtable(print(summarize_draws(gG_fv1),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
gG_g1 = mcmc_combo(gG_fv[,1:4],gg_theme = theme(legend.position = "none"))
gG_g2 = mcmc_combo(gG_fv[,c(5:8)])
cowplot::plot_grid(gG_g1,gG_g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = gG_fit3$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(GastoTotal, yrep[sple,], group = glevels2)