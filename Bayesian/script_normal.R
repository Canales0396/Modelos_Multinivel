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
colnames(fv2) = c("μ",levels(glevels3),"σ")

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))

summarize_draws(fv2)
xtable(print(summarize_draws(fv2),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel Zona-Procedencia Fit4
color_scheme_set("blue")

g0 = mcmc_combo(fv2[,c(1,35)],gg_theme = theme(legend.position = "right"))
g1 = mcmc_combo(fv2[,2:4],gg_theme = theme(legend.position = "none"))
g1b = mcmc_combo(fv2[,5:7])
g2 = mcmc_combo(fv2[,8:10],gg_theme = theme(legend.position = "noe"))
g2b = mcmc_combo(fv2[,11:12])
g3 = mcmc_combo(fv2[,13:15],gg_theme = theme(legend.position = "none"))
g3b = mcmc_combo(fv2[,16:18])
g4 = mcmc_combo(fv2[,19:21],gg_theme = theme(legend.position = "none"))
g4b = mcmc_combo(fv2[,22:24])
g5 = mcmc_combo(fv2[,25:26],gg_theme = theme(legend.position = "none"))
g5b = mcmc_combo(fv2[,27:28])
g6 = mcmc_combo(fv2[,29:31],gg_theme = theme(legend.position = "none"))
g6b = mcmc_combo(fv2[,32:34])
cowplot::plot_grid(g0, ncol = 1)
cowplot::plot_grid(g1,g1b,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g2,g2b,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g3,g3b,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g4,g4b,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g5,g5b,ncol = 2,rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g6,g6b,ncol = 2,rel_widths = c(1.1, 1.2))


#cowplot::plot_grid(g3,g4,ncol = 2,rel_widths = c(1.1, 1.2))
#cowplot::plot_grid(g5,ncol = 1,rel_widths = c(1.1, 1.2))


                      
# Leave one out modelo multinivel
print(loo_compare(fit1$loo(), fit2$loo(),fit3$loo(),fit4$loo()),simplify = FALSE)
xtable(print(loo_compare(fit1$loo(), fit2$loo(),fit3$loo(),fit4$loo()),simplify = FALSE))
## result = Hierarchical2-zona visitada y Procedencia modelo fit4
###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel log normal
sple = sample(1:4000,500)
yrep = fit4$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(exp(LogGTN), yrep[sple,], group = glevels3)
