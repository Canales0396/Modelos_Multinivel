library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021.RData")

# Compilar el codigo Stan del modelo multinivel
sf <- "~/Documents/Modelos_Multinivel/Stancodes/ML_gamma.stan"
sm <- cmdstan_model(sf)

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(GastoTotal), J = 6, group = gl, y = 1 / GastoTotal)
d2 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), 
          y = 1 / GastoTotal)
# mcmc para modelo multinivel
fit  <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)

fv = fit$draws(variables = c("mu","mu_group","sigma","alpha"),
               format = "matrix")

colnames(fv) = c("mu",
                 paste0("mu_",levels(glevels)),
                 paste0("alpha_",levels(glevels)),
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
yrep = fit$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(1/GastoTotal, yrep, group = glevels)

# Leave one out modelo multinivel
print(loo_compare(fit$loo(), fit2$loo()),simplify = FALSE)

## result = Hierarchical-zona visitada