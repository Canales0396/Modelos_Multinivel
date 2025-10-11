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
sf1 <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_invgamma.stan"
sm1 <- cmdstan_model(sf1, force_recompile = TRUE)

# --------------------------------------
# 2. Validaciones previas
# --------------------------------------
stopifnot(all(GastoTotal > 0))  # 🔧 CORRECCIÓN: Inv-Gamma también requiere y>0 (comentario)

# Validar índices de agrupamiento
stopifnot(length(GastoTotal) == length(gl1))
stopifnot(all(gl1 >= 1 & gl1 <= 6))

stopifnot(length(GastoTotal) == length(gl2))
stopifnot(all(gl2 >= 1 & gl2 <= 5))

stopifnot(length(GastoTotal) == length(gl3))
stopifnot(all(gl3 >= 1 & gl3 <= 29))

# 🔧 CORRECCIÓN: asegurar factores para etiquetas (si aún no existen)
glevels1 <- if (exists("glevels1")) glevels1 else factor(gl1)
glevels2 <- if (exists("glevels2")) glevels2 else factor(gl2)
glevels3 <- if (exists("glevels3")) glevels3 else factor(gl3)

## Global
d1_in = list(n = length(GastoTotal), J = 1,  group = rep(1, length(GastoTotal)), y = GastoTotal)                             
## Zona visitada
d2_in = list(n = length(GastoTotal), J = 6,  group = as.integer(gl1), y = GastoTotal)                             
## Procedencia
d3_in = list(n = length(GastoTotal), J = 5,  group = as.integer(gl2),y = GastoTotal)                             
## Procedencia y Zona
d4_in = list(n = length(GastoTotal), J = 29, group = as.integer(gl3),y = GastoTotal)                            

# mcmc para modelo multinivel Inverse-Gamma
fit1_in <- sm1$sample(data = d1_in, chains = 4, parallel_chains = 4, refresh = 500)
fit2_in <- sm1$sample(data = d2_in, chains = 4, parallel_chains = 4, refresh = 500)
fit3_in <- sm1$sample(data = d3_in, chains = 4, parallel_chains = 4, refresh = 500)
fit4_in <- sm1$sample(data = d4_in, chains = 4, parallel_chains = 4, refresh = 500)

# Extraer cadenas del modelo ganodor Procedencia Multinivel
fv <- fit3_in$draws(variables = c("mu","mu_group","sigma","alpha","beta"),
                    format = "matrix")

# Renombrado seguro según niveles del factor
colnames(fv) <- c("mu",
                  paste0("mu_", levels(glevels2)),
                  "sigma",
                  paste0("alpha_", levels(glevels2)),
                  paste0("beta_",  levels(glevels2)))

# resumen de las cadenas
summarize_draws(fv)
xtable(print(summarize_draws(fv), simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1  = mcmc_combo(fv[,1:3],  gg_theme = theme(legend.position = "none"))
g2  = mcmc_combo(fv[,4:6])
g3  = mcmc_combo(fv[,7:9],  gg_theme = theme(legend.position = "none"))
g4  = mcmc_combo(fv[,10:12])
g5  = mcmc_combo(fv[,13:17])
cowplot::plot_grid(g1, g2, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g3, g4, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g5, ncol = 1, rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################
sple = sample(1:4000,500)
yrep <- fit3_in$draws(variables = "y_rep", format = "matrix")
stopifnot(ncol(yrep) == length(GastoTotal))  # chequeo de dimensiones
ppc_dens_overlay_grouped( y = log(GastoTotal),log(yrep [sple,]),group = glevels2
                         )+ facet_wrap(~ group, scales = "free")
                         

# Leave one out modelo multinivel
loo_res_in <-loo_compare(fit1_in$loo(), fit2_in$loo(), fit3_in$loo(), fit4_in$loo())
print(loo_res_in, simplify = FALSE)
xtable(print(loo_res_in, simplify = FALSE, digits = 2))
## result = Hierarchical-Procedencia fit3 (comentario informativo)



