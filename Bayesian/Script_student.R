library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Github/Modelos_Multinivel/Datos/Datos2021.RData")

sf <- "~/Documents/Github/Modelos_Multinivel/Stancodes/ML_t.stan"
sm <- cmdstan_model(sf)

# La lista de datos que Stan necesita para hacer mcmc
## Global
d1 = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
## Zona visitada
d2 = list(n = length(LogGTN), J = 6, group = gl1, y = LogGTN)
## Procedencia
d3 = list(n = length(LogGTN), J = 6, group = gl2, y = LogGTN)
## Procedencia y Zona
d4 = list(n = length(LogGTN), J = 33, group = gl3, y = LogGTN)

# mcmc para modelo multinivel
fit1 <- sm$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)
fit2 <- sm$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500)
fit3 <- sm$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500)
fit4 <- sm$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500)

# extraer las cadenas de las variables importantes multinivel student_t
fv1 = fit1$draws(variables = c("mu","mu_group","nu_group","sigma"),format = "matrix")
fv2 = fit2$draws(variables = c("mu","mu_group","nu_group","sigma"),format = "matrix")
fv3 = fit3$draws(variables = c("mu","mu_group","nu_group","sigma"),format = "matrix")
fv4 = fit4$draws(variables = c("mu","mu_group","nu_group","sigma"),format = "matrix")

cn <- colnames(fv4)
lv <- levels(factor(glevels3))

i_mu <- grepl("^mu_group\\[", cn)
i_nu <- grepl("^nu_group\\[", cn)

# extrae el índice k dentro de los corchetes, p.ej. "mu_group[10]" -> 10
km <- as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", cn[i_mu]))
kn <- as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", cn[i_nu]))

k_max <- max(c(km, kn), na.rm = TRUE)
if (length(lv) < k_max) {
  lv <- c(lv, paste0("extra_", seq_len(k_max - length(lv))))
}

cn[cn == "mu"]    <- "μ"
cn[cn == "sigma"] <- "σ"
cn[i_mu] <- paste0("μ_", lv[km])
if (any(i_nu)) cn[i_nu] <- paste0("ν_", lv[kn])

colnames(fv4) <- cn


# Resumen de la cadenas
summarise_draws(fv1)
summarise_draws(fv2)
summarise_draws(fv3)
summarise_draws(fv4)
xtable(summarise_draws(fv4))

# graficos de las posteriors multinivel student_t
mcmc_combo(fv4[,1:7], gg_theme = theme(legend.position = "none"))
mcmc_combo(fv4[,8:14])
g1  = mcmc_combo(fv4[,1:5],  gg_theme = theme(legend.position = "none"))
g2  = mcmc_combo(fv4[,6:10])
g3  = mcmc_combo(fv4[,11:15])
g4  = mcmc_combo(fv4[,16:20])
g5  = mcmc_combo(fv4[,21:25])
g6  = mcmc_combo(fv4[,26:30])
g7  = mcmc_combo(fv4[,31:35])
g8  = mcmc_combo(fv4[,36:40])
g9  = mcmc_combo(fv4[,41:45])
g10 = mcmc_combo(fv4[,46:50])
g11 = mcmc_combo(fv4[,51:55])
g12 = mcmc_combo(fv4[,56:60])
g13 = mcmc_combo(fv4[,61:65])
g14 = mcmc_combo(fv4[,66:68])

cowplot::plot_grid(g1, g2,ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g3, g4, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g5, g6, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g7, g8, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g9, g10, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g11, g12, ncol = 2, rel_widths = c(1.1, 1.2))
cowplot::plot_grid(g13, g14, ncol = 2, rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################
sple = log(sample(1:4000,500))
yrep = fit4$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,],group = glevels3) + 
  labs(title = "Posterior Predictive checks",
       subtitle = "Modelo student-t multinivel")
# Leave one out modelo multinivel
print(loo_compare(fit1$loo(), fit2$loo(), fit3$loo(), fit4$loo()), simplify = FALSE)
xtable(print(loo_compare(fit1$loo(), fit2$loo(), fit3$loo(), fit4$loo()), simplify = FALSE))
## result = Hierarchical2-zona visitada y Procedencia modelo fit4


lv   <- levels(glevels3)
keep <- glevels3 %in% lv[1:2]

ppc_dens_overlay_grouped(
  LogGTN[keep],
  yrep[sple, keep, drop = FALSE],
  group = droplevels(glevels3[keep])
) + labs(title = "Posterior Predictive checks",
         subtitle = "Modelo student-t multinivel")

idx  <- !is.na(LogGTN) & !is.na(glevels3)
y    <- LogGTN[idx]
grp  <- droplevels(glevels3[idx])

sple <- sample.int(nrow(yrep), 300)  # o 500
Yrep_log <- log(pmax(yrep[sple, idx, drop = FALSE], .Machine$double.eps))

bayesplot::ppc_dens_overlay_grouped(
  y, Yrep_log, group = grp,
  facet_args = list(ncol = 2, nrow = 2, scales = "free_x")  # 4 facetas por página
) + labs(title="Posterior Predictive checks", subtitle="Modelo t-student multinivel")

