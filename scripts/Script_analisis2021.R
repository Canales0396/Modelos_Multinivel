library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/Modelos_Multinivel/Datos/Datos2021Ingles.RData")

compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 500)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

# Compilar el codigo Stan del modelo multinivel
sm1 <- cmdstan_model("~/Documents/Modelos_Multinivel/Stancodes/multi_level.stan")

# Compilar el codigo Stan del modelo de Gomez
sm2 <- cmdstan_model("~/Documents/Modelos_Multinivel/Stancodes/skew_normal.stan")

# Compilar el codigo Stan del modelo student
sm3 <- cmdstan_model("~/Documents/Modelos_Multinivel/Stancodes/ML_student.stan")

# Compilar el codigo Stan del modelo skew-normal-multi-nivel
sm4 <- cmdstan_model("~/Documents/Modelos_Multinivel/Stancodes/ML_skew_normal.stan")

# La lista de datos que Stan necesita para hacer mcmc
d1 = list(n = length(LogGTN), J = 6, group = gl, y = LogGTN)

# mcmc para modelo multinivel
fit1 <- sm1$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500)

# mcmc para modelo de Gomez
fit2 <- sm2$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# mcmc para modelo de Gomez
fit3 <- sm3$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# mcmc para modelo de Gomez
fit4 <- sm4$sample(data = d1, chains = 4, parallel_chains = 4,refresh = 500)

# extraer las cadenas de las variables importantes multinivel
fv1 = fit1$draws(variables = c("mu_group","sigma"),format = "matrix")
colnames(fv1) = c(levels(glevels),'sigma')

# resumen de las cadenas
summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
color_scheme_set("blue")
g1 = mcmc_combo(fv1[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv1[,5:7])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))

###########################################################
# Posterior predictive checks
###########################################################

# modelo multinivel normal
sple = sample(1:4000,500)
yrep = fit1$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels)

#modelo skew-normal
yrep = fit2$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay(LogGTN, yrep[sple,])

# modelo multinivel student
yrep = fit3$draws(variables = c("y_rep"),format = "matrix")
ppc_dens_overlay_grouped(LogGTN, yrep[sple,], group = glevels) 

# Leave one out modelo multinivel
ll1 = fit1$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll1), cores = 2, chain_id = rep(1:4, each = 1000))
loo1 = loo(ll1, r_eff = r_eff, cores = 2)

# Leave one out modelo Gomez
ll2 = fit2$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll2), cores = 2, chain_id = rep(1:4, each = 1000))
loo2 = loo(ll2, r_eff = r_eff, cores = 2)

# Leave one out modelo student
ll3 = fit3$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll3), cores = 2, chain_id = rep(1:4, each = 1000))
loo3 = loo(ll3, r_eff = r_eff, cores = 2)

# Leave one out modelo student
ll4 = fit4$draws(variables = "log_lik",format = "matrix")
r_eff = relative_eff(exp(ll4), cores = 2, chain_id = rep(1:4, each = 1000))
loo4 = loo(ll4, r_eff = r_eff, cores = 2)

# compare LOO-s
comp = loo_compare(loo1,loo2,loo3,loo4)
xtable(print(comp,simplify = FALSE, digits = 2))

# compare LOO-s
comp = loo_compare(loo1,loo2)
xtable(print(comp,simplify = FALSE, digits = 2))




