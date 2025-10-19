library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(splines2)
library(splines)
library(bayesplot)
library(cowplot)


load("~/Documents/GitHub/Modelos_Multinivel/Datos/DatosRegresionV22021.RData")
compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 500)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

# Multinivel Skew-normal Asimetrica con covariables
sm2 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion2021/Regresion_SkewN_ML.stan")
# Modelo lineal 
sm1 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion/Skew_NormalRegresion.stan")

### Matrix de diseño de las covariables
# Transformar y escalar
ECV2021REG$P10D_std <- scale(log1p(ECV2021REG$P10D))

# Crear matriz sin intercepto
matriz_X <- model.matrix(~ 0 + P10D_std + gruviaje + Hotel + Amigos + CasaP, data = ECV2021REG)
#matriz_X <- model.matrix(~ 0 + P10D + gruviaje + Hotel + Amigos + CasaP, data = ECV2021REG)
matriz_X_1 <- model.matrix(~ 0 + P10D_std + gruviaje + Hotel + CasaP, data = ECV2021REG) #Amigos 

## Lista de datos multinivel Skew-Normal (modelo lineal)

dL_log_sc <- list(n = length(LogGTN), J = 1,  group = rep(1, length(LogGTN)), y = LogGTN, K = 0, X = matrix(0, nrow = length(LogGTN), ncol = 0))
dL_log_1  <- list(n = length(LogGTN), J = 6,  group = gl1, y = LogGTN, K = ncol(matriz_X),   X = matriz_X)
dL_log_2  <- list(n = length(LogGTN), J = 5,  group = gl2, y = LogGTN, K = ncol(matriz_X),   X = matriz_X)
dL_log_3  <- list(n = length(LogGTN), J = 22, group = gl3, y = LogGTN, K = ncol(matriz_X),   X = matriz_X)

## fit modelos multinivel skew-nomal
fit2   <- sm2$sample(data = dL_log_sc, chains = 4, parallel_chains = 4, refresh = 500)
fit2.1 <- sm2$sample(data = dL_log_1,  chains = 4, parallel_chains = 4, refresh = 500,adapt_delta = 0.95, max_treedepth = 12)
fit2.2 <- sm2$sample(data = dL_log_2,  chains = 4, parallel_chains = 4, refresh = 500,adapt_delta = 0.95, max_treedepth = 12)
fit2.3 <- sm2$sample(data = dL_log_3,  chains = 4, parallel_chains = 4, refresh = 500,adapt_delta = 0.95, max_treedepth = 12)
#fit2.4 <- sm2$sample(data = dL_log_31, chains = 4, parallel_chains = 4, refresh = 500)

loo_multi<- loo_compare(
  fit2.1$loo(),  # Modelo 2: Modelo : Multinivel Zona -con covariables 
  fit2.2$loo(),  # Modelo 3: Modelo : Multinivel proce -con covariables
  fit2.3$loo()  # Modelo 3: Modelo : Multinivel proce -con covariables
)
print(loo_multi, simplify = FALSE)
xtable(print(loo_multi, simplify = FALSE, digits = 2))

# Extraer cadenas MCMC de los parámetros principales
fv <- fit2.3$draws(variables = c("mu", "mu_group", "alpha", "sigma", "beta", "beta_group"),
                   format = "draws_matrix")

# Resumir las cadenas (media, sd, rhat, ESS, etc.)
resumen_fv <- summarize_draws(fv)
print(resumen_fv, n = 10)
tabla_fv <- xtable(resumen_fv, digits = 2)
print(tabla_fv, include.rownames = FALSE)

loo_fit <- loo(fit2.3$draws("log_lik"))
bad_obs <- which(loo_fit$diagnostics$pareto_k > 0.7)
bad_obs


##Trazas y densdides

fv_mu <- as_draws_df(fit2.3$draws(variables = paste0("mu_group[", 1:22, "]")))
color_scheme_set("blue")
for (i in seq(1, 22, by = 6)) {
  end1 <- min(i + 2, 22)
  end2 <- min(i + 5, 22)
  g1= mcmc_combo(fv_mu[, i:end1], gg_theme = theme(legend.position = "none"))
  g2 = mcmc_combo(fv_mu[, (i + 3):end2])
  print(cowplot::plot_grid(g1, g2, ncol = 2,rel_widths = c(1.1, 1.2)))
  readline(prompt = paste0("Continuar con el bloque ", i, "–", end2, "?"))
}

# modelo multinivel log normal
sple = sample(1:4000,500)
yrep = fit2.3$draws(variables = c("y_rep"),format = "matrix")

niveles_validos <- setdiff(levels(glevels3),1)
n_glevels3<-factor(glevels3, levels = niveles_validos)
niveles<-levels(n_glevels3)
keep1<- n_glevels3 %in% niveles[1:6]

#### PPC1
ppc_dens_overlay_grouped(
  LogGTN[keep1],
  yrep[sple, keep1, drop = FALSE],
  group = droplevels(n_glevels3[keep1])
)+ facet_wrap(~ group, scales = "free")  + labs(title = "Posterior Predictive checks",
                                                subtitle = "Modelo student-t multinivel")

#### PPC2
keep2<- n_glevels3 %in% niveles[7:12]

ppc_dens_overlay_grouped(
  LogGTN[keep2],
  yrep[sple, keep2, drop = FALSE],
  group = droplevels(n_glevels3[keep2])
)+ facet_wrap(~ group, scales = "free")  + labs(title = "Posterior Predictive checks",
                                                subtitle = "Modelo student-t multinivel")

#### PPC3
keep3<- n_glevels3 %in% niveles[13:18]

ppc_dens_overlay_grouped(
  LogGTN[keep3],
  yrep[sple, keep3, drop = FALSE],
  group = droplevels(n_glevels3[keep3])
)+ facet_wrap(~ group, scales = "free")  + labs(title = "Posterior Predictive checks",
                                                subtitle = "Modelo student-t multinivel")
#### PPC4
keep4<- n_glevels3 %in% niveles[19:24]

ppc_dens_overlay_grouped(
  LogGTN[keep4],
  yrep[sple, keep4, drop = FALSE],
  group = droplevels(n_glevels3[keep4])
)+ facet_wrap(~ group, scales = "free")  + labs(title = "Posterior Predictive checks",
                                                subtitle = "Modelo student-t multinivel")
#### PPC5
keep5<- n_glevels3 %in% niveles[25:30]

ppc_dens_overlay_grouped(
  LogGTN[keep5],
  yrep[sple, keep5, drop = FALSE],
  group = droplevels(n_glevels3[keep5])
)+ facet_wrap(~ group, scales = "free")  + labs(title = "Posterior Predictive checks",
                                                subtitle = "Modelo student-t multinivel")







