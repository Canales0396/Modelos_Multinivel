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
load("~/Documents/GitHub/Modelos_Multinivel/Datos/DatosRegresion2016.RData")
compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 500)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

# Modelo RegresionLineal Gomez
sm1 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion/ML_sNRegresion2016.stan")
sm2 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion/GAMLineal_sNRegresion2016.stan")
sm2$compile(force_recompile = TRUE)
sm3 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_sN.stan")


# La lista de datos que Stan necesita para hacer mcmc
### Base Spines MATRIZ DE DISEÑO
matriz_X <- model.matrix(~ P10_3NumNoch + gruviaje + Hotel + Amigos + CasaP, data = EGYPV2016REG)
matriz_X_1 <- model.matrix(~ P10_3NumNoch + gruviaje + Hotel + CasaP, data = EGYPV2016REG)
B <- bs(EGYPV2016REG$P10_3NumNoch)
W_lin <- model.matrix(~ gruviaje + Hotel + Amigos + CasaP, data = EGYPV2016REG)
W_lin <- W_lin[,-1]

W_lin1 <- model.matrix(~ gruviaje + Hotel + CasaP, data = EGYPV2016REG)
W_lin1 <- W_lin1[,-1]


##Lista de datos para GAM LINEAL GLOBAL Skew_Normal Gomez
d2_log_1 <- list(n = length(LogGTN), J = 1,group = rep (1L,length(LogGTN)),y=LogGTN,Kb=ncol(B),Kx=ncol(W_lin),B=B,W=W_lin)


### Multinivel Lineal

dL_log_3 <- list(n = length(LogGTN), J = 26, group = gl3, y = LogGTN, K = ncol(W_lin),  X = W_lin)
## GAM No lineal Multinivel

d3_log   <- list(n = length(LogGTN), J = 26, group = gl3, y = LogGTN, Kb = ncol(B),   Kx = ncol(W_lin), B = B, W = W_lin)

# Modelo Multinivel sin covariables
fit1<- sm3$sample(data = dL_log_3, chains = 4, parallel_chains = 4, refresh = 500)

## 2) GAM lineal (global)
fit2.1 <- sm2$sample(data = d2_log_1, chains = 4, parallel_chains = 4, refresh = 500) ### Gomez


## 3) Lineales multinivel (sin spline)
fitL2.3 <- sm1$sample(data = dL_log_3, chains = 4, parallel_chains = 4, refresh = 500) # Zona-Proce

## 4) No lineales multinivel (GAM jerárquicos)

fit2.4 <- sm2$sample(data = d3_log,   chains = 4, parallel_chains = 4, refresh = 500)  # Zona-Proce




## Comparar modelos con buen LOO
loo_final <- loo_compare(
  fit1$loo(),
  fit2.1$loo(),  # Modelo 3: GAM Global (log)- Gomez
  fitL2.3$loo(),  # Modelo 5:  Multinivel (Zona-Procedencia) lineal sin spline 
  fit2.4$loo()   # Modelo 6: GAM Multinivel (Zona-Procedencia) Spline
)

print(loo_final, simplify = FALSE)
xtable(print(loo_final, simplify = FALSE, digits = 2))
fit2.1$summary()
fit2.4$summary()
#################################################################################
##        Análisis de parámetros del modelo de regresión skew-normal lineal
#################################################################################

# Extraer cadenas de los parámetros principales
fv <- fit2.4$draws(variables = c("beta", "sigma", "alpha","mu_group"), format = "matrix")
summarize_draws(fv)
xtable(print(summarize_draws(fv),simplify = FALSE, digits = 2))

summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel GAM
mcmc_combo(fv[,1:10],gg_theme = theme(legend.position = "none"))
mcmc_combo(fv[,11:20])
mcmc_combo(fv[,21:30],gg_theme = theme(legend.position = "none"))
mcmc_combo(fv[,31:40])
mcmc_combo(fv[,41:50],gg_theme = theme(legend.position = "none"))
mcmc_combo(fv[,51:57])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))



###########################################################
# Posterior predictive checks
###########################################################
sple = sample(1:4000,500)
yrep = fit1.1$draws(variables = c("y_rep"),format = "matrix") #Posterior
ppc_dens_overlay(LogGTN, yrep[sple, ])  
ppc_stat(LogGTN, yrep[sple, ],stat="mean") # Comparacion de Medias 
ppc_stat(LogGTN, yrep[sple, ],stat="sd") # Comparacion desviacio estandar
ppc_scatter_avg(LogGTN, yrep[sple, ]) # Residuos




# Extraer cadenas de los parámetros principales
#fv2 <- fit2.4$draws(variables = c("beta", "sigma", "alpha"), format = "matrix")
fv2 <- fit2.4$draws(variables = c("mu", "beta", "b_s", "tau_s", "sigma", "alpha", "mu_group"),format = "matrix")

summarize_draws(fv2)
xtable(print(summarize_draws(fv2),simplify = FALSE, digits = 2))

summarize_draws(fv1)
xtable(print(summarize_draws(fv1),simplify = FALSE, digits = 2))

# graficos de las posteriors multinivel
g1 = mcmc_combo(fv1[,1:4],gg_theme = theme(legend.position = "none"))
g2 = mcmc_combo(fv1[,5:8])
cowplot::plot_grid(g1,g2,ncol = 2,rel_widths = c(1.1, 1.2))


###########################################################
# Posterior predictive checks
###########################################################
sple = sample(1:4000,500)
yrep = fit1.1$draws(variables = c("y_rep"),format = "matrix") #Posterior
ppc_dens_overlay(LogGTN, yrep[sple, ])











ppc_dens_overlay(LogGTN, yrep[sple, ])
ppc_dens_overlay(LogGTN, yrep[sple, ])
## Global
d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)
d1_log = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
## Zona visitada
d2 = list(n = length(GastoTotal), J = 6, group = gl1, y = GastoTotal)
d2_log = list(n = length(LogGTN), J = 6, group = gl1, y = LogGTN)
## Procedencia
d3 = list(n = length(GastoTotal), J = 5, group = gl2, y = GastoTotal)
d3_log = list(n = length(LogGTN), J = 5, group = gl2, y = LogGTN)
## Procedencia y Zona
d4 = list(n = length(GastoTotal), J = 29, group = gl3, y = GastoTotal)
d4_log = list(n = length(LogGTN), J = 29, group = gl3, y = LogGTN)

## datos para la inversa Gamma


log_lik_matrix <- as_draws_matrix(fit2.4$draws("log_lik"))
loo0 <- loo(fit2.4,relloo=TRUE)
loo_fixed <- loo_moment_match(fit2.4, loo0)








