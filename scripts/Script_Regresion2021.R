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
load("~/Documents/GitHub/Modelos_Multinivel/Datos/DatosRegresion2021.RData")
compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, 
                   parallel_chains = 4, refresh = 500)
  
  ll = fit$draws(variables = "log_lik",format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  
  return(loo_results)
}

################ # Modelo lineal para Nulo y Covariables
 #log(y_i)=\beta_0+\beta_1*NumNoches+ \beta_2*Hotel+ \beta_3*Amigos+\beta_4*CasaP+e_i, e_i ~SKew-N(mu, sigma, alpha)
##################
sm1 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion/Skew_NormalRegresion.stan")
sm2 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/StancodesRegresion/ML_sNRegresion.stan")

### Matrix de diseño del modelo lineal
matriz_X <- model.matrix(~ P10D + gruviaje + Hotel + Amigos + CasaP, data = ECV2021REG)
matriz_X_1 <- model.matrix(~ P10D + gruviaje + Hotel + Amigos + CasaP, data = ECV2021REG)

#Lista de datos para los modelos lineales Skew_Normal
d1_log <- list(n = nrow(matriz_X), K = ncol(matriz_X) , X = matriz_X, y = LogGTN) # con amigos
d1_log_1 <- list(n = nrow(matriz_X_1),K = ncol(matriz_X_1) , X = matriz_X_1, y = LogGTN) # sin amigos
d1_log_2 <- list(n = nrow(matriz_X),K = 0, X = matrix(0, nrow(matriz_X), 0), y = LogGTN) # Nulo Global
d1_log_2=list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)

## Lista datos multinivel skew-nomal

dL_log_sc <- list(n = nrow(matriz_X), J = 29, group = gl3, y = LogGTN, K = 0, X = array(dim = c(nrow(matriz_X),0)) );
dL_log_1  <- list(n = nrow(matriz_X), J = 6,  group = gl1, y = LogGTN, K = ncol(matriz_X), X = matriz_X);
dL_log_2  <- list(n = nrow(matriz_X), J = 5,  group = gl2, y = LogGTN, K = ncol(matriz_X), X = matriz_X);
dL_log_3  <- list(n = nrow(matriz_X), J = 29, group = gl3, y = LogGTN, K = ncol(matriz_X), X = matriz_X);




## 1) Lineales globales
fit1   <- sm1$sample(data = d1_log,     chains = 4, parallel_chains = 4, refresh = 500)  # mod lineal global(log) con covariables 
fit1.1 <- sm1$sample(data = d1_log_1, chains = 4, parallel_chains = 4, refresh = 500)  # mod lineal (log) con covariables - amigos
fit1.1.1 <- sm1$sample(data = d1_log_2, chains = 4, parallel_chains = 4, refresh = 500)  # mod lineal global(log) sin covariables

loo_Lineal<- loo_compare(
  fit1$loo(),    # Modelo 1: Lineal Global (log) con covariables 
  fit1.1$loo(),  # Modelo 2: Lineal Globalcon covariables - amigos
  fit1.1.1$loo()  # Modelo 3: lineal global(log) sin covariables
)
print(loo_Lineal, simplify = FALSE)
xtable(print(loo_final, simplify = FALSE, digits = 2))


## 1) Modelos Multinivel Lineales globales
fit2   <- sm2$sample(data = dL_log_sc, chains = 4, parallel_chains = 4, refresh = 500)  # multinivel sin covariables (solo intercepto por grupo)
fit2.1 <- sm2$sample(data = dL_log_1,  chains = 4, parallel_chains = 4, refresh = 500)  # multinivel por zona (con covariables)
fit2.2 <- sm2$sample(data = dL_log_2,  chains = 4, parallel_chains = 4, refresh = 500)  # multinivel por procedencia (con covariables)
fit2.3 <- sm2$sample(data = dL_log_3,  chains = 4, parallel_chains = 4, refresh = 500)  # mod zon proc 

loo_multi<- loo_compare(
  fit2$loo(),    # Modelo 1: MultiLineal Global 
  fit2.1$loo(),  # Modelo 2: Modelo : Multinivel Zona -con covariables 
  fit2.2$loo(),  # Modelo 3: Modelo : Multinivel proce -con covariables
  fit2.3$loo()  # Modelo 3: Modelo : Multinivel proce -con covariables
)
print(loo_multi, simplify = FALSE)
xtable(print(loo_multi, simplify = FALSE, digits = 2))


