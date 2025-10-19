library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/GitHub/Modelos_Multinivel/Datos/Datos2021.RData")

compute_loo <- function(stan_file_path = NULL, data_list = NULL){
  sm <- cmdstan_model(stan_file_path)
  fit <- sm$sample(data = data_list, chains = 4, parallel_chains = 4, refresh = 500)
  ll = fit$draws(variables = "log_lik", format = "matrix")
  r_eff = relative_eff(exp(ll), cores = 2, chain_id = rep(1:4, each = 1000))
  loo_results = loo(ll, r_eff = r_eff, cores = 2)
  return(loo_results)
}

#----------------------------------------------------------
# Modelos Multinivel Bayesianos - Todas las Distribuciones
# Escalas: Real (todas) y Logarítmica (solo aplicables)
#----------------------------------------------------------

# Compilación de modelos
sm1 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_gamma.stan")
sm2 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_gG.stan")
sm3 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_invgamma.stan")
sm4 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_N.stan")
sm5 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_sN.stan")
sm6 <- cmdstan_model("~/Documents/GitHub/Modelos_Multinivel/Stancodes/ML_t.stan")

#----------------------------------------------------------
# Listas de datos (Real y Logarítmica)
#----------------------------------------------------------

d1 = list(n = length(GastoTotal), J = 1, group = rep(1, length(GastoTotal)), y = GastoTotal)
d1_log = list(n = length(LogGTN), J = 1, group = rep(1, length(LogGTN)), y = LogGTN)
d2 = list(n = length(GastoTotal), J = 6, group = gl1, y = GastoTotal)
d2_log = list(n = length(LogGTN), J = 6, group = gl1, y = LogGTN)
d3 = list(n = length(GastoTotal), J = 5, group = gl2, y = GastoTotal)
d3_log = list(n = length(LogGTN), J = 5, group = gl2, y = LogGTN)
d4 = list(n = length(GastoTotal), J = 29, group = gl3, y = GastoTotal)
d4_log = list(n = length(LogGTN), J = 29, group = gl3, y = LogGTN)

#----------------------------------------------------------
# Ajustes de los modelos - Escala Real
#----------------------------------------------------------

fit1 <- sm1$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Global
fit1.1 <- sm1$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Zona
fit1.2 <- sm1$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Procedencia
fit1.3 <- sm1$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Zona-Procedencia

fit2 <- sm2$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Generalizada Global
fit2.1 <- sm2$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Generalizada Zona
fit2.2 <- sm2$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Generalizada Procedencia
fit2.3 <- sm2$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Gamma Generalizada Zona-Procedencia

fit3 <- sm3$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Inversa Gamma Global
fit3.1 <- sm3$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Inversa Gamma Zona
fit3.2 <- sm3$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Inversa Gamma Procedencia
fit3.3 <- sm3$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Inversa Gamma Zona-Procedencia

fit4 <- sm4$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Normal Global
fit4.1 <- sm4$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Normal Zona
fit4.2 <- sm4$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Normal Procedencia
fit4.3 <- sm4$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Normal Zona-Procedencia

fit5.1 <- sm5$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Skew Normal Global
fit5.2 <- sm5$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Skew Normal Zona
fit5.3 <- sm5$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Skew Normal Procedencia
fit5.4 <- sm5$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Skew Normal Zona-Procedencia

fit6.1 <- sm6$sample(data = d1, chains = 4, parallel_chains = 4, refresh = 500) # Real - Student-t Global
fit6.2 <- sm6$sample(data = d2, chains = 4, parallel_chains = 4, refresh = 500) # Real - Student-t Zona
fit6.3 <- sm6$sample(data = d3, chains = 4, parallel_chains = 4, refresh = 500) # Real - Student-t Procedencia
fit6.4 <- sm6$sample(data = d4, chains = 4, parallel_chains = 4, refresh = 500) # Real - Student-t Zona-Procedencia

#----------------------------------------------------------
# Ajustes de los modelos - Escala Logarítmica
# (Solo Normal, Skew-Normal y Student-t)
#----------------------------------------------------------

fit4_log <- sm4$sample(data = d1_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Normal Global
fit4.1_log <- sm4$sample(data = d2_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Normal Zona
fit4.2_log <- sm4$sample(data = d3_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Normal Procedencia
fit4.3_log <- sm4$sample(data = d4_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Normal Zona-Procedencia

fit5_log <- sm5$sample(data = d1_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Skew Normal Global
fit5.1_log <- sm5$sample(data = d2_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Skew Normal Zona
fit5.2_log <- sm5$sample(data = d3_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Skew Normal Procedencia
fit5.3_log <- sm5$sample(data = d4_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Skew Normal Zona-Procedencia

fit6_log <- sm6$sample(data = d1_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Student-t Global
fit6.1_log <- sm6$sample(data = d2_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Student-t Zona
fit6.2_log <- sm6$sample(data = d3_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Student-t Procedencia
fit6.3_log <- sm6$sample(data = d4_log, chains = 4, parallel_chains = 4, refresh = 500) # Log - Student-t Zona-Procedencia

## Comparar modelos con buen LOO
loo_final <- loo_compare(
  fit1$loo(),     # model1: Gamma, Global, escala Real
  fit1.1$loo(),   # model2: Gamma, Zona, escala Real
  fit1.2$loo(),   # model3: Gamma, Procedencia, escala Real
  fit1.3$loo(),   # model4: Gamma, Zona_Procedencia, escala Real
  fit2$loo(),     # model5: Gamma Generalizada, Global, escala Real
  fit2.1$loo(),   # model6: Gamma Generalizada, Zona, escala Real
  fit2.2$loo(),   # model7: Gamma Generalizada, Procedencia, escala Real
  fit2.3$loo(),   # model8: Gamma Generalizada, Zona_Procedencia, escala Real
  fit3$loo(),     # model9: Inversa Gamma, Global, escala Real
  fit3.1$loo(),   # model10: Inversa Gamma, Zona, escala Real
  fit3.2$loo(),   # model11: Inversa Gamma, Procedencia, escala Real
  fit3.3$loo(),   # model12: Inversa Gamma, Zona_Procedencia, escala Real
  fit4$loo(),     # model13: Normal, Global, escala Real
  fit4.1$loo(),   # model14: Normal, Zona, escala Real
  fit4.2$loo(),   # model15: Normal, Procedencia, escala Real
  fit4.3$loo(),   # model16: Normal, Zona_Procedencia, escala Real
  fit5.1$loo(),   # model17: Skew-Normal, Global, escala Real
  fit5.2$loo(),   # model18: Skew-Normal, Zona, escala Real
  fit5.3$loo(),   # model19: Skew-Normal, Procedencia, escala Real
  fit5.4$loo(),   # model20: Skew-Normal, Zona_Procedencia, escala Real
  fit6.1$loo(),   # model21: Student-t, Global, escala Real
  fit6.2$loo(),   # model22: Student-t, Zona, escala Real
  fit6.3$loo(),   # model23: Student-t, Procedencia, escala Real
  fit6.4$loo(),   # model24: Student-t, Zona_Procedencia, escala Real
  fit4_log$loo(),   # model25: Normal, Global, escala Log
  fit4.1_log$loo(), # model26: Normal, Zona, escala Log
  fit4.2_log$loo(), # model27: Normal, Procedencia, escala Log
  fit4.3_log$loo(), # model28: Normal, Zona_Procedencia, escala Log
  fit5_log$loo(),   # model29: Skew-Normal, Global, escala Log
  fit5.1_log$loo(), # model30: Skew-Normal, Zona, escala Log
  fit5.2_log$loo(), # model31: Skew-Normal, Procedencia, escala Log
  fit5.3_log$loo(), # model32: Skew-Normal, Zona_Procedencia, escala Log
  fit6_log$loo(),   # model33: Student-t, Global, escala Log
  fit6.1_log$loo(), # model34: Student-t, Zona, escala Log
  fit6.2_log$loo(), # model35: Student-t, Procedencia, escala Log
  fit6.3_log$loo()  # model36: Student-t, Zona_Procedencia, escala Log
)
print(loo_final, simplify = FALSE)
xtable(print(loo_final, simplify = FALSE, digits = 2))


