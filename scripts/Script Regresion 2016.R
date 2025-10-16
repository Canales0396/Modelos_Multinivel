library(brms)
library(cmdstanr)
library(bayesplot)
library(loo)
library(posterior)
library(xtable)
library(ggplot2)
library(cowplot)
library(ggthemes)

load("~/Documents/GitHub/Modelos_Multinivel/Datos/DatosRegresion2016.RData")
####### Modelos de regresion multinivel

### Modelo Global, propone Gomez con covariables Numero de Noches, GtupViaje, Alojamientio
## log(y_i)=
fitglobal<- brm(
        bf(log(GastoFin) ~ s(P10_3NumNoch)+gruviaje+ Hotel + Amigos + CasaP,
            alpha~1),
           data = EGYPV2016REG,
           family = skew_normal(),
           prior  = c(
             prior(normal(0, 10), class = "Intercept"),   # mu ~ N(0,10)
             prior(student_t(3, 0, 1), class = "sigma"),  # sigma ~ t(3,0,1)
             prior(normal(0, 1), class = "Intercept", dpar = "alpha")  # alpha ~ N(0,1))
             ),
           chains = 4, iter = 4000, warmup = 1000,
)


summary(fitglobal)
loo(fitglobal)
mcmc_trace(fitglobal)
mcmc_dens(fitglobal)
## Gomez

fitGlineal<- brm(
  bf(log(GastoFin) ~ P10_3NumNoch + gruviaje + Hotel + Amigos + CasaP,
     alpha ~ 1),
  data = EGYPV2016REG,
  family = skew_normal(),
  prior  = c(
    prior(normal(0, 10), class = "Intercept"),   # mu ~ N(0,10)
   prior(normal(0, 5), class = "b"),   # mu ~ N(0,5)
    prior(student_t(3, 0, 1), class = "sigma"),  # sigma ~ t(3,0,1)
    prior(normal(0, 1), class = "Intercept", dpar = "alpha")  # alpha ~ N(0,1))
  ),
  chains = 4, iter = 4000, warmup = 1000,
)


summary(fitGlineal)

mcmc_trace(fitGlineal)
mcmc_dens(fitGlineal)
##lineal multinivel
fitMLinealGAM2<- brm(
  bf(log(GastoFin) ~ s(P10_3NumNoch) + gruviaje + Hotel + Amigos + CasaP + (1|Procedencia),
     alpha ~ 1),
  data = EGYPV2016REG,
  family = skew_normal(),
  prior  = c(
    prior(normal(0, 10), class = "Intercept"),   # mu ~ N(0,10)
    prior(normal(0, 10), class = "b"),   # mu ~ N(0,5)
    prior(student_t(3, 0, 1), class = "sigma"),  # sigma ~ t(3,0,1)
    prior(normal(0, 1), class = "Intercept", dpar = "alpha"),  # alpha ~ N(0,1))
    prior(student_t(3, 0, 1), class = "sd",group = "Procedencia")  # tau ~ t(3,0,1)
  ),
  chains = 4, iter = 2000, warmup = 500,
)


summary(fitMLinealGAM2)
mcmc_trace(fitMLinealGAM2)
mcmc_dens(fitMLinealGAM2)
loo_compare(loo(fitglobal),loo(fitGlineal),loo(fitMLineal),loo(fitMLinealGAM),loo(fitMLinealGAM2))


