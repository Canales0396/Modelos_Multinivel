data {
  int<lower=0> n;                           // número de observaciones
  int<lower=1> J;                           // número de grupos
  array[n] int<lower=1, upper=J> group;     // índice de grupo para cada observación
  vector<lower=1e-9>[n] y;                  // gasto turístico positivo
}

parameters {
  real<lower=1e-6> mu;                      // media global > 0
  vector<lower=1e-6>[J] mu_group;           // medias de grupo > 0
  real<lower=1e-6> sigma;                   // margen numérico para divisiones estables
}

transformed parameters {
  // Reparametrización Inv-Gamma (shape, scale) que garantiza:
  //    E[y_ij] = mu_j  y  Var(y_ij) = sigma * mu_j^2
  // Para y ~ Inv-Gamma(alpha, beta):
  //    E[y] = beta / (alpha - 1),  Var[y] = beta^2 / ((alpha-1)^2 * (alpha-2))
  // Elegimos: alpha = 2 + 1/sigma  (constante en j),  beta_j = mu_j * (alpha - 1)

  real alpha_scalar = 2.0 + 1.0 / sigma;          // shape > 2 ⇒ varianza finita
  vector[J] alpha = rep_vector(alpha_scalar, J);   // shape (constante en j)
  vector[J] beta  = mu_group * (alpha_scalar - 1.0); // scale por grupo
}

model {
  // Priors (poco informativas, comparables con Gamma)
  mu ~ normal(0, 10);
  mu_group ~ normal(mu, 10);
  sigma ~ student_t(5, 0, 10);              // Half-t efectiva por la restricción >0

  // Verosimilitud Inverse-Gamma (shape, scale)
  y ~ inv_gamma(alpha[group], beta[group]);
}

generated quantities {
  vector[n] y_rep;                           // réplicas simuladas
  vector[n] log_lik;                         // log-verosimilitud punto a punto (LOO/WAIC)

  for (i in 1:n) {
    y_rep[i]   = inv_gamma_rng(alpha[group[i]], beta[group[i]]);
    log_lik[i] = inv_gamma_lpdf(y[i] | alpha[group[i]], beta[group[i]]);
  }
}

