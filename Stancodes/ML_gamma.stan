data {
  int<lower=0> n; // Número de observaciones
  int<lower=1> J; // Número de grupos (zona, procedencia, etc.)
  array[n] int<lower=1, upper=J> group; // Asignación de grupo por observación
  vector<lower=1e-3>[n] y; // Gasto transformado (positivo), recomendado mínimo > 0
}

parameters {
  real<lower=1e-3> mu; // Media global, positiva
  vector<lower=1e-3>[J] mu_group; // CORREGIDO: Se asegura que los valores sean positivos
  real<lower=1e-3> sigma; // Desviación, positiva
}

transformed parameters {
  // Parametrización de Gamma: alpha y beta
  vector[J] alpha = mu_group .* mu_group / sigma;
  vector[J] beta = mu_group / sigma;
  //  Se eliminó fmax(): ya no es necesario porque mu_group es positivo por restricción
}

model {
  // Priors no informativos o débiles
  mu ~ normal(0,10);
  mu_group ~ normal(mu, 10); // Priori jerárquica centrada en mu
  sigma ~ student_t(5, 0, 10);

  // Verosimilitud con distribución Gamma por grupo
  y ~ gamma(alpha[group], beta[group]);
}

generated quantities {
  vector[n] y_rep;     // Simulación posterior
  vector[n] log_lik;   // Log-verosimilitud para LOO/WAIC

  for (i in 1:n) {
    y_rep[i] = gamma_rng(alpha[group[i]], beta[group[i]]);
    log_lik[i] = gamma_lpdf(y[i] | alpha[group[i]], beta[group[i]]);
  }
}
