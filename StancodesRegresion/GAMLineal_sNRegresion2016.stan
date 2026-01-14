data {
  int<lower=0> n;   // Nunmber Observation
  int<lower=1> J;  // number gruop
  array[n] int<lower=1, upper=J> group; // grupo (zona o procedencia)
  vector[n] y;          // variable respuesta (log gasto)
  int<lower=1> Kb;      // Number de bases spline
  int<lower=1> Kx;     // Number de covariables lineales (incluye intercepto)
  matrix[n, Kb] B;    // Number de bases spline (Noches)
  matrix[n, Kx] W;    // Number de covariables lineales
}
parameters {
  real mu;                // nivel global
  vector[J] mu_group;     // efectos de grupo
  vector<lower=0>[J] alpha;    // asimetría por grupo
  real<lower=0> sigma;        // desviación global
  
  // parámetros del GAM lineal
  vector[Kx] beta;              // efectos fijos (Hotel, Amigos, etc.)
  vector[Kb] b_s;               // coeficientes spline
  real<lower=0> tau_s;          // suavizado spline
}
transformed parameters {
  vector[n] mu_hat;           // media esperada del modelo

  mu_hat = W * beta + B * b_s + mu_group[group];
}
model {
  // prior
  mu ~ normal(0, 10);
  mu_group ~ normal(mu, 1);
  sigma ~ student_t(3, 0, 1);
  alpha ~ normal(0, 1);

  // Priors adicionales para GAM lineal
  beta ~ normal(0, 1);
  b_s ~ normal(0, tau_s);
  tau_s ~ student_t(3, 0, 1);

   //likelihood
  y ~ skew_normal(mu_hat, sigma, alpha[group]);
}
generated quantities {
  vector[n] y_rep;
  vector[n] log_lik;

  for (i in 1:n) {
    y_rep[i] = skew_normal_rng(mu_hat[i], sigma, alpha[group[i]]);
    log_lik[i] = skew_normal_lpdf(y[i] | mu_hat[i], sigma, alpha[group[i]]);
  }
}

