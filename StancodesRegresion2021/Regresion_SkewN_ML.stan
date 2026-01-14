data {
  int<lower=0> n;                       // Número de observaciones
  int<lower=1> J;                       // Número de grupos (zona-procedencia)
  array[n] int<lower=1, upper=J> group; // Índice del grupo
  vector[n] y;                          // Log del gasto
  int<lower=0> K;                       // Número de covariables
  matrix[n, K] X;                       // Matriz de covariables
}

parameters {
  // ----- Nivel global -----
  real mu;                              // Intercepto global
  vector[K] beta;                       // Coeficientes globales
  real<lower=0> sigma;                  // Desviación global
  real alpha;                           // Asimetría global

  // ----- Nivel de grupo -----
  vector[J] mu_group;                   // Interceptos por grupo
  matrix[J, K] beta_group;              // Coeficientes por grupo
  vector[J] alpha_group;                // Asimetría por grupo
}
transformed parameters{ 
  vector[n] mu_ij;
  for(i in 1:n){
      mu_ij[i] = mu_group[group[i]];
      if(K>0) mu_ij[i]+= X[i] * beta_group[group[i]]';
  }
}
model {
  // priors
  //for (j in 1:J)
  if(K>0){
    beta ~ normal(0, 10);
    for (j in 1:J) beta_group[j] ~ normal(beta, 1);   
  }
  alpha_group ~ normal(alpha, 1);       
  mu ~ normal(0, 10);
  mu_group ~ normal(mu, 1);   
  sigma ~ student_t(3, 0, 1);
  alpha ~ normal(0, 1);

  //likelihood
  for (i in 1:n) {
    y[i] ~ skew_normal(mu_ij[i], sigma, alpha_group[group[i]]);
  }
}
generated quantities {
  vector[n] y_rep;
  vector[n] log_lik;

  for (i in 1:n) {
    y_rep[i] = skew_normal_rng(mu_ij[i], sigma, alpha_group[group[i]]); // Predicción posterior
    log_lik[i] = skew_normal_lpdf(y[i] | mu_ij[i], sigma, alpha_group[group[i]]); // Log-verosimilitud individual
  }
}

