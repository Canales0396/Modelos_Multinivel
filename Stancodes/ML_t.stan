data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
}
parameters {
  real mu;
  vector[J] mu_group; // Group-level means
  real<lower=0> sigma; // Individual-level standard deviation
  array[J] real<lower=2> nu_group;
}
model {
  // Prior for group-level parameters using t-student generalizada
  mu ~ normal(0,1);
  mu_group ~ student_t(3, mu, 1);
  // Prior for individual-level parameters
  //sigma ~ student_t(5, 0, 10);
  // CAMBIO 1: prior más informativa para sigma (antes: student_t(5,0,10) muy laxa)
  sigma ~ student_t(4, 0, 1);         // o: sigma ~ normal(0,1);  // half-normal implícita por <0>
  
  // Prior for degrees of freedom
  //nu_group ~ gamma(3, 0.1);
  // CAMBIO 2 (opción simple, misma forma): gamma que empuja ν hacia valores más altos
  nu_group ~ gamma(10, 0.5);          // media ≈ 20, var ≈ 40 (evita ν ≈ 2–4 sin evidencia fuerte)
  
  // Likelihood using t-student generalizada
  for (i in 1:n) {
    y[i] ~ student_t(nu_group[group[i]], mu_group[group[i]], sigma);
  }
}
generated quantities {
  vector[n] y_rep; 
  vector[n] log_lik;
  
  for (i in 1:n) {
    y_rep[i] = student_t_rng(nu_group[group[i]], mu_group[group[i]], sigma);
    log_lik[i] = student_t_lpdf(y[i] | nu_group[group[i]], mu_group[group[i]], sigma);
  }
}
