data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
  //vector<lower=1e-3>[n] y; // y>0 (Gamma)
}
parameters {
  real mu;
  vector[J] mu_group;
  real<lower=0> sigma;
}
// Para garantizar solo tener numeros positivos 
//parameters {
  //real<lower=1e-3> mu;
  //vector [J] mu_group;
  //real<lower=1e-3> sigma;
//}
transformed parameters{
  vector[J] alpha = mu_group .* mu_group / sigma;
  vector[J] beta = mu_group / sigma;
}
model {
  // priors
  mu ~ normal(0,10);
  // Prior for group-level parameters
  mu_group ~ normal(mu, 10);
  
  // Prior for individual-level parameters
  sigma ~ student_t(5, 0, 10);
  
  //likelihood
  y ~ gamma(alpha[group], beta[group]);
}
generated quantities{
  vector[n] y_rep;
  vector[n] log_lik;
  
 for(i in 1:n){
    y_rep[i] = gamma_rng(alpha[group[i]], beta[group[i]]);
    log_lik[i] = gamma_lpdf(y[i] | alpha[group[i]], beta[group[i]]);
  }
}
