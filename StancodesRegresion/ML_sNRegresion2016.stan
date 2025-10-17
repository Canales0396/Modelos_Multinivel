data {
  int<lower=0> n; // Number of observations
  int<lower=1> K; // Number of covariables
  matrix[n,K] X; // matris de diseño
  vector[n] y; // Observed log-normal data
}
parameters {
  vector [K] beta; // coficientes de regresion
  real<lower=0> sigma; // Individual-level standard deviation
  real alpha;
}
model {
  vector[n] mu = X*beta;
  // priors
  beta ~ normal(0, 10);
  sigma ~ student_t(3, 0, 1);
  
  //likelihood
  y ~ skew_normal(mu, sigma, alpha);
}
generated quantities{
  vector [n] mu;
  vector[n] y_rep; 
  vector[n] log_lik;
  mu =X*beta;
  
 for(i in 1:n){
    y_rep[i] = skew_normal_rng(mu[i], sigma, alpha);
    log_lik[i] = skew_normal_lpdf(y[i] | mu[i], sigma, alpha);
  }
}
