data {
  int<lower=0> n;  // Number of observations
  int<lower=0> K;  // Number of covariables
  matrix[n,K] X;  // matris de diseño
  vector[n] y;  // Observed log-normal data
}
parameters {
  vector [K] beta;     // coficientes de regresion
  real<lower=0> sigma; // Individual-level standard deviation
  real alpha;          // parametro de asimetria
}
model {
  vector[n] mu;  // Predictor del modelo Lineal
  
  if (K > 0)
    mu = X * beta;   // modelo con covariables
  else
    mu = rep_vector(0, n); // modelo nulo sin covariables
  
  // priors
  if (K > 0) beta ~ normal(0, 10);      //prior debil sobre los coeficiente de las covariables
  
  sigma ~ student_t(3, 0, 1); //prior sobre la desviacion.
  alpha ~ normal(0,1);   // Prior sobre la simetria 
  
  
  //likelihood
  y ~ skew_normal(mu, sigma, alpha);
}
generated quantities{
  vector[n] mu;
  vector[n] y_rep; 
  vector[n] log_lik;
  
  if (K > 0) 
    mu = X * beta;  
  else
    mu = rep_vector(0, n); 
  
 for(i in 1:n){
    y_rep[i] = skew_normal_rng(mu[i], sigma, alpha);
    log_lik[i] = skew_normal_lpdf(y[i] | mu[i], sigma, alpha);
  }
}
