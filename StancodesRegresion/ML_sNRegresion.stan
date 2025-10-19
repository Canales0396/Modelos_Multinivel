data {
  int<lower=0> n; // Number of observations
  int<lower=1> J; // Number of groups
  array[n] int<lower=1, upper=J> group; // Group assignment for each observation
  vector[n] y; // Observed log-normal data
  int<lower=0> K;  // Number of covariables
  matrix[n,K] X;  // matris de diseño
}
parameters {
  real mu;
  vector[J] mu_group; // Group-level means
  vector<lower=0>[J] alpha;
  real<lower=0> sigma; // Individual-level standard deviation
  vector [K] beta;     // coficientes de regresion
}
model {
  vector[n] mu_pred;  // Predictor del modelo Linealcon covriables
  // priors
  if (K > 0) beta ~ normal(0, 10);      //prior debil sobre los coeficiente de las covariables
  
  mu ~ normal(0, 10);
  mu_group ~ normal(mu, 1);
  sigma ~ student_t(3, 0, 1);
  alpha ~ normal(0, 1);
  
  // Predictictor lineal con las covariables
   mu_pred = mu_group[group];
   
   if (K > 0) mu_pred += X * beta;
  
  //likelihood normal multinivel
  y ~ skew_normal(mu_pred, sigma, alpha[group]);
}
generated quantities{
  vector[n] mu_pred;
  vector[n] y_rep; 
  vector[n] log_lik;
   mu_pred = mu_group[group];
   if (K > 0)
     mu_pred += X * beta;
  
 for(i in 1:n){
    y_rep[i] = skew_normal_rng(mu_pred[i], sigma, alpha[group[i]]);
    log_lik[i] = skew_normal_lpdf(y[i] | mu_pred[i], sigma, alpha[group[i]]);
  }
}
