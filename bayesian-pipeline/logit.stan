// Builds logit regression model                    //
// Data is for the 2019 Hottest 100 but outputs     //
// should be used as priors for the                 //
// 2020 Hottest 100.                                //

// Author: Trent Henderson, 18 September 2020 //

data {
  
  int<lower=1> N;               // Number of observations
  vector[N] x1;                 // Facebook likes
  vector[N] x2;                 // Spotify plays
  vector[N] x3;                 // Hottest 100 rank
  vector[N] x4;                 // Days since song release
  int<lower=0,upper=1> y[N];    // Classification of artist nationality
  
}

parameters {
  
  real alpha; // Intercept
  vector[4] beta; // Regression coefficients
  
}

model {
  
  // Specify logit model with all parameters
  
  y ~ bernoulli_logit(alpha + beta[1] * x1 + beta[2] * x2 + beta[3] * x3 + beta[4] * x4);
  
  // Specify distributions for priors - NOTE: THESE WILL BE UPDATED 
  
  alpha ~ normal(0, 1); // Prior for intercept
  beta ~ normal(0, 1); // Prior for regression coefficients
  
}

generated quantities {
  
  // Define calculations for posterior predictions
  
  vector[N] y_hat; // Specify prediction vector
  
  // Posterior predictions for each point
  
  for (n in 1:N) {
    y_hat[n] = inv_logit(alpha + beta[1] * x1[n] + beta[2] * x2[n] + beta[3] * x3[n] + beta[4] * x4[n]);
  }
}