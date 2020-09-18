// Builds logit regression model based on collected metrics //
// Incorporates priors from 2019 Hottest 100                //

// Author: Trent Henderson, 18 September 2020 //

data {
  
  int<lower=1> N;               // Number of observations
  vector[N] x1;                 // Facebook likes
  vector[N] x2;                 // Spotify plays
  vector[N] x3;                 // Hottest 100 rank
  vector[N] x4;                 // Days since song release
  int<lower=0,upper=1> y[N];    // Classification of artist nationality
  vector[N] alpha;              // Prior for intercept
  vector[N] beta_1;               // Prior for regression coefficient for x1
  vector[N] beta_2;               // Prior for regression coefficient for x2
  vector[N] beta_3;               // Prior for regression coefficient for x3
  vector[N] beta_4;               // Prior for regression coefficient for x4
  
}

parameters {
}

model {
}

generated quantities {
  
  // Specify prediction vector
  
  vector[N] y_hat;
  
  // Posterior predictions for each point
  
  for (n in 1:N) {
    y_hat[n] = inv_logit(alpha + beta_1[1] * x1[n] + beta_2[2] * x2[n] + beta_3[3] * x3[n] + beta_4[4] * x4[n]);
  }
}