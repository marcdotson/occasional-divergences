// Index value and observations.
data {
  int<lower = 1> N;    // Number of observations.
  vector[N] y;         // Vector of observations.
}

// Parameters.
parameters {
  real mu;             // Mean of the regression.
  real<lower = 0> tau; // Variance of the regression.
}

// Regression.
model {
  // Priors.
  mu ~ normal(0, 5);
  tau ~ normal(0, 5);

  // Likelihood.
  y ~ normal(mu, tau);
}
