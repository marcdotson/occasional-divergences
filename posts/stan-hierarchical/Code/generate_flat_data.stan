// Index and parameter values.
data {
  int<lower = 1> N;    // Number of observations.
  real mu;             // Mean of the regression.
  real<lower = 0> tau; // Variance of the regression.
}

// Generate data according to the simple regression.
generated quantities {
  // Vector of observations.
  vector[N] y;

  // Generate data.
  for (n in 1:N) {
    y[n] = normal_rng(mu, tau);
  }
}
