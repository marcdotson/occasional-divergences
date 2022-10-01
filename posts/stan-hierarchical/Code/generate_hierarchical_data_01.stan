// Index and hyperparameter values.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  real mu;                        // Mean of the population model.
  real<lower = 0> tau;            // Variance of the population model.
  real<lower = 0> sigma;          // Variance of the likelihood.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;                    // Vector of observations.
  vector[K] beta;                 // Vector of group intercepts.

  // Draw parameter values and generate data.
  for (k in 1:K) {
    beta[k] = normal_rng(mu, tau);
  }
  for (n in 1:N) {
    y[n] = normal_rng(beta[g[n]], sigma);
  }
}
