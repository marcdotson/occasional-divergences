// Index value and observations.
data {
  int<lower = 1> N;    // Number of observations.
  int<lower = 1> I;    // Number of covariates.
  vector[N] y;         // Vector of observations.
  matrix[N, I] X;      // Matrix of covariates.
}

// Parameters.
parameters {
  vector[I] beta;      // Vector of slopes.
  real<lower = 0> tau; // Variance of the regression.
}

// Regression.
model {
  // Priors.
  for (i in 1:I) {
    beta[i] ~ normal(0, 5);
  }
  tau ~ normal(0, 5);

  // Likelihood.
  for (n in 1:N) {
    y[n] ~ normal(X[n,] * beta, tau);
  }
}
