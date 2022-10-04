// Index values, observations, and covariates.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.
  int<lower = 1> J;               // Number of population-level covariates.

  vector[N] y;                    // Vector of observations.
  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  matrix[K, J] Z;                 // Matrix of population-level covariates.
}

// Parameters and hyperparameters.
parameters {
  matrix[J, I] Gamma;             // Matrix of population-level coefficients.
  corr_matrix[I] Omega;           // Population model correlation matrix.
  vector<lower = 0>[I] tau;       // Population model vector of scale parameters.
  matrix[K, I] Delta;             // Matrix of non-centered observation-level coefficients.
  real<lower = 0> sigma;          // Variance of the likelihood.
}

// Deterministic transformation.
transformed parameters {
  // Matrix of centered observation-level coefficients.
  matrix[K, I] Beta;

  // Non-centered parameterization.
  for (k in 1:K) {
    Beta[k,] = Z[k,] * Gamma + Delta[k,] * quad_form_diag(Omega, tau);
  }
}

// Hierarchical regression.
model {
  // Hyperpriors.
  for (j in 1:J) {
    Gamma[j,] ~ normal(0, 5);
  }
  Omega ~ lkj_corr(2);
  tau ~ normal(0, 5);

  // Prior.
  sigma ~ normal(0, 5);

  // Non-centered population model and likelihood.
  for (k in 1:K) {
    Delta[k,] ~ normal(0, 1);
  }
  for (n in 1:N) {
    y[n] ~ normal(X[n,] * Beta[g[n],]', sigma);
  }
}

// Quantities conditioned on parameter draws.
generated quantities {
  // Log likelihood to estimate loo.
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n,] * Beta[g[n],]', sigma);
  }
}
