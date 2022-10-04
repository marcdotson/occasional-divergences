// Index values, covariates, and hyperparameter values.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  int<lower = 1> I;               // Number of observation-level covariates.
  int<lower = 1> J;               // Number of population-level covariates.

  int<lower = 1, upper = K> g[N]; // Vector of group assignments.
  matrix[N, I] X;                 // Matrix of observation-level covariates.
  matrix[K, J] Z;                 // Matrix of population-level covariates.
}

// Generate data according to the hierarchical regression.
generated quantities {
  vector[N] y;                    // Vector of observations.
  matrix[J, I] Gamma;             // Matrix of population-level coefficients.
  corr_matrix[I] Omega;           // Population model correlation matrix.
  vector[I] tau;                  // Population model vector of scale parameters.
  matrix[K, I] Beta;              // Matrix of group-level coefficients.
  real<lower = 0> sigma;          // Variance of the likelihood.

  // Draw parameter values and generate data.
  for (j in 1:J) {
    for (i in 1:I) {
      Gamma[j, i] = normal_rng(0, 5);
    }
  }
  Omega = lkj_corr_rng(I, 2);
  for (i in 1:I) {
    tau[i] = chi_square_rng(2);
  }
  for (k in 1:K) {
    Beta[k,] = multi_normal_rng(Z[k,] * Gamma, quad_form_diag(Omega, tau))';
  }
  sigma = normal_rng(0, 5);
  for (n in 1:N) {
    y[n] = normal_rng(X[n,] * Beta[g[n],]', sigma);
  }
}
