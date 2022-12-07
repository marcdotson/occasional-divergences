// Index values, hyperprior values, and covariates.
data {
  int<lower = 1> R;                  // Number of respondents.
  int<lower = 1> S;                  // Number of choice tasks.
  int<lower = 2> A;                  // Number of choice alternatives.
  int<lower = 1> I;                  // Number of observation-level covariates.
  int<lower = 1> J;                  // Number of population-level covariates.

  real Gamma_mean;                   // Mean of population-level means.
  real<lower=0> Gamma_scale;         // Scale of population-level means.
  real<lower=0> Omega_shape;         // Shape of population-level scale.
  real tau_df;                       // Degrees of freedom of population-level scale.

  matrix[A, I] X[R, S];              // Array of observation-level covariates.
  matrix[R, J] Z;                    // Matrix of population-level covariates.
}

// Generate data according to the hierarchical multinomial logit.
generated quantities {
  int<lower = 1, upper = A> Y[R, S]; // Matrix of observations.
  matrix[J, I] Gamma;                // Matrix of population-level hyperparameters.
  corr_matrix[I] Omega;              // Population model correlation matrix hyperparameters.
  vector[I] tau;                     // Population model vector of scale hyperparameters.
  matrix[R, I] Beta;                 // Matrix of observation-level parameters.

  // Draw parameter values and generate data.
  for (j in 1:J) {
    for (i in 1:I) {
      Gamma[j, i] = normal_rng(Gamma_mean, Gamma_scale);
    }
  }
  for (i in 1:I) {
    tau[i] = chi_square_rng(tau_df);
  }
  Omega = lkj_corr_rng(I, Omega_shape);
  for (r in 1:R) {
    Beta[r,] = multi_normal_rng(Z[r,] * Gamma, quad_form_diag(Omega, tau))';
    for (s in 1:S) {
      Y[r, s] = categorical_logit_rng(X[r, s] * Beta[r,]');
    }
  }
}
