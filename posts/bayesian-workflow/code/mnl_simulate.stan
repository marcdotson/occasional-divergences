// Number of observations, choices, etc. to simulate.
data {
  int N;             // Number of observations.
  int P;             // Number of product alternatives.
  int L;             // Number of estimable attribute levels.

  matrix[P, L] X[N]; // Experimental design for each observation.
}

// Simulate data according to the multinomial logit model.
generated quantities {
  int Y[N];          // Vector of observed choices.
  vector[L] beta;    // Vector of aggregate beta coefficients.

  // Draw parameter values from the prior.
  for (l in 1:L) {
    beta[l] = normal_rng(0, 1);
  }

  // Generate an experimental design and draw data from the likelihood.
  for (n in 1:N) {
    Y[n] = categorical_logit_rng(X[n] * beta);
  }
}
