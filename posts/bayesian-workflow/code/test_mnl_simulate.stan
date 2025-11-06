// Flat multinomial logit for modeling conjoint data derived from the Stan User's Guide:
// https://mc-stan.org/docs/2_23/stan-users-guide/multi-logit-section.html

data {
  int N;
  int P;
  int L;
}
generated quantities {
  int y[N];
  matrix[N, L] x;
  matrix[L, P] beta;
  matrix[N, P] x_beta;
  // int Y[N];          // Vector of observed choices.
  // matrix[P, L] X[N]; // Experimental design for each observations.
  // vector[L] beta;    // Vector of aggregate beta coefficients.
  // vector[P] x_beta[N];

  for (l in 1:L) {
    for (p in 1:P) {
      beta[l, p] = normal_rng(0, 5);
    }
    // beta[l] = normal_rng(0, 5);
  }
  for (n in 1:N) {
    for (l in 1:L) {
      x[n, l] = binomial_rng(1, 0.5);
    }
    // for (p in 1:P) {
    //   for (l in 1:L) {
    //     X[n][p, l] = binomial_rng(1, 0.5);
    //   }
    // }
    // x_beta[n] = X[n] * beta;
    // Y[n] = categorical_logit_rng(x_beta[n]);
  }
  x_beta = x * beta;
  for (n in 1:N) {
    y[n] = categorical_logit_rng(x_beta[n]');
  }
}
