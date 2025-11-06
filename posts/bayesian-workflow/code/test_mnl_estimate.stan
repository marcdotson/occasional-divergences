// Flat multinomial logit for modeling conjoint data from the Stan User's Guide:
// https://mc-stan.org/docs/2_23/stan-users-guide/multi-logit-section.html

data {
  int N;
  int P;
  int L;
  // int y[N];
  // matrix[N, L] x;
  int Y[N];          // Vector of observed choices.
  matrix[P, L] X[N]; // Array of experimental designs per observations.
}
parameters {
  // matrix[L, P] beta;
  vector[L] beta;    // Vector of aggregate beta coefficients.
}
model {
  // matrix[N, P] x_beta = x * beta;

  // to_vector(beta) ~ normal(0, 5);

  for (n in 1:N)
    // y[n] ~ categorical_logit(x_beta[n]');
    Y[n] ~ categorical_logit(X[n] * beta);
}
