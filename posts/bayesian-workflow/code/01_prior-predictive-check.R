# Load libraries.
library(tidyverse)
library(rstan)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Specify the number of simulated datasets.
R <- 10

# Specify the data values for simulation in a list.
sim_values <- list(
  N = 500,           # Number of observations.
  P = 3,             # Number of product alternatives.
  A = c(3, 4, 5),    # Number of levels in each discrete attribute.
  L = 12             # Number of estimable attribute levels, including the brand intercept.
)
# Experimental design for each observations.
X <- NULL
# X <- array(NA, c(sim_values$N, sim_values$P, sim_values$L))
for (n in 1:sim_values$N) {
  # Discrete predictors.
  X_n <- NULL
  for (a in 1:length(sim_values$A)) {
    X_a <- NULL
    for (p in 1:sim_values$P) {
      X_p <- matrix(0, nrow = 1, ncol = sim_values$A[a])
      X_p[1, sample(seq(1, sim_values$A[a]), 1)] <- 1
      if (a == 1) X_a <- rbind(X_a, X_p)
      if (a != 1) X_a <- rbind(X_a, X_p[, -1])
    }
    X_n <- cbind(X_n, X_a)
  }
  # Continuous predictors.
  L_n <- sim_values$L - (sum(sim_values$A) - length(sim_values$A) + 1)
  if(L_n != 0) {
    X_n <- cbind(X_n, matrix(rnorm(sim_values$P * L_n), ncol = L_n))
  }
  X[[n]] <- X_n
  # X[n,,] <- X_n
}
sim_values$X <- X

# Simulate data.
sim_data <- stan(
  file = here::here("content", "post", "tidy-bayes", "Code", "mnl_simulate.stan"),
  data = sim_values,
  iter = R,
  warmup = 0,
  chains = 1,
  refresh = R,
  seed = 42,
  algorithm = "Fixed_param"
)

saveRDS(sim_values, file = here::here("content", "post", "tidy-bayes", "Data", "sim_values.rds"))
saveRDS(sim_data, file = here::here("content", "post", "tidy-bayes", "Data", "sim_data.rds"))

# Extract simulated data and parameters.
sim_x <- sim_values$X
sim_b <- extract(sim_data)$beta

# Compute the implied choice probabilities.
probs <- NULL
for (r in 1:R) {
  probs_temp <- NULL
  for (n in 1:sim_values$N) {
    exp_xb <- exp(sim_x[[n]] %*% sim_b[r,])
    max_prob <- max(exp_xb / sum(exp_xb))
    probs <- c(probs, max_prob)
  }
  probs <- cbind(probs, probs_temp)
}

# Make sure there aren't dominating alternatives.
tibble(probs) %>%
  ggplot(aes(x = probs)) +
  geom_histogram()

# ggsave(
#   "probs_plot.png",
#   path = here::here("Figures"),
#   width = 6, height = 3, units = "in"
# )

# Possible next steps:
# - Plot the uncertainty in the prior predictive distribution.
# - Utilize tidybayes and bayesplot.

# # Compute the implied choice probabilities.
# probs <- tribble(~ draw, ~ prob)
# for (r in 1:R) {
#   probs_r <- tibble(
#     draw = r,
#     prob = rep(NA, sim_values$N)
#   )
#   for (n in 1:sim_values$N) {
#     exp_xb <- exp(sim_x[r,n,,] %*% sim_b[r,])
#     max_prob <- max(exp_xb / sum(exp_xb))
#     probs_r[n, "prob"] <- max_prob
#   }
#   probs <- bind_rows(probs, probs_r)
# }
#
# # Make sure there aren't dominating alternatives.
# probs %>%
#   ggplot(aes(x = prob)) +
#   geom_histogram() +
#   facet_wrap(~ draw)

# # Count of attribute levels in the chosen alternative.
# library(tidybayes)
# sim_data %>%
#   spread_draws(Y[n], X[n][p, l]) %>%
#   filter(Y == p) %>%
#   group_by(l) %>%
#   summarize(sum_x = sum(X)) %>%
#   ggplot(aes(x = as.factor(l), y = sum_x)) +
#   geom_col() +
#   labs(
#     title = "Count of attribute levels in the chosen alternative",
#     x = "Attribute Levels",
#     y = "Count"
#   ) +
#   coord_flip()
