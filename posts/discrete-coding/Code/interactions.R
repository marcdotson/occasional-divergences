# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidybayes)

# Set the simulation seed.
set.seed(42)

# Generate Data -----------------------------------------------------------
# Specify data and parameter values.
sim_values <- list(
  N = 50,                                       # Number of observations.
  I = 11,                                       # Number of covariates (including interactions).
  J = c(2, 3),                                  # Number of levels for each discrete variable.
  beta = c(1, -4, 6, 3, -2, -1, 1, 2, 0, 2, 4), # Vector of slopes (including interactions).
  tau = 1                                       # Variance of the regression.
)

# Matrix of covariates.
sim_X <- matrix(data = 0, nrow = sim_values$N, ncol = (sim_values$I))
for (n in 1:sim_values$N) {
  temp_X <- NULL
  for (j in 1:length(sim_values$J)) {
    temp_J <- rep(0, sim_values$J[j])
    temp_J[sample(seq(1, (sim_values$J[j])), 1)] <- 1
    temp_X[[j]] <- temp_J
  }
  for (l in 1:sim_values$J[1]) {
    temp_X[[length(sim_values$J) + l]] <- temp_X[[1]][l] * temp_X[[2]]
  }
  sim_X[n,] <- unlist(temp_X)
}
sim_values$X <- sim_X

# Compile the model for generating data.
generate_flat_data <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "generate_flat_data.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Generate data.
sim_data <- generate_flat_data$sample(
  data = sim_values,
  chains = 1,
  iter_sampling = 1,
  seed = 42,
  fixed_param = TRUE
)

# Extract generated data.
sim_y <- sim_data$draws(variables = "y", format = "draws_list") %>%
  pluck(1) %>%
  flatten_dbl()

# Full Model --------------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  I = ncol(sim_X),     # Number of covariates.
  y = sim_y,           # Vector of observations.
  X = sim_X            # Matrix of covariates.
)

# Compile the model.
flat_regression_index <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_index.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_index <- flat_regression_index$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:4),
  values = c(
    sim_values$beta[8] - sim_values$beta[6],
    sim_values$beta[7] - sim_values$beta[6],
    sim_values$beta[11] - sim_values$beta[9],
    sim_values$beta[10] - sim_values$beta[9]
  )
)
fit_index$draws(variables = c("beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[8]` - `beta[6]`,
    contrast2 = `beta[7]` - `beta[6]`,
    contrast3 = `beta[11]` - `beta[9]`,
    contrast4 = `beta[10]` - `beta[9]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3, contrast4) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 2)

# Dropped Main Effects ----------------------------------------------------
# DROP FIRST MAIN EFFECT
# Specify data.
data <- list(
  N = length(sim_y),     # Number of observations.
  I = ncol(sim_X[,3:5]), # Number of covariates.
  y = sim_y,             # Vector of observations.
  X = sim_X[,3:5]        # Matrix of covariates.
)

# Compile the model.
flat_regression_index <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_index.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_index <- flat_regression_index$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:2),
  values = c(
    sim_values$beta[8] - sim_values$beta[6],
    sim_values$beta[7] - sim_values$beta[6],
    sim_values$beta[11] - sim_values$beta[9],
    sim_values$beta[10] - sim_values$beta[9]
  )
)
fit_index$draws(variables = c("beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[3]` - `beta[1]`,
    contrast2 = `beta[2]` - `beta[1]`
  ) %>%
  gather_draws(contrast1, contrast2) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# DROP SECOND MAIN EFFECT
# Specify data.
data <- list(
  N = length(sim_y),     # Number of observations.
  I = ncol(sim_X[,1:2]), # Number of covariates.
  y = sim_y,             # Vector of observations.
  X = sim_X[,1:2]        # Matrix of covariates.
)

# Compile the model.
flat_regression_index <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_index.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_index <- flat_regression_index$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1),
  values = c(
    sim_values$beta[11] - sim_values$beta[8],
    sim_values$beta[10] - sim_values$beta[7],
    sim_values$beta[9] - sim_values$beta[6]
  )
)
fit_index$draws(variables = c("beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[2]` - `beta[1]`
  ) %>%
  gather_draws(contrast1) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

