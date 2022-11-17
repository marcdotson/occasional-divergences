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
  N = 50,                    # Number of observations.
  I = 5,                     # Number of covariates.
  J = c(2, 3),               # Number of levels for each discrete variable.
  beta = c(1, -4, 6, 3, -2), # Vector of slopes.
  tau = 1                    # Variance of the regression.
)

# Matrix of covariates.
sim_X <- matrix(data = 0, nrow = sim_values$N, ncol = (sim_values$I))
for (n in 1:sim_values$N) {
  temp_X <- NULL
  for (j in 1:length(sim_values$J)) {
    temp_J <- rep(0, sim_values$J[j])
    temp_J[sample(seq(1, (sim_values$J[j])), 1)] <- 1
    temp_X <- c(temp_X, temp_J)
  }
  sim_X[n,] <- temp_X
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

# Dummy Coding ------------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),    # Number of observations.
  I = ncol(sim_X) - 2,  # Number of covariates.
  y = sim_y,            # Vector of observations.
  X = sim_X[, -c(1, 3)] # Matrix of covariates.
)

# Compile the model.
flat_regression_dummy <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_dummy.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_dummy <- flat_regression_dummy$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Check diagnostics.
fit_dummy$cmdstan_diagnose()

# Check on trace plots for index coding with two discrete variables.
mcmc_trace(fit_dummy$draws(variables = c("alpha", "beta")))

# Try fitting with $optimize().
test_dummy <- flat_regression_dummy$optimize(
  data = data,
  seed = 42
)
test_dummy$summary()

# Check on parameter recovery for index coding with two discrete variables.
parameter_values <- tibble(
  .variable = str_c("parameter", 1:(sim_values$I - length(sim_values$J))),
  values = c(
    # First discrete variable.
    sim_values$beta[2],
    # Second discrete variable.
    sim_values$beta[4], sim_values$beta[5]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    parameter1 = `beta[1]`,
    parameter2 = `beta[2]`,
    parameter3 = `beta[3]`
  ) %>%
  gather_draws(parameter1, parameter2, parameter3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), parameter_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:(sim_values$I - length(sim_values$J))),
  values = c(
    # First discrete variable.
    sim_values$beta[2] - sim_values$beta[1],
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[3],
    sim_values$beta[5] - sim_values$beta[3]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[1]`,
    contrast2 = `beta[2]`,
    contrast3 = `beta[3]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-dummy-01.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

contrast_values <- tibble(
  .variable = str_c("contrast", 1:2),
  values = c(
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[5],
    sim_values$beta[5] - sim_values$beta[4]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[2]` - `beta[3]`,
    contrast2 = `beta[3]` - `beta[2]`
  ) %>%
  gather_draws(contrast1, contrast2) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-dummy-02.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

# # Save output.
# fit_dummy$save_object(file = here::here("content", "post", "discrete-coding", "Output", "fit_dummy.rds"))

# Effects Coding ----------------------------------------------------------
# Specify data.
data <- list(
  N = length(sim_y),    # Number of observations.
  I = ncol(sim_X) - 2,  # Number of covariates.
  y = sim_y,            # Vector of observations.
  X = sim_X[, -c(2, 5)] # Matrix of covariates.
)

# Compile the model.
flat_regression_dummy <- cmdstan_model(
  stan_file = here::here("content", "post", "discrete-coding", "Code", "flat_regression_dummy.stan"),
  dir = here::here("content", "post", "discrete-coding", "Code", "Compiled")
)

# Fit the model.
fit_dummy <- flat_regression_dummy$sample(
  data = data,
  chains = 4,
  parallel_chains = 4,
  seed = 42
)

# Check diagnostics.
fit_dummy$cmdstan_diagnose()

# Check on trace plots for index coding with two discrete variables.
mcmc_trace(fit_dummy$draws(variables = c("alpha", "beta")))

# Try fitting with $optimize().
test_dummy <- flat_regression_dummy$optimize(
  data = data,
  seed = 42
)
test_dummy$summary()

# Check on parameter recovery for index coding with two discrete variables.
parameter_values <- tibble(
  .variable = str_c("parameter", 1:(sim_values$I - length(sim_values$J))),
  values = c(
    # First discrete variable.
    sim_values$beta[2],
    # Second discrete variable.
    sim_values$beta[4], sim_values$beta[5]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    parameter1 = `beta[1]`,
    parameter2 = `beta[2]`,
    parameter3 = `beta[3]`
  ) %>%
  gather_draws(parameter1, parameter2, parameter3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), parameter_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:(sim_values$I - length(sim_values$J))),
  values = c(
    # First discrete variable.
    sim_values$beta[2] - sim_values$beta[1],
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[3],
    sim_values$beta[5] - sim_values$beta[3]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[1]`,
    contrast2 = `beta[2]`,
    contrast3 = `beta[3]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-dummy-01.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

contrast_values <- tibble(
  .variable = str_c("contrast", 1:2),
  values = c(
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[5],
    sim_values$beta[5] - sim_values$beta[4]
  )
)
fit_dummy$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[2]` - `beta[3]`,
    contrast2 = `beta[3]` - `beta[2]`
  ) %>%
  gather_draws(contrast1, contrast2) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-dummy-02.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

# # Save output.
# fit_dummy$save_object(file = here::here("content", "post", "discrete-coding", "Output", "fit_dummy.rds"))

# Index Coding ------------------------------------------------------------
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

# Check diagnostics.
fit_index$cmdstan_diagnose()

# Check on trace plots for index coding with two discrete variables.
mcmc_trace(fit_index$draws(variables = "beta"))

# Try fitting with $optimize().
test_index <- flat_regression_index$optimize(
  data = data,
  seed = 42
)
test_index$summary()

# Check on parameter recovery for index coding with two discrete variables.
parameter_values <- tibble(
  .variable = str_c("parameter", 1:sim_values$I),
  values = c(
    # First discrete variable.
    sim_values$beta[1], sim_values$beta[2],
    # Second discrete variable.
    sim_values$beta[3], sim_values$beta[4], sim_values$beta[5]
  )
)
fit_index$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    parameter1 = `beta[1]`,
    parameter2 = `beta[2]`,
    parameter3 = `beta[3]`,
    parameter4 = `beta[4]`,
    parameter5 = `beta[5]`
  ) %>%
  gather_draws(parameter1, parameter2, parameter3, parameter4, parameter5) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), parameter_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

# Extract draws and compare a series of contrasts.
contrast_values <- tibble(
  .variable = str_c("contrast", 1:(sim_values$I - length(sim_values$J))),
  values = c(
    # First discrete variable.
    sim_values$beta[2] - sim_values$beta[1],
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[3],
    sim_values$beta[5] - sim_values$beta[3]
  )
)
fit_index$draws(variables = c("beta"), format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[2]` - `beta[1]`,
    contrast2 = `beta[4]` - `beta[3]`,
    contrast3 = `beta[5]` - `beta[3]`
  ) %>%
  gather_draws(contrast1, contrast2, contrast3) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-index-01.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

contrast_values <- tibble(
  .variable = str_c("contrast", 1:2),
  values = c(
    # Second discrete variable.
    sim_values$beta[4] - sim_values$beta[5],
    sim_values$beta[5] - sim_values$beta[4]
  )
)
fit_index$draws(variables = "beta", format = "draws_df") %>%
  mutate_variables(
    contrast1 = `beta[4]` - `beta[5]`,
    contrast2 = `beta[5]` - `beta[4]`
  ) %>%
  gather_draws(contrast1, contrast2) %>%
  ggplot(aes(y = .variable, x = .value)) +
  stat_histinterval() +
  geom_vline(aes(xintercept = values), contrast_values, color = "red") +
  facet_wrap(~ .variable, scales = "free", ncol = 1)

ggsave(
  "flat-contrasts-index-02.png",
  path = here::here("content", "post", "discrete-coding", "Figures"),
  width = 5, height = 4, units = "in"
)

# # Save output.
# fit_index$save_object(file = here::here("content", "post", "discrete-coding", "Output", "fit_index.rds"))
