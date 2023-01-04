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
# Specify data and hyperprior values.
sim_values <- list(
  R = 500,           # Number of respondents.
  S = 10,            # Number of choice tasks.
  A = 4,             # Number of choice alternatives.
  L = c(3, 4, 5),    # Number of levels for each discrete attribute.
  I = 12,            # Number of observation-level covariates.
  J = 3,             # Number of population-level covariates.
  
  Gamma_mean = 0,    # Mean of population-level means.
  Gamma_scale = 5,   # Scale of population-level means.
  Omega_shape = 2,   # Shape of population-level scale.
  tau_df = 2         # Degrees of freedom of population-level scale.
)

# Array of observation-level covariates.
sim_X <- array(
  data = NA,
  dim = c(sim_values$R, sim_values$S, sim_values$A, sim_values$I)
)
for (r in 1:sim_values$R) {
  for (s in 1:sim_values$S) {
    temp_S <- NULL
    for (l in 1:length(sim_values$L)) {
      temp_L <- NULL
      for (a in 1:sim_values$A) {
        temp_A <- matrix(0, nrow = 1, ncol = sim_values$L[l])
        temp_A[1, sample(seq(1, sim_values$L[l]), 1)] <- 1
        temp_L <- rbind(temp_L, temp_A)
      }
      temp_S <- cbind(temp_S, temp_L)
    }
    sim_X[r, s,,] <- temp_S
  }
}
sim_values$X <- sim_X

# Matrix of population-level covariates.
sim_Z <- cbind(
  rep(1, sim_values$R),
  matrix(
    runif(sim_values$R * (sim_values$J - 1), min = 2, max = 5),
    nrow = sim_values$R
  )
)
sim_values$Z <- sim_Z

# Compile the model for generating data.
generate_hier_mnl_data <- cmdstan_model(
  stan_file = here::here("posts", "discrete-coding", "Code", "generate_hier_mnl_data.stan"),
  dir = here::here("posts", "discrete-coding", "Code", "Compiled")
)

# Generate data.
sim_data <- generate_hier_mnl_data$sample(
  data = sim_values,
  chains = 1,
  iter_sampling = 1,
  seed = 42,
  fixed_param = TRUE
)

# Extract generated data.
sim_Y <- sim_data$draws(variables = "Y", format = "draws_list") %>%
  pluck(1) %>%
  flatten_dbl()

# rstan::extract(sim_data)$Y[1,,]

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
  stan_file = here::here("posts", "discrete-coding", "Code", "flat_regression_dummy.stan"),
  dir = here::here("posts", "discrete-coding", "Code", "Compiled")
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
  path = here::here("posts", "discrete-coding", "Figures"),
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
  path = here::here("posts", "discrete-coding", "Figures"),
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
  stan_file = here::here("posts", "discrete-coding", "Code", "flat_regression_index.stan"),
  dir = here::here("posts", "discrete-coding", "Code", "Compiled")
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

