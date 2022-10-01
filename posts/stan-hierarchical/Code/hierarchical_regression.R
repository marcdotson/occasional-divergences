# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Simple Flat Regression --------------------------------------------------
# Specify data and parameter values.
sim_values <- list(
  N = 100, # Number of observations.
  mu = 5,  # Mean of the regression.
  tau = 1  # Variance of the regression.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_flat_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data.
sim_y <- extract(sim_data)$y

# Specify data.
data <- list(
  N = length(sim_y),   # Number of observations.
  y = as.vector(sim_y) # Vector of observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "flat_regression.stan"),
  data = data,
  seed = 42
)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = c("mu", "tau"),
    n_warmup = 500,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "flat_mcmc_trace.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

# Recover parameter values.
par_values <- tibble(
  .variable = c("mu", "tau"),
  values = c(sim_values$mu, sim_values$tau),
)

fit %>%
  gather_draws(mu, tau) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 2,
    scales = "free"
  )

ggsave(
  "flat_marginals.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

# Simple Hierarchical Regression ------------------------------------------
# Specify data and hyperparameter values.
sim_values <- list(
  N = 500,                            # Number of observations.
  K = 5,                              # Number of groups.
  g = sample(5, 500, replace = TRUE), # Vector of group assignments.
  mu = 5,                             # Mean of the population model.
  tau = 1,                            # Variance of the population model.
  sigma = 1                           # Variance of the likelihood.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_hierarchical_data_01.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data and group intercepts.
sim_y <- extract(sim_data)$y
sim_beta <- extract(sim_data)$beta

# Specify data.
data <- list(
  N = length(sim_y),    # Number of observations.
  K = sim_values$K,     # Number of groups.
  y = as.vector(sim_y), # Vector of observations.
  g = sim_values$g      # Vector of group assignments.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "hierarchical_regression_01.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = c("mu", "tau", str_c("beta[", 1:data$K, "]"), "sigma"),
    n_warmup = 500,
    facet_args = list(nrow = 5, labeller = label_parsed)
  )

ggsave(
  "hierarchical_mcmc_trace-01.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 5, units = "in"
)

# Recover hyperparameter and parameter values.
hyper_par_values <- tibble(
  .variable = c("mu", "tau", "sigma"),
  values = c(sim_values$mu, sim_values$tau, sim_values$sigma),
)

beta_values <- tibble(
  n = 1:data$K,
  beta = as.vector(sim_beta)
)

fit %>%
  gather_draws(mu, tau, sigma) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), hyper_par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 3,
    scales = "free"
  )

ggsave(
  "hierarchical_marginals-01a.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 4, units = "in"
)

fit %>%
  spread_draws(beta[n]) %>%
  ggplot(aes(x = beta, y = n)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = beta), beta_values, color = "red") +
  facet_wrap(
    ~ n,
    nrow = 3,
    scales = "free"
  )

ggsave(
  "hierarchical_marginals-01b.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 4, units = "in"
)

# Multiple Hierarchical Regression ----------------------------------------
# Specify data and hyperparameter values.
sim_values <- list(
  N = 500,                            # Number of observations.
  K = 5,                              # Number of groups.
  I = 7,                              # Number of observation-level covariates.
  J = 3,                              # Number of population-level covariates.

  # Matrix of observation-level covariates.
  X = cbind(
    rep(1, 500),
    matrix(runif(500 * (7 - 1), min = 1, max = 10), nrow = 500)
  ),

  # Matrix of population-level covariates.
  Z = cbind(
    rep(1, 5),
    matrix(runif(5 * (3 - 1), min = 2, max = 5), nrow = 5)
  ),

  g = sample(5, 500, replace = TRUE), # Vector of group assignments.
  tau = 1,                            # Variance of the population model.
  sigma = 1                           # Variance of the likelihood.
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "generate_hierarchical_data_02.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Extract simulated data and group intercepts.
sim_y <- extract(sim_data)$y
sim_Gamma <- extract(sim_data)$Gamma
sim_Beta <- extract(sim_data)$Beta

# Specify data.
data <- list(
  N = sim_values$N,     # Number of observations.
  K = sim_values$K,     # Number of groups.
  I = sim_values$I,     # Number of observation-level covariates.
  J = sim_values$J,     # Number of population-level covariates.
  y = as.vector(sim_y), # Vector of observations.
  g = sim_values$g,     # Vector of group assignments.
  X = sim_values$X,     # Matrix of observation-level covariates.
  Z = sim_values$Z      # Matrix of population-level covariates.
)

# Calibrate the model.
fit <- stan(
  file = here::here("content", "post", "stan-hierarchical", "Code", "hierarchical_regression_02.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Check population model trace plots.
gamma_string <- str_c("Gamma[", 1:data$J, ",", 1, "]")
for (i in 2:data$I) {
  gamma_temp <- str_c("Gamma[", 1:data$J, ",", i, "]")
  gamma_string <- c(gamma_string, gamma_temp)
}
fit %>%
  mcmc_trace(
    pars = c(gamma_string, "tau"),
    n_warmup = 500,
    facet_args = list(
      nrow = ceiling(length(c(gamma_string, "tau")) / 4),
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "hierarchical_mcmc_trace-02a.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 8, height = 8, units = "in"
)

# Check observation model trace plots.
beta_string <- str_c("Beta[", 1:data$K, ",", 1, "]")
for (i in 2:data$I) {
  beta_temp <- str_c("Beta[", 1:data$K, ",", i, "]")
  beta_string <- c(beta_string, beta_temp)
}
fit %>%
  mcmc_trace(
    pars = c(beta_string, "sigma"),
    n_warmup = 500,
    facet_args = list(
      nrow = ceiling(length(c(beta_string, "sigma")) / 4),
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "hierarchical_mcmc_trace-02b.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 8, height = 12, units = "in"
)

# Recover Gamma values.
gamma_values <- tibble(
  j = sort(rep(1:(data$J), data$I)),
  i = rep(1:(data$I), data$J),
  .variable = str_c("Gamma", "_", j, "_", i),
  values = as.vector(t(matrix(sim_Gamma, ncol = data$I)))
) %>%
  select(.variable, values)

fit %>%
  gather_draws(Gamma[j, i]) %>%
  unite(.variable, .variable, j, i) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), gamma_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = data$J,
    ncol = data$I,
    scales = "free"
  )

ggsave(
  "hierarchical_marginals-02a.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 16, height = 8, units = "in"
)

# Recover Beta values.
beta_values <- tibble(
  n = sort(rep(1:(data$K), data$I)),
  i = rep(1:(data$I), data$K),
  .variable = str_c("Beta", "_", n, "_", i),
  values = as.vector(t(matrix(sim_Beta, ncol = data$I)))
) %>%
  select(.variable, values)

fit %>%
  gather_draws(Beta[n, i]) %>%
  unite(.variable, .variable, n, i) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), beta_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = data$K,
    ncol = data$I,
    scales = "free"
  )

ggsave(
  "hierarchical_marginals-02b.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 16, height = 12, units = "in"
)

# Recover tau and sigma values.
hyper_par_values <- tibble(
  .variable = c("tau", "sigma"),
  values = c(sim_values$tau, sim_values$sigma),
)

fit %>%
  gather_draws(tau, sigma) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), hyper_par_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 2,
    scales = "free"
  )

ggsave(
  "hierarchical_marginals-02c.png",
  path = here::here("content", "post", "stan-hierarchical", "Figures"),
  width = 7, height = 3, units = "in"
)

