# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generate Data -----------------------------------------------------------
# Specify data and hyperparameter values.
sim_values <- list(
  N = 500,                            # Number of observations.
  K = 5,                              # Number of groups.
  I = 7,                              # Number of observation-level covariates.
  J = 3,                              # Number of population-level covariates.

  # Vector of group assignments.
  g = sample(5, 500, replace = TRUE),

  # Matrix of observation-level covariates.
  X = cbind(
    rep(1, 500),
    matrix(runif(500 * (7 - 1), min = 1, max = 10), nrow = 500)
  ),

  # Matrix of population-level covariates.
  Z = cbind(
    rep(1, 5),
    matrix(runif(5 * (3 - 1), min = 2, max = 5), nrow = 5)
  )
)

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "non-centered", "Code", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Save simulation values and data.
write_rds(
  sim_values,
  path = here::here("content", "post", "non-centered", "Data", "sim_values.rds")
)
write_rds(
  sim_data,
  path = here::here("content", "post", "non-centered", "Data", "sim_data.rds")
)

# Load simulation values and data.
sim_values <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_values.rds"))
sim_data <- read_rds(here::here("content", "post", "non-centered", "Data", "sim_data.rds"))

# Extract simulated data and parameters.
sim_y <- extract(sim_data)$y
sim_Gamma <- extract(sim_data)$Gamma
sim_Omega <- extract(sim_data)$Omega
sim_tau <- extract(sim_data)$tau
sim_Beta <- extract(sim_data)$Beta
sim_sigma <- extract(sim_data)$sigma

# Fit Centered Parameterization -------------------------------------------
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

fit_centered <- stan(
  file = here::here("content", "post", "non-centered", "Code", "hlm_centered.stan"),
  data = data,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model run.
write_rds(
  fit_centered,
  path = here::here("content", "post", "non-centered", "Output", "fit_centered.rds")
)

# Load model run.
fit_centered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_centered.rds"))

# Fit Non-Centered Parameterization ---------------------------------------
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

fit_noncentered <- stan(
  file = here::here("content", "post", "non-centered", "Code", "hlm_noncentered.stan"),
  data = data,
  iter = 10000,
  thin = 5,
  control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model run.
write_rds(
  fit_noncentered,
  path = here::here("content", "post", "non-centered", "Output", "fit_noncentered.rds")
)

# Load model run.
fit_noncentered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_noncentered.rds"))

# Check population model trace plots.
gamma_string <- str_c("Gamma[", 1:data$J, ",", 1, "]")
omega_string <- str_c("Omega[", 1:data$I, ",", 1, "]")
tau_string <- str_c("tau[", 1:data$I, "]")
for (i in 2:data$I) {
  gamma_temp <- str_c("Gamma[", 1:data$J, ",", i, "]")
  gamma_string <- c(gamma_string, gamma_temp)
  omega_temp <- str_c("Omega[", 1:data$I, ",", i, "]")
  omega_string <- c(omega_string, omega_temp)
}

# Gamma.
fit_noncentered %>%
  mcmc_trace(
    pars = gamma_string,
    n_warmup = 500,
    facet_args = list(
      nrow = ceiling(length(gamma_string) / 4),
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-gamma.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 8, height = 8, units = "in"
)

# Omega.
fit_noncentered %>%
  mcmc_trace(
    pars = omega_string,
    n_warmup = 500,
    facet_args = list(
      nrow = ceiling(length(omega_string) / 4),
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-omega.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 8, height = 16, units = "in"
)

# tau.
fit_noncentered %>%
  mcmc_trace(
    pars = tau_string,
    n_warmup = 500,
    facet_args = list(
      nrow = ceiling(length(tau_string) / 4),
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-tau.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 8, height = 4, units = "in"
)

# Check observation model trace plots.
beta_string <- str_c("Beta[", 1:data$K, ",", 1, "]")
for (i in 2:data$I) {
  beta_temp <- str_c("Beta[", 1:data$K, ",", i, "]")
  beta_string <- c(beta_string, beta_temp)
}

# Beta and sigma.
fit_noncentered %>%
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
  "mcmc_trace-beta_sigma.png",
  path = here::here("content", "post", "non-centered", "Figures"),
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

fit_noncentered %>%
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
  "marginals-gamma.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 16, height = 8, units = "in"
)

# Recover Omega values.
omega_values <- tibble(
  j = sort(rep(1:(data$I), data$I)),
  i = rep(1:(data$I), data$I),
  .variable = str_c("Omega", "_", j, "_", i),
  values = as.vector(t(matrix(sim_Omega, ncol = data$I)))
) %>%
  select(.variable, values)

fit_noncentered %>%
  gather_draws(Omega[j, i]) %>%
  unite(.variable, .variable, j, i) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), omega_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = data$I,
    ncol = data$I,
    scales = "free"
  )

ggsave(
  "marginals-omega.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 16, height = 8, units = "in"
)

# Recover tau values.
tau_values <- tibble(
  i = 1:(data$I),
  .variable = str_c("tau", "_", i),
  values = as.vector(sim_tau)
) %>%
  select(.variable, values)

fit_noncentered %>%
  gather_draws(tau[i]) %>%
  unite(.variable, .variable, i) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), tau_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = ceiling(data$I / 4),
    ncol = 4,
    scales = "free"
  )

ggsave(
  "marginals-tau.png",
  path = here::here("content", "post", "non-centered", "Figures"),
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

fit_noncentered %>%
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
  "marginals-beta.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 16, height = 12, units = "in"
)

# Recover sigma value.
sigma_values <- tibble(
  .variable = "sigma",
  values = sim_sigma,
)

fit_noncentered %>%
  gather_draws(sigma) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = values), sigma_values, color = "red") +
  facet_wrap(
    ~ .variable,
    nrow = 1,
    scales = "free"
  )

ggsave(
  "marginals-sigma.png",
  path = here::here("content", "post", "non-centered", "Figures"),
  width = 4, height = 3, units = "in"
)

# Compute Model Fit -------------------------------------------------------
# Load packages.
library(loo)

# Load model runs.
fit_centered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_centered.rds"))
fit_noncentered <- read_rds(here::here("content", "post", "non-centered", "Output", "fit_noncentered.rds"))

# Centered parameterization.
log_lik_centered <- extract_log_lik(fit_centered, merge_chains = FALSE)
r_eff_centered <- relative_eff(exp(log_lik_centered))
loo(log_lik_centered, r_eff = r_eff_centered)
loo(fit_centered, save_psis = TRUE)

loo_centered <- loo(fit_centered, save_psis = TRUE)

# Non-centered parameterization.
log_lik_noncentered <- extract_log_lik(fit_noncentered, merge_chains = FALSE)
r_eff_noncentered <- relative_eff(exp(log_lik_noncentered))
loo(log_lik_noncentered, r_eff = r_eff_noncentered)
loo(fit_noncentered, save_psis = TRUE)

loo_noncentered <- loo(fit_noncentered, save_psis = TRUE)

# Compare model fit.
loo_compare(loo_centered, loo_noncentered)

