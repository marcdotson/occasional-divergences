# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(loo)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generate Data -----------------------------------------------------------
# Specify data and hyperprior values.
sim_values <- list(
  R = 500,           # Number of respondents.
  S = 10,            # Number of choice tasks.
  A = 4,             # Number of choice alternatives.
  L = c(3, 4, 5),    # Number of levels in each discrete attribute.
  I = 12,            # Number of estimable attribute levels, including the brand intercept.
  J = 3,             # Number of population-level covariates.

  Gamma_mean = 0,    # Mean of population-level means.
  Gamma_scale = 5,   # Scale of population-level means.
  Omega_shape = 2,   # Shape of population-level scale.
  tau_df = 2         # Degrees of freedom of population-level scale.
)

# Array of observation-level covariates.
X <- array(
  NA,
  dim = c(sim_values$R, sim_values$S, sim_values$A, sim_values$I)
)
for (r in 1:sim_values$R) {
  for (s in 1:sim_values$S) {
    # Discrete predictors.
    X_s <- NULL
    for (l in 1:length(sim_values$L)) {
      X_l <- NULL
      for (a in 1:sim_values$A) {
        X_a <- matrix(0, nrow = 1, ncol = sim_values$L[l])
        X_a[1, sample(seq(1, sim_values$L[l]), 1)] <- 1
        if (l == 1) X_l <- rbind(X_l, X_a)
        if (l != 1) X_l <- rbind(X_l, X_a[, -1])
      }
      X_s <- cbind(X_s, X_l)
    }

    # Continuous predictors.
    L_n <- sim_values$I - (sum(sim_values$L) - length(sim_values$L) + 1)
    if(L_n != 0) {
      X_s <- cbind(X_s, matrix(rnorm(sim_values$A * L_n), ncol = L_n))
    }
    X[r, s, , ] <- X_s
  }
}
sim_values$X <- X

# Matrix of population-level covariates.
Z <- cbind(
  rep(1, sim_values$R),
  matrix(
    runif(sim_values$R * (sim_values$J - 1), min = 2, max = 5),
    nrow = sim_values$R
  )
)
sim_values$Z <- Z

# Generate data.
sim_data <- stan(
  file = here::here("content", "post", "choice-models", "Code", "generate_data.stan"),
  data = sim_values,
  iter = 1,
  chains = 1,
  seed = 42,
  algorithm = "Fixed_param"
)

# Save simulation values and data.
write_rds(
  sim_values,
  path = here::here("content", "post", "choice-models", "Data", "sim_values.rds")
)
write_rds(
  sim_data,
  path = here::here("content", "post", "choice-models", "Data", "sim_data.rds")
)

# Load simulation values and data.
sim_values <- read_rds(here::here("content", "post", "choice-models", "Data", "sim_values.rds"))
sim_data <- read_rds(here::here("content", "post", "choice-models", "Data", "sim_data.rds"))

# Extract simulated data and parameters.
sim_Y <- extract(sim_data)$Y[1,,]
sim_Gamma <- extract(sim_data)$Gamma[1,,]
sim_Omega <- extract(sim_data)$Omega[1,,]
sim_tau <- extract(sim_data)$tau[1,]
sim_Beta <- extract(sim_data)$Beta[1,,]

# Centered Parameterization -----------------------------------------------
data <- list(
  R = sim_values$R,    # Number of respondents.
  S = sim_values$S,    # Number of choice tasks.
  A = sim_values$A,    # Number of choice alternatives.
  I = sim_values$I,    # Number of observation-level covariates.
  J = sim_values$J,    # Number of population-level covariates.

  Gamma_mean = 0,      # Mean of population-level means.
  Gamma_scale = 5,     # Scale of population-level means.
  Omega_shape = 2,     # Shape of population-level scale.
  tau_mean = 0,        # Mean of population-level scale.
  tau_scale = 5,       # Scale of population-level scale.

  Y = sim_Y,           # Matrix of observations.
  X = sim_values$X,    # Array of observation-level covariates.
  Z = sim_values$Z     # Matrix of population-level covariates.
)

fit_centered <- stan(
  file = here::here("content", "post", "choice-models", "Code", "hmnl_centered.stan"),
  data = data,
  iter = 4000,
  thin = 2,
  # control = list(adapt_delta = 0.99),
  seed = 42
)

# Save model output.
write_rds(
  fit_centered,
  here::here("content", "post", "choice-models", "Output", "fit_centered.rds")
)

# Load model output.
fit_centered <- read_rds(
  here::here("content", "post", "choice-models", "Output", "fit_centered.rds")
)

# Non-Centered Parameterization -------------------------------------------
data <- list(
  R = sim_values$R,    # Number of respondents.
  S = sim_values$S,    # Number of choice tasks.
  A = sim_values$A,    # Number of choice alternatives.
  I = sim_values$I,    # Number of observation-level covariates.
  J = sim_values$J,    # Number of population-level covariates.

  Gamma_mean = 0,      # Mean of population-level means.
  Gamma_scale = 5,     # Scale of population-level means.
  Omega_shape = 2,     # Shape of population-level scale.
  tau_mean = 0,        # Mean of population-level scale.
  tau_scale = 5,       # Scale of population-level scale.

  Y = sim_Y,           # Matrix of observations.
  X = sim_values$X,    # Array of observation-level covariates.
  Z = sim_values$Z     # Matrix of population-level covariates.
)

# Calibrate the model.
fit_noncentered <- stan(
  file = here::here("content", "post", "choice-models", "Code", "hmnl_noncentered.stan"),
  data = data,
  iter = 4000,
  thin = 2,
  seed = 42
)

# Save model output.
write_rds(
  fit_noncentered,
  here::here("content", "post", "choice-models", "Output", "fit_noncentered.rds")
)

# Load model output.
fit_noncentered <- read_rds(
  here::here("content", "post", "choice-models", "Output", "fit_noncentered.rds")
)

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
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-gamma.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 20, units = "in"
)

# Omega.
fit_noncentered %>%
  mcmc_trace(
    pars = omega_string,
    n_warmup = 500,
    facet_args = list(
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-omega.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 40, units = "in"
)

# tau.
fit_noncentered %>%
  mcmc_trace(
    pars = tau_string,
    n_warmup = 500,
    facet_args = list(
      ncol = 4,
      labeller = label_parsed
    )
  )

ggsave(
  "mcmc_trace-tau.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 5, units = "in"
)

# # Check observation model trace plots.
# beta_string <- str_c("Beta[", 1:data$R, ",", 1, "]")
# for (i in 2:data$I) {
#   beta_temp <- str_c("Beta[", 1:data$R, ",", i, "]")
#   beta_string <- c(beta_string, beta_temp)
# }
# beta_string_rand <- beta_string[sample(1:(data$R * data$I), 32)]
#
# # Beta.
# fit_noncentered %>%
#   mcmc_trace(
#     pars = beta_string_rand,
#     n_warmup = 500,
#     facet_args = list(
#       ncol = 4,
#       labeller = label_parsed
#     )
#   )
#
# ggsave(
#   "mcmc_trace-beta.png",
#   path = here::here("content", "post", "choice-models", "Figures"),
#   width = 12, height = 20, units = "in"
# )

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
    ncol = 4,
    scales = "free"
  )

ggsave(
  "marginals-gamma.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 20, units = "in"
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
    ncol = 4,
    scales = "free"
  )

ggsave(
  "marginals-omega.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 40, units = "in"
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
    ncol = 4,
    scales = "free"
  )

ggsave(
  "marginals-tau.png",
  path = here::here("content", "post", "choice-models", "Figures"),
  width = 12, height = 5, units = "in"
)

# # Recover Beta values.
# beta_rand <- sort(sample(1:(data$R * data$I), 32))
# beta_values <- tibble(
#   n = sort(rep(1:(data$R), data$I)),
#   i = rep(1:(data$I), data$R),
#   .variable = str_c("Beta", "_", n, "_", i),
#   values = as.vector(t(matrix(sim_Beta, ncol = data$I)))
# ) %>%
#   mutate(id = row_number()) %>%
#   filter(id %in% beta_rand) %>%
#   select(.variable, values)
#
# beta_ids <- fit_noncentered %>%
#   gather_draws(Beta[n, i]) %>%
#   unite(.variable, .variable, n, i) %>%
#   distinct(.variable) %>%
#   mutate(id = row_number()) %>%
#   select(.variable, id)
#
# fit_noncentered %>%
#   gather_draws(Beta[n, i]) %>%
#   unite(.variable, .variable, n, i) %>%
#   left_join(beta_ids) %>%
#   filter(id %in% beta_rand) %>%
#   ggplot(aes(x = .value, y = .variable)) +
#   geom_halfeyeh(.width = .95) +
#   geom_vline(aes(xintercept = values), beta_values, color = "red") +
#   facet_wrap(
#     ~ .variable,
#     ncol = 4,
#     scales = "free"
#   )
#
# ggsave(
#   "marginals-beta.png",
#   path = here::here("content", "post", "choice-models", "Figures"),
#   width = 12, height = 20, units = "in"
# )

# Miscellaneous -----------------------------------------------------------
# # Load model output.
# fit_noncentered <- read_rds(here::here("Output", "hmnl-noncentered_fit.RDS"))
# fit_conjugate <- read_rds(here::here("Output", "hmnl-conjugate-20k_fit.RDS"))
# colnames(fit_conjugate$Gammadraw) <-
#   c(
#     "Theta[1,1]", "Theta[2,1]", "Theta[3,1]", "Theta[4,1]", "Theta[5,1]", "Theta[6,1]",
#     "Theta[7,1]", "Theta[8,1]", "Theta[9,1]", "Theta[10,1]", "Theta[11,1]", "Theta[12,1]"
#   )
#
#
# # Check trace plots.
# fit_noncentered %>%
#   extract(
#     inc_warmup = TRUE,
#     permuted = FALSE
#   ) %>%
#   mcmc_trace(
#     regex_pars = "Theta",
#     n_warmup = 1000,
#     facet_args = list(nrow = 2, labeller = label_parsed)
#   )
#
# ggsave(
#   "mcmc_trace_noncentered.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )
#
# fit_conjugate$Gammadraw %>%
#   mcmc_trace(
#     n_warmup = 500,
#     facet_args = list(nrow = 2, labeller = label_parsed)
#   )
#
# ggsave(
#   "mcmc_trace_conjugate.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )
#
# # Recover parameter values.
# Theta <- tibble(i = as.factor(1:ncol(sim_data$Theta)), Theta = t(sim_data$Theta))
#
# draws_centered <- fit_centered %>%
#   spread_draws(Theta[i, j]) %>%
#   mutate(model = "centered") %>%
#   select(model, .chain, .iteration, .draw, i, j, Theta) %>%
#   ungroup()
#
# draws_noncentered <- fit_noncentered %>%
#   spread_draws(Theta[i, j]) %>%
#   mutate(model = "noncentered") %>%
#   select(model, .chain, .iteration, .draw, i, j, Theta) %>%
#   ungroup()
#
# draws_conjugate <- as_tibble(fit_conjugate$Gammadraw) %>%
#   mutate(
#     .draw = row_number(),
#     .iteration = row_number()
#   ) %>%
#   gather(key = i, value = Theta, -c(.draw, .iteration)) %>%
#   separate(col = i, into = c("temp1", "i"), sep = "\\[") %>%
#   separate(col = i, into = c("i", "j"), sep = ",") %>%
#   separate(col = j, into = c("j", "temp2"), sep = "\\]") %>%
#   mutate(
#     model = "conjugate",
#     .chain = as.integer(1),
#     i = as.integer(i),
#     j = as.integer(j)
#   ) %>%
#   select(model, .chain, .iteration, .draw, i, j, Theta) %>%
#   arrange(.iteration) %>%
#   filter(.iteration > 500)
#
# draws <- draws_centered %>%
#   bind_rows(draws_noncentered) %>%
#   bind_rows(draws_conjugate)
#
# draws %>%
#   mutate(
#     model = factor(model),
#     model = fct_relevel(
#       model, "noncentered", "centered", "conjugate"
#     )
#   ) %>%
#   ggplot(aes(x = Theta, y = model)) +
#   geom_halfeyeh(.width = c(.95, .95)) +
#   facet_wrap(
#     ~ as.factor(i),
#     nrow = 3,
#     ncol = 4,
#     scales = "free_x"
#   ) +
#   geom_vline(aes(xintercept = Theta), Theta, color = "red")
#
# ggsave(
#   "mcmc_marginal_posteriors.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )

