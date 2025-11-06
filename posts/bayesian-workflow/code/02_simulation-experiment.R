# Load libraries.
library(tidyverse)
library(rstan)
library(bayesplot)
library(tidybayes)
library(posterior)

# Set Stan options.
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# TEST ---------------------------------------------------

# # Stan User's Guide Multi-Logit Regression --------------------------------
# sim_values <- list(
#   N = 100,           # Number of observations.
#   P = 3,             # Number of product alternatives.
#   L = 10             # Number of (estimable) attribute levels.
# )
#
# R <- 50
#
# # Simulate data.
# test_sim_data <- stan(
#   file = here::here("content", "post", "tidy-bayes", "Code", "test_mnl_simulate.stan"),
#   data = sim_values,
#   iter = R,
#   warmup = 0,
#   chains = 1,
#   refresh = R,
#   seed = 42,
#   algorithm = "Fixed_param"
# )
#
# # Extract the data and betas from the first simulated dataset.
# y <- extract(test_sim_data)$y[1,]
# x <- extract(test_sim_data)$x[1,,]
# beta <- extract(test_sim_data)$beta[1,,]
# # Y <- extract(test_sim_data)$Y[1,]
# # X <- extract(test_sim_data)$X[1,,,]
# # beta <- extract(test_sim_data)$beta[1,]
#
# data <- list(
#   N = length(y),           # Number of observations.
#   P = max(y),              # Number of product alternatives.
#   L = ncol(x),             # Number of (estimable) attribute levels.
#   y = y,                   # Vector of observed choices.
#   x = x                    # Experimental design for each observations.
#   # L = ncol(X),             # Number of (estimable) attribute levels.
#   # Y = Y,                   # Vector of observed choices.
#   # X = X                    # Experimental design for each observations.
# )
#
# test_fit <- stan(
#   file = here::here("content", "post", "tidy-bayes", "Code", "test_mnl_estimate.stan"),
#   data = data,
#   seed = 42
# )
#
# beta_values <- tibble(
#   l = sort(rep(1:(data$L), data$P)),
#   p = rep(1:(data$P), data$L),
#   .variable = str_c("beta", "_", l, "_", p),
#   values = as.vector(t(matrix(beta, ncol = data$P)))
# ) %>%
#   select(.variable, values)
#
# test_fit %>%
#   gather_draws(beta[l, p]) %>%
#   unite(.variable, .variable, l, p) %>%
#   ggplot(aes(x = .value, y = .variable)) +
#   geom_halfeyeh(.width = .95) +
#   geom_vline(aes(xintercept = values), beta_values, color = "red") +
#   facet_wrap(
#     ~ .variable,
#     nrow = data$J,
#     ncol = data$I,
#     scales = "free"
#   )

# # Elea's Multinomial Logit Regression -------------------------------------
# generate_mnl_data <- function(N=1000, K=2, J=3, beta=c(1, -2), alpha=c(1,0,-1)){
#   if(length(beta) != K) stop ("incorrect number of parameters")
#   Y <- rep(NA, N)
#   X <- list(NULL)
#   for (i in 1:N) {
#     X[[i]] <- matrix(rnorm(J*K), ncol=K)
#     Y[i] <- sample(x=J, size=1, prob=exp(alpha+X[[i]]%*%beta))
#   }
#   list(N=N, J=J, K=K, Y=Y, X=X, beta=beta, alpha=alpha)
# }
# d1 <- generate_mnl_data(N=1000, beta=c(-1,1), alpha=c(0,0,0))
#
# data <- list(
#   N = d1$N,           # Number of observations.
#   P = d1$J,           # Number of product alternatives.
#   L = d1$K,           # Number of (estimable) attribute levels.
#   Y = d1$Y,           # Vector of observed choices.
#   X = d1$X            # Experimental design for each observations.
# )
#
# test_fit <- stan(
#   file = here::here("content", "post", "tidy-bayes", "Code", "test_mnl_estimate.stan"),
#   data = data,
#   seed = 42
# )

# # The number of simulated datasets.
# R <- dim(extract(sim_data)$Y)[1]
#
# fit <- NULL
# for (r in 1:R) {
#   # Extract the data from the first simulated dataset.
#   Y <- extract(sim_data)$Y[r,]
#
#   # Specify the data for calibration in a list.
#   data <- list(
#     N = sim_values$N,        # Number of observations.
#     P = sim_values$P,        # Number of product alternatives.
#     L = sim_values$L,        # Number of (estimable) attribute levels.
#     Y = Y,                   # Vector of observed choices.
#     X = sim_values$X         # Experimental design for each observations.
#   )
#
#   # Calibrate the model.
#   fit[[r]] <- stan(
#     file = here::here("content", "post", "tidy-bayes", "Code", "mnl_estimate.stan"),
#     data = data,
#     seed = 42
#   )
# }

# # Comparing cmdstanr runtime with rstan. ----------------------------------
# library(cmdstanr)
#
# model <- cmdstan_model(here::here("content", "post", "tidy-bayes", "Code", "mnl_estimate.stan"))
#
# # Load simulated data.
# sim_values <- readRDS(here::here("content", "post", "tidy-bayes", "Data", "sim_values.rds"))
# sim_data <- readRDS(here::here("content", "post", "tidy-bayes", "Data", "sim_data.rds"))
#
# # Extract the data from the first simulated dataset.
# Y <- rstan::extract(sim_data)$Y[1,]
#
# # Specify the data for calibration in a list.
# data <- list(
#   N = sim_values$N,        # Number of observations.
#   P = sim_values$P,        # Number of product alternatives.
#   L = sim_values$L,        # Number of (estimable) attribute levels.
#   Y = Y,                   # Vector of observed choices.
#   X = sim_values$X         # Experimental design for each observations.
# )
#
# # Calibrate the model.
# fit <- model$sample(
#   data = data,
#   seed = 42,
#   chains = 4,
#   parallel_chains = 2,
#   refresh = 500
# )

# --------------------------------------------------------

# --------------------------------------------------------
# Predictions in generated quantities block is commented out (caused issues).
# --------------------------------------------------------

# Load simulated data.
sim_values <- readRDS(here::here("content", "post", "tidy-bayes", "Data", "sim_values.rds"))
sim_data <- readRDS(here::here("content", "post", "tidy-bayes", "Data", "sim_data.rds"))

# Extract the data from the first simulated dataset.
Y <- extract(sim_data)$Y[1,]

# Specify the data for calibration in a list.
data <- list(
  N = sim_values$N,        # Number of observations.
  P = sim_values$P,        # Number of product alternatives.
  L = sim_values$L,        # Number of (estimable) attribute levels.
  Y = Y,                   # Vector of observed choices.
  X = sim_values$X         # Experimental design for each observations.
)

# Calibrate the model.
# m <- stan_model(file = here::here("content", "post", "tidy-bayes", "Code", "mnl_estimate.stan"))
fit <- stan(
# fit <- vb(
  # object = m,
  file = here::here("content", "post", "tidy-bayes", "Code", "mnl_estimate.stan"),
  data = data,
  seed = 42
)

loo(fit)

# Check divergences.
source(here::here("content", "post", "tidy-bayes", "Code", "stan_utility.R"))

check_div(fit)

as.matrix(fit) %>%
  mcmc_scatter(
    pars = c("beta[1]", "beta[2]"),
    np = nuts_params(fit),
    np_style = scatter_style_np(div_color = "green", div_alpha = 0.5)
  )

# ggsave(
#   "mcmc_scatter.png",
#   path = here::here("Figures"),
#   width = 6, height = 3, units = "in"
# )

# Check the effective sample size.
check_n_eff(fit)

# Check the Rhat statistic.
check_rhat(fit)

# Check trace plots.
fit %>%
  mcmc_trace(
    pars = str_c("beta[", 1:data$L, "]"),
    n_warmup = 1000,
    facet_args = list(ncol = 3, labeller = label_parsed)
  )

# ggsave(
#   "mcmc_trace.png",
#   path = here::here("Figures"),
#   width = 9, height = 3, units = "in"
# )

# Recover parameter values.
# mcmc_recover_hist(As.mcmc.list(p1, pars="beta"), true=d1$beta)
sim_beta <- extract(sim_data)$beta[1,]
beta_values <- tibble(
  l = 1:data$L,
  beta = sim_beta
)

fit %>%
  spread_draws(beta[l]) %>%
  ggplot(aes(x = beta, y = l)) +
  geom_halfeyeh(.width = .95) +
  geom_vline(aes(xintercept = beta), beta_values, color = "red") +
  facet_wrap(
    ~ l,
    scales = "free"
  )

# ggsave(
#   "mcmc_hist.png",
#   path = here::here("Figures"),
#   width = 6, height = 3, units = "in"
# )

