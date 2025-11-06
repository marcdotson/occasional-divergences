# Load libraries.
library(tidyverse)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load data.
Y <- readRDS(here::here("Data", "Y.RDS"))
X <- readRDS(here::here("Data", "X.RDS"))

# Specify the data for calibration in a list.
data <- list(
  N = length(Y),           # Number of observations.
  P = nrow(X[1,,]),        # Number of product alternatives.
  L = ncol(X[1,,]),        # Number of (estimable) attribute levels.
  Y = Y,                   # Vector of observed choices.
  X = X                    # Experimental design for each observations.
)

# Calibrate the model.
fit <- stan(
  file = here::here("Code", "mnl_estimate.stan"),
  data = data,
  seed = 42
)

write_rds(fit, here::here("Output", "fit.RDS"))

# Check divergences.
library(bayesplot)
source(here::here("Code", "stan_utility.R"))

check_div(fit)

# Check the effective sample size.
check_n_eff(fit)

# Check the Rhat statistic.
check_rhat(fit)

