# Load libraries.
library(tidyverse)
library(bayesplot)

# Read in model output.
fit <- read_rds(here::here("Output", "fit.RDS"))

# Load data.
Y <- readRDS(here::here("Data", "Y.RDS"))
X <- readRDS(here::here("Data", "X.RDS"))

# Extract parameters and predictions.
beta <- extract(fit)$beta
yrep <- extract(fit)$yrep

# Evaluate the choices directly.
ppc_hist(Y, yrep[1:10,])

ggsave(
  "ppc_hist.png", 
  path = here::here("Figures"), 
  width = 6, height = 3, units = "in"
)

# Compute the implied choice probabilities for the final 1/10 of draws.
probs <- NULL
for (r in (dim(beta)[1] - dim(beta)[1] * 1/10):dim(beta)[1]) {
  probs_temp <- NULL
  for (n in 1:length(Y)) {
    exp_xb <- exp(X[n,,] %*% beta[r,])
    max_prob <- max(exp_xb / sum(exp_xb))
    probs <- c(probs, max_prob)
  }
  probs <- cbind(probs, probs_temp)
}

# Make sure there aren't dominating alternatives.
tibble(probs) %>% 
  ggplot(aes(x = probs)) +
  geom_histogram()

ggsave(
  "ppc_probs_plot.png", 
  path = here::here("Figures"), 
  width = 6, height = 3, units = "in"
)

