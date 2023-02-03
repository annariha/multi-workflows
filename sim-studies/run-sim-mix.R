library(cmdstanr)
library(rstan)
library(dplyr)
library(ggplot2)
stanfit <- function(fit) rstan::read_stan_csv(fit$output_files())

# stan code ###
stan_code <- "
data {
  int<lower = 0> N; // number of obs
}

parameters {
  vector[2] mu;
  real<lower=0> sigma[2];
}

model {
  // prior
  sigma ~ lognormal(0, 2);
  mu ~ normal(0, 2);
}

generated quantities {
  // simulate data
  vector[N] y_sim;
  for (n in 1:N) {
    y_sim[n] = log_sum_exp(normal_rng(mu[1], sigma[1]), 
                           normal_rng(mu[2], sigma[2]));
  }
}"

stan_file <- write_stan_file(stan_code)

# model data ####
N <- 1
y <- c(rnorm(300), rnorm(700, 0, 3))
run_estimation <- 0

stan_data <- list(
  N = N,
  y = y, 
  run_estimation = run_estimation
)

# compile, fit the model ####
# m1_exec <- cmdstan_model(here::here("sim-studies", "sim-mix-multi.stan"))
m1_exec <- cmdstan_model(stan_file)
m1_fit <- stanfit(
  m1_exec$sample(data = stan_data,
                 chains = 4,
                 parallel_chains = 4,
                 refresh = 0,
                 fixed_param = TRUE)
)

# samples ###
samples <- rstan::extract(m1_fit, 'y_sim')$y_sim
data.frame(samples = samples)

plot_data <- data.frame(samples = samples) %>%
  reshape2::melt()
( p <- ggplot() + 
    geom_density(
      data = plot_data, 
      mapping = aes(x = value, fill = variable),
      alpha = 0.2, 
      colour = NA
    ) + 
    theme_bw()) 