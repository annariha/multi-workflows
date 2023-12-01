# input: data list for Stan, created with the chosen covariate and the true outcome
# output: a model fit 
library(cmdstanr)

dataset <- readRDS(file = snakemake@input[[1]])
stan_exe_file <- snakemake@input[[2]]
signal_idx <- as.numeric(snakemake@wildcards$signal_idx)

data <- list(covars = dataset[["y"]][signal_idx,],
             obs = dataset[["obs"]],
             n_obs = NROW(dataset[["obs"]])) 

# use previoulsy compiled stan executable 
model <- cmdstan_model(exe_file = stan_exe_file)

fit <- model$sample(data = data,
                   seed = 42424242,
                   chains = 4,
                   parallel_chains = 4, 
                   refresh = 500)

saveRDS(fit, file = snakemake@output[[1]]) 