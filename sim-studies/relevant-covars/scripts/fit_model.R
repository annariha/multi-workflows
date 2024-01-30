# input: data list for Stan, created with the chosen covariate and the true outcome
# output: a model fit 
library(cmdstanr)

# to run script with and without snakemake object available
if (exists("snakemake")){
  dataset <- readRDS(file = snakemake@input[[1]])
  stan_exe_file <- snakemake@input[[2]]
  signal_idx <- as.numeric(snakemake@wildcards$signal_idx)
  model_file <- snakemake@output[[1]]
} else {
  # for testing 
  dataset <- readRDS(here::here("sim-studies", "relevant-covars", "outputs", "tmp", "datasets", "data_1.rds"))
  stan_exe_file <- here::here("sim-studies", "relevant-covars", "outputs", "model_testlinreg")
  signal_idx <- 1
  model_file <- here::here("sim-studies", "relevant-covars", "outputs", "tmp", "fits", paste0("fit_1_", signal_idx, ".rds"))
}

# data list for Stan file 
data <- list(covars_train = as.matrix(dataset[["covars_train"]][, 1:signal_idx]),
             obs_train = dataset[["obs_train"]],
             n_train = NROW(dataset[["obs_train"]]), 
             n_covars = signal_idx, 
             covars_test = as.matrix(dataset[["covars_test"]][, 1:signal_idx]),
             obs_test = dataset[["obs_test"]],
             n_test = NROW(dataset[["obs_test"]])) 

# use previoulsy compiled Stan executable 
model <- cmdstan_model(exe_file = stan_exe_file)

# fit the model 
fit <- model$sample(data = data,
                   seed = 42424242,
                   chains = 4,
                   parallel_chains = 4, 
                   refresh = 500)

# use wrapper around base::saveRDS() to ensure all posterior draws and diagnostics are saved
fit$save_object(file = model_file) 