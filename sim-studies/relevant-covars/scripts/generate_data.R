# input: simulation conditions 
# output: data lists to use with Stan file

# to run script with and without snakemake object available
if (exists("snakemake")){
  idx = as.numeric(snakemake@wildcards$idx)
  # n_obs = as.numeric(snakemake@wildcards$n_obs)
  #complexity of true dgp
  #n_covars = 10 #as.numeric(snakemake@wildcards$n_covars)
  data_file <- snakemake@output[[1]]
} else {
  # for testing 
  idx <- 1
  data_file <- here::here("sim-studies", "relevant-covars", "outputs", "tmp", "datasets", paste0("data_", idx, ".rds"))
}

# set seed
set.seed(4242 + idx)

# other conditions 
n_train = 100
n_test = 100
n_obs = n_train + n_test
# number of relevant and irrelevant covariates 
n_relevant = 4
n_irrelevant = 6
# var of true signal compared to var of noise
noise_to_signal = 0.1 
  
# 1d position vector, could also be input for GP 
x = runif(n = n_obs, min= -1, max = 1)

# initialise n_obs x n_covars matrix
covars = matrix(0, nrow = n_obs, ncol = sum(n_relevant, n_irrelevant))

# first possible covariate is initialised as a constant
covars[, 1] = rep(rnorm(1), times = n_obs)

# starting from constant, create more possible covariates with polynomial 

for (i in 2:n_relevant){
  covars[, i] = covars[, i-1] + rnorm(1)*x^(i-1)/sd(x^(i-1))
}

# for creating more possible covariates use last true signal as baseline plus noise  
covars[10, n_relevant] <- (4 * sd(covars[, n_relevant])) + mean(covars[, n_relevant]) 
  
for (i in 5:10){
  covars[, i] = covars[, n_relevant] + rnorm(n_obs)*noise_to_signal
}

# generate observations as last true signal in covars[, n_relevant] plus noise 
obs <- covars[, n_relevant] + rnorm(n_obs)*noise_to_signal 

# add noise to covariates 1-4 -> we expect the models with one of those covariates to be worse than models 5-10 most of the time 
for (i in 1:n_relevant){
  covars[, i] = covars[, i] + rnorm(n_obs)*noise_to_signal
}

# above code was added such that no model is using the true signal, i.e., no model will be the single best model 
# remove line 33-35 to have one model that is clearly the best

test <- dplyr::as_tibble(cbind(obs, covars))

# join in list for use with Stan files
dataset <- list(
  x = x[1:n_train],
  covars_train = covars[1:n_train, ], 
  obs_train = obs[1:n_train],
  covars_test = covars[(n_train+1):n_obs, ], 
  obs_test = obs[(n_train+1):n_obs]
)

# store generated data 
saveRDS(dataset, file = data_file)