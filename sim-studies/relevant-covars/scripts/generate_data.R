# load libraries 

idx = as.numeric(snakemake@wildcards$idx)
seed = set.seed(idx)
n_obs = 100 #as.numeric(snakemake@wildcards$n_obs)
#complexity of true dgp
#n_sums = 10 #as.numeric(snakemake@wildcards$n_sums)
n_relevant = 4
n_irrelevant = 6
noise_to_signal = 0.1 #var of true signal compared to var of noise
  
# 1d position vector, input for GP 
x = runif(n = n_obs, min= -1, max = 1)
# 10x100 matrix (n_sums x n_obs)
y = matrix(0, nrow = 10, ncol = 100)
# first term is constant for each x
y[1,] = rep(rnorm(1), times = 100)

# polynomial
for (i in 2:n_relevant){
  y[i,] = y[i-1,] + rnorm(1)*x^(i-1)
}

for (i in 5:10){
  # last true signal as baseline plus noise 
  y[i,] = y[n_relevant,] + rnorm(100)*noise_to_signal
}

obs <- y[n_relevant,] + rnorm(100)*noise_to_signal 

dataset <- list(
  x = x,
  y = y, 
  obs = obs
)

# store generated data 
saveRDS(dataset, file = snakemake@output[[1]])