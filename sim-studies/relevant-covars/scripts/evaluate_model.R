# input: model fit 
# output: loo object

library(loo)
library(cmdstanr)
library(matrixStats)

#modelfit <- readRDS(file = snakemake@input[[3]]) 

loo_object <- loo(modelfit$draws("log_lik"), 
                  r_eff=relative_eff(modelfit$draws("log_lik")))

test_draws <- modelfit$draws("log_lik_test")
n_test <- dim(test_draws)[3]
n_chains <- dim(test_draws)[2]
n_draws <- dim(test_draws)[1]
pointwise_test_elpd <- c()

for (i in 1:n_test){
  # Monte Carlo estimate of lpd for the test data
  pointwise_test_elpd[i] <- logSumExp(test_draws[,,i]) - log(n_chains * n_draws)
}

#n_bb <- 100
#bb_elpd <- c()
#for (i in 1:n_bb){
#  bb_elpd[i] <- sum(sample(pointwise_test_elpd, size=100, replace=TRUE))
#}
#hist(bb_elpd)

#saveRDS(loo_object, file = snakemake@output[[1]])
saveRDS(list(loo_object = loo_object, 
             pointwise_test_elpd = pointwise_test_elpd, 
             model_name = c(paste("model", snakemake@wildcards$idx, snakemake@wildcards$stanfile, snakemake@wildcards$signal_idx, sep="_")),
             snakemake_info = snakemake), file = snakemake@output[[1]])