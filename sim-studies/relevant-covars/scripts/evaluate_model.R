# input: model fit 
# output: loo object

library(loo)
library(cmdstanr)

modelfit <- readRDS(file = snakemake@input[[3]]) 

loo_object <- loo(modelfit$draws("log_lik"), 
                  r_eff=relative_eff(modelfit$draws("log_lik")))

#saveRDS(loo_object, file = snakemake@output[[1]])
saveRDS(list(loo_object = loo_object, 
             model_name = c(paste("model", snakemake@wildcards$idx, snakemake@wildcards$stanfile, snakemake@wildcards$signal_idx, sep="_")),
             snakemake_info = snakemake), file = snakemake@output[[1]])