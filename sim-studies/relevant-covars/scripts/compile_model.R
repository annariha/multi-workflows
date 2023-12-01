library(cmdstanr)

stanfile <- snakemake@input[[1]]
binaryfile <- snakemake@output[[1]]

print(paste("compiling", stanfile))

#model = cmdstanr::cmdstan_model(here::here("sim-studies", "irrelevant-covars", "stan", "one_covar_model.stan"))

model <- cmdstan_model(stan_file = stanfile, exe_file = binaryfile)