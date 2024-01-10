# input: list with loo objects
# output: comparison df for all models 

library(loo)
library(purrr)
library(here)

# inputs and outputs with and without existing snakemake object 
if (exists("snakemake")){
  list_of_inputs <- map(snakemake@input, readRDS) 
  output_file <- snakemake@output[[1]]
} else {
  # for testing 
  idx <- 1
  list_of_files <- list.files(path = here::here("sim-studies", "relevant-covars", "outputs", "loos"), 
                              pattern = paste0("_", idx, "_"), 
                              full.names = TRUE)
  list_of_inputs <- map(list_of_files, readRDS)
  output_file <- here::here("sim-studies", "relevant-covars", "outputs", "tmp", paste0("comparison_plot_df_", idx, ".rds"))
}

# helper function 
source(here::here("sim-studies", "relevant-covars", "scripts", "build_comparison_plot_df.R"))

# hacky access to script by waiting to see temporary file in .snakemake/scripts/..
#Sys.sleep(25)

print(list_of_inputs)

# extract list of loo objects
list_of_loos <- map(list_of_inputs, 1)
names(list_of_loos) <- map(list_of_inputs, 2)
print(list_of_loos)

# comparison df with modelname, elpd_diff, se_diff, MCSE, LOO-BB weight, pseudo-BMA weight
comparison_df <- build_comparison_plot_df(list_of_loos)
print(comparison_df)

# save
saveRDS(comparison_df, file = output_file)