#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, parallel, tidyverse, knitr, kableExtra)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# run script to get combinations df
source(here::here("case-studies", "epilepsy", "R", "get_combinations.R"))

# load data ####
loos_with_default <- read_rds(here::here("case-studies", "epilepsy", "results", "loos_with_default.rds"))
loos_obs_randint <- read_rds(here::here("case-studies", "epilepsy", "results", "loos_obs_randint.rds"))

# join with default loos for models without obs-level random intercept
modelnames_without_obs_randint <- setdiff(names(loos_with_default), names(loos_obs_randint))
loos_without_obs_randint <- loos_with_default[modelnames_without_obs_randint]
loos_with_intloo <- c(loos_obs_randint, loos_without_obs_randint)

# extract Pareto k's for all models
pareto_ks_with_default <- purrr::map(loos_with_default, ~.x$diagnostics$pareto_k)
high_pareto_ks_default <- purrr::map_dbl(purrr::map(loos_with_default, ~.x$diagnostics$pareto_k), ~sum(.x > 0.7))
pareto_ks_with_intloo <- purrr::map(loos_with_intloo, ~.x$diagnostics$pareto_k)
high_pareto_ks_intloo <- purrr::map_dbl(purrr::map(loos_with_intloo, ~.x$diagnostics$pareto_k), ~sum(.x > 0.7))

# compare models with loo & model averaging weights ####
comparison_df_default = loo::loo_compare(loos_with_default)
comparison_df_randint = loo::loo_compare(loos_with_intloo)

# create two tables of top 24 models (top 25%) for default and int loos
table_df_default <- comparison_df_default |>
  slice_head(n=10) |>
  mutate(Model = seq(1:NROW()), 
         Family = names(), 
         Prior = ) 
  
  
penguins %>% 
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    align = "l",
    col.names = linebreak(column_names, align = "l"),
    escape = FALSE
  ) %>%
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )
