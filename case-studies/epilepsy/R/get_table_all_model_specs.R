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

# load helper functions
source(here::here("case-studies", "epilepsy", "R", "build_formula_string.R"))

# load data 
data <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "models_combs_df.rds"))

data_table_models <- data |>
  select(model_id, family, priors, formula) |>
  mutate(priors = fct_recode(
    priors, 
    "default in \\texttt{brms}" = "brms_default", 
    "\\texttt{brms::horseshoe(3)}" = "brms_horseshoe")) |>
  mutate(family = fct_recode(
    family, 
    "Poisson" = "poisson", 
    "Neg. Binomial" = "negbinomial"
  )) |>
  remove_rownames()

data_table_reduced_models <- data |>
  filter(obs == "" & patient == "" & visit == "") |>
  select(model_id, family, priors, formula) |>
  mutate(priors = fct_recode(
    priors, 
    "default in \\texttt{brms}" = "brms_default", 
    "\\texttt{brms::horseshoe(3)}" = "brms_horseshoe")) |>
  mutate(family = fct_recode(
    family, 
    "Poisson" = "poisson", 
    "Neg. Binomial" = "negbinomial"
  )) |>
  remove_rownames()

column_names <- c("Model ID", "Obs. family", "Priors", "Formula")

# table with model id, family, prior, formula (PPC) 
data_table_reduced_models %>%
  knitr::kable(format = "latex", booktabs = TRUE, longtable = TRUE,
               linesep = "", align = "l", 
               col.names = linebreak(column_names, align = "l"),
               escape = FALSE) %>%
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("repeat_header")
  )

# table with model id, family, prior, formula (PSIS-LOO-CV)
data_table_models %>%
  knitr::kable(format = "latex", booktabs = TRUE, longtable = TRUE,
               linesep = "", align = "l", 
               col.names = linebreak(column_names, align = "l"),
               escape = FALSE) %>%
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("repeat_header")
  )

