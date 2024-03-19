# fit model for each combination ####
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))

build_fit <- function(row, dataset, ...){
  # for storing results
  filedir = here::here("case-studies", "epilepsy", "results", "prelim")
  if (!dir.exists(filedir)) {dir.create(filedir)}
  # set priors here
  if (row[["priors"]] == "brms_horseshoe"){
    prior = brms::set_prior("horseshoe(3)")
  } else if (row[["priors"]] == "brms_default"){
    prior = NULL
  }
  # fit model with brms
  brm(
    formula = build_brms_formula(row), 
    data = dataset, 
    prior = prior,
    seed = 424242,
    file = here::here("case-studies", "epilepsy", "results", "prelim", digest::digest(build_name(row), algo="md5")),
    backend = "cmdstanr", 
    silent = 2, 
    refresh = 0
  ) 
}