# fit model for each combination ####
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))

build_fit <- function(row, dataset, ...){
  # for storing results
  filedir = here::here("case-studies", "epilepsy", "results", "prelim")
  if (!dir.exists(filedir)) {dir.create(filedir)}
  # set priors here bc old code stopped working after updating 
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
    # the below code used to work, still figuring out why it stopped working after updating
    # prior = row[["prior"]], 
    seed = 424242,
    file = here::here("case-studies", "epilepsy", "results", "prelim", digest::digest(build_name(row), algo="md5")),
    backend = "cmdstanr", 
    silent = 2, 
    refresh = 0
  ) 
}

build_fit_rstan <- function(row, ...){
  brm(
    formula=build_brms_formula(row), 
    data=epilepsy, 
    prior=row[["prior"]],
    file=digest::digest(build_name(row), algo="md5"),
    silent=2, 
    refresh=0,
    # for moment matching 
    save_pars = save_pars(all = TRUE)
  ) 
}