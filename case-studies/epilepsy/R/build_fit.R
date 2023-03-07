# fit model for each combination ####

build_fit <- function(row, ...){
  brm(
    formula = build_brms_formula(row), 
    data = brms::epilepsy, 
    prior = row[["prior"]],
    file = digest::digest(build_name(row), algo="md5"),
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