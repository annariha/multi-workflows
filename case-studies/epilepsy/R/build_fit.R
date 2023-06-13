# fit model for each combination ####

build_fit <- function(row, dataset, ...){
  # for storing results
  filedir = here::here("case-studies", "epilepsy", "data", "prelim", "modelfits")
  if (!dir.exists(filedir)) {dir.create(filedir)}
  # fit model with brms
  brm(
    formula = build_brms_formula(row), 
    data = dataset, 
    prior = row[["prior"]],
    seed = 424242,
    file = here::here("case-studies", "epilepsy", "data", "prelim", "modelfits", digest::digest(build_name(row), algo="md5")),
    backend = "cmdstanr", 
    silent = 2, 
    refresh = 0
  ) 
}

#test <- combinations_df[3,]
#test_fit <- build_fit(test, dataset = brms::epilepsy)

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