#! /usr/bin/Rscript --vanilla

# Rhat from brms model fit object ####

# Rhat for treatment ####
# works for "b_Trt1" (and eventually "b_zBase:Trt1" if present)
get_rhat_trt <- function(modelfit, parameter_string = "Trt1"){
  rhats = brms::rhat(modelfit)
  # find "b_Trt1" (and if present "b_zBase:Trt1")
  rhat_trt = rhats[str_detect(names(rhats), regex(parameter_string, ignore_case = TRUE))]
  rhat_trt = as.numeric(rhat_trt[1])
  return(rhat_trt)
}

get_high_rhats <-  function(modelfit){
  rhats = brms::rhat(modelfit)
  # from https://mc-stan.org/misc/warnings.html; # "only fully trust the sample if R-hat is less than 1.01"
  out = names(rhats[rhats > 1.01])
  out = list(out)
  return(out)
}

get_prop_rhats <- function(modelfit){
  rhats = brms::rhat(modelfit)
  n_model_variables = brms::nvariables(modelfit)
  # > 1.01 e.g. 3/5 parameters without convergence issues - a, c had issues 
  n_high_rhats = length(rhats[rhats > 1.01])
  prop = n_high_rhats / n_model_variables
  return(prop)
}

# number of divergent transitions ####
# from STAN reference manual: "only few divergences and [...] good Rhat and ESS values, the resulting posterior is often good enough to move forward"
get_ndivtrans <- function(modelfit){
  params_df <- brms::nuts_params(modelfit)
  # from brms documentation
  number_div_trans <- sum(subset(params_df, Parameter == "divergent__")$Value)
  return(number_div_trans)
}

# bulk ESS ####
get_n_low_bulkess <- function(modelfit){
  # number of model parameters 
  n_model_variables = brms::nvariables(modelfit)
  # good bulk ESS: # of chains * 100
  good_ess = brms::nchains(modelfit) * 100
  # get all bulk ESS's
  sm = summary(modelfit)
  bulk_ess_fixed = sm$fixed$Bulk_ESS
  bulk_ess_spec = sm$spec_pars$Bulk_ESS
  all_bulk_ess = c(bulk_ess_fixed, bulk_ess_spec)
  low_bulk_ess = all_bulk_ess[all_bulk_ess < good_ess]
  n_low_bulk_ess = length(low_bulk_ess)
  return(n_low_bulk_ess)
}

get_prop_bulkess <- function(modelfit){
  # number of model parameters 
  n_model_variables = brms::nvariables(modelfit)
  # good bulk ESS: # of chains * 100
  good_ess = brms::nchains(modelfit) * 100
  # get all bulk ESS's
  sm = summary(modelfit)
  bulk_ess_fixed = sm$fixed$Bulk_ESS
  bulk_ess_spec = sm$spec_pars$Bulk_ESS
  all_bulk_ess = c(bulk_ess_fixed, bulk_ess_spec)
  low_bulk_ess = all_bulk_ess[all_bulk_ess < good_ess]
  n_low_bulk_ess = length(low_bulk_ess)
  # proportion of too low bulk ESS
  prop_low_bulk_ess = n_low_bulk_ess / n_model_variables
  return(prop_low_bulk_ess)
}

# tail ESS ####
get_n_low_tailess <- function(modelfit){
  # number of model parameters 
  n_model_variables = brms::nvariables(modelfit)
  # good tail ESS: # of chains * 100
  good_ess = brms::nchains(modelfit) * 100
  # get all tail ESS's
  sm = summary(modelfit)
  tail_ess_fixed = sm$fixed$Tail_ESS
  tail_ess_spec = sm$spec_pars$Tail_ESS
  all_tail_ess = c(tail_ess_fixed, tail_ess_spec)
  low_tail_ess = all_tail_ess[all_tail_ess < good_ess]
  n_low_tail_ess = length(low_tail_ess)
  return(n_low_tail_ess)
}

get_prop_tailess <- function(modelfit){
  # number of model parameters 
  n_model_variables = brms::nvariables(modelfit)
  # good tail ESS: # of chains * 100
  good_ess = brms::nchains(modelfit) * 100
  # get all tail ESS's
  sm = summary(modelfit)
  tail_ess_fixed = sm$fixed$Tail_ESS
  tail_ess_spec = sm$spec_pars$Tail_ESS
  all_tail_ess = c(tail_ess_fixed, tail_ess_spec)
  low_tail_ess = all_tail_ess[all_tail_ess < good_ess]
  n_low_tail_ess = length(low_tail_ess)
  # proportion of too low bulk ESS
  prop_low_tail_ess = n_low_tail_ess / n_model_variables
  return(prop_low_tail_ess)
}