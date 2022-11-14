################################################################################
# function that creates and stores metrics for one model 
# (i.e. for one row of the df of models/universes)

evaluate_universe <- function(modelobject, dataset){
  # function requires custom functions check_rhats(), extract_div_trans()
  # builds the dictionary row cell by cell for one model
  
  m_dict_row <- cbind(
    # rhats 
    # number of rhats -> model parameters (model parameters)
    n_rhats = length(brms::rhat(modelobject)), 
    # parameter names for which rhat is > 1.01 (model)
    highrhats = check_rhats(brms::rhat(modelobject)), 
    # count the number of rhats > 1.01 (model)
    flag_rhats = length(unlist(check_rhats(brms::rhat(modelobject)))), 
    # high rhat for treatment
    high_rhat_trt = ifelse(TRUE %in% str_detect(unlist(check_rhats(brms::rhat(modelobject))), regex("b_Trt1", ignore_case = TRUE)), "yes", "no"), 
    # extract divergent transitions 
    divtrans = extract_div_trans(brms::nuts_params(modelobject)),
    # n_eff: what is good? what problematic?
    neffs = list(brms::neff_ratio(modelobject))
  )
  
  # get summary once 
  #mod_summary <- summary(modelobject)
  
  #m_dict_row <- m_dict_row %>% cbind(
  # use model summary for bulk ESS, tail ESS etc. and aggregate information?
  #bulkess = mod_summary$spec_pars$Bulk_ESS,
  #tailess = mod_summary$spec_pars$Tail_ESS
  # use model summary for bulk ESS, tail ESS 
  #)
  
  # get priorsense results once 
  ps_df <- priorsense::powerscale_sensitivity(modelobject)
  
  m_dict_row <- m_dict_row %>% cbind(
    # priorsense
    ps_params = list(filter(ps_df$sensitivity, diagnosis != "-")), # which parameters are showing issues?
    priordataconf = unlist(count(filter(ps_df$sensitivity, diagnosis == "prior-data conflict")))
  )
  
  # PPC, needs models + data
  dat <- brms::epilepsy
  outcome <- dat$count
  # posterior predictions 
  yrep = posterior_predict(modelobject)
  # mean of posterior predictions 
  mean_yrep = colMeans(yrep)
  # median of posterior predictions (e.g., useful for discrete data)
  median_yrep = apply(yrep, 2, median)
  # Wasserstein distance 
  dist_ws = wasserstein1d(outcome, median_yrep)
  # loo, always set moment_match = TRUE? 
  results_loo = loo(modelobject, moment_match = TRUE)
  modelobjectfit <- modelobject$fit
  
  # loo results 
  m_dict_loo <- cbind(
    flag_pareto_ks = sum(results_loo$diagnostics$pareto_k > 0.7),
    elpd_loo = results_loo$estimates[1],
    se_elpd_loo = results_loo$estimates[4],
    nparams = length(colnames(as.matrix(modelobjectfit))), 
    nvars = brms::nvariables(modelobject),
    p_loo = results_loo$estimates[2],
    se_p_loo = results_loo$estimates[5]
  )
  
  # cbind all together 
  m_dict_row <- m_dict_row %>% cbind(
    dist_ws,
    m_dict_loo
  )
  
  # return one named row vector with all results 
  out <- as.data.frame(m_dict_row)
}