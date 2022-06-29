# helper functions (from Andrew's code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# adjust extract_variables from multiverse to work with other objects 
extract_vars_df <- function (x, ..., .results = .results){
  .results <- enquo(.results)
  mutate(x, extracted = lapply(!!.results, multiverse:::extract_from_env, ...)) %>%
    # added the below code instead of using unnest_wider()
    # adapted from https://stackoverflow.com/questions/49689927/unnest-a-list-column-directly-into-several-columns
    mutate(r = map(extracted, ~ data.frame(t(.)))) %>%
    unnest(r) %>%
    select(-extracted)
}

# extract convergence diagnostics
check_rhats <- function(named_vec){
  # from https://mc-stan.org/misc/warnings.html; # "only fully trust the sample if R-hat is less than 1.01"
  # > 1.05 e.g. 3/5 parameters without convergence issues - a, c had issues 
  out <- names(named_vec[named_vec > 1.01])
  return(out)
}

# "check also Monte Carlo standard error for the quantities of interest and 
# compare that to the domain knowledge of the required accuracy, and if it is 
# low, then run longer chains to get more samples from the posterior."

# extract the number of divergent transitions
# "if you get only few divergences and you get good Rhat and ESS values, 
# the resulting posterior is often good enough to move forward"
extract_div_trans <- function(df){
  # from brms documentation
  sum(subset(df, Parameter == "divergent__")$Value)
}

# "effective sample size (ESS) of a quantity of interest captures how many 
# independent draws contain the same amount of information as the dependent 
# sample obtained by the MCMC algorithm. The higher the ESS the better. [...]
# For final results, we recommend requiring that the bulk-ESS is greater than 
# 100 times the number of chains. For example, when running four chains, this 
# corresponds to having a rank-normalized effective sample size of at least 400. 
# In early workflow, ESS > 20 is often sufficient." 

check_bulkess <- function(modelfit){
  nc <- nchains(modelfit)
  bulkess <- posterior::ess_bulk(modelfit)
  # flag if bulk ESS < 100 * number of chains
  out <- bulkess[bulkess < 100 * nc] 
}

evaluate_multiverse <- function(multiverse, mod, outcome){
  # to pass column names of multiverse object without quotation marks
  mod <- deparse(substitute(mod))
  # evaluate and extract 
  m_dict <- multiverse %>%
    mutate(rhats = purrr::map(multi[[mod]], brms::rhat), 
           flagrhats = purrr::map(purrr::map(multi[[mod]], brms::rhat), check_rhats),
           flag_rhats = purrr::map_dbl(flagrhats, length), 
           neffs = purrr::map(multi[[mod]], brms::neff_ratio),
           divtrans = purrr::map_dbl(purrr::map(multi[[mod]], brms::nuts_params), extract_div_trans),
           bulkess = purrr::map_dbl(multi[[mod]], posterior::ess_bulk),
           flagbulkess = purrr::map(multi[[mod]], check_bulkess),
           flag_bulkess = if_else(purrr::map_dbl(multi[[mod]], posterior::ess_bulk) < 100 * 4, "too low", "ok"),
           # priorsense
           ps_df_params = purrr::map(purrr::map(multi[[mod]], priorsense::powerscale_sensitivity), "sensitivity"),
           ps_params = purrr::map(ps_df_params, ~filter(.x, diagnosis != "-")),
           pdc = purrr::map(purrr::map(ps_df_params, ~filter(.x, diagnosis == "prior-data conflict")), count) %>% unlist(),
           # PPC
           yrep = purrr::map(multi[[mod]], posterior_predict),
           mean_yrep = purrr::map(yrep, colMeans),
           dist_ws = purrr::map(mean_yrep, wasserstein1d, a = outcome) %>% unlist(), # this line needs data
           # loo 
           results_loo = purrr::map(multi[[mod]], loo),
           flag_pareto_ks = purrr::map_dbl(purrr::map(purrr::map(loo_results, "diagnostics"), "pareto_k"), ~sum(.x > 0.7)),
           elpd_loo = purrr::map_dbl(purrr::map(results_loo, "estimates"), 1),
           se_elpd_loo = purrr::map_dbl(purrr::map(results_loo, "estimates"), 4),
           p_loo = purrr::map_dbl(purrr::map(results_loo, "estimates"), 2),
           se_p_loo = purrr::map_dbl(purrr::map(results_loo, "estimates"), 5)) %>%
    select(!c(.parameter_assignment, 
              .code, 
              .results, 
              mod, 
              rhats, 
              neffs, 
              ps_df_params, 
              ps_params, 
              yrep, 
              mean_yrep,
              results_loo))
  out <- m_dict
}

# compare posteriors of several models visually
# code adapted from https://stackoverflow.com/questions/52875665/plotting-posterior-parameter-estimates-from-multiple-models-with-bayesplot
# new: input can be one, more or a list of fit-objects, option to exclude vars from plot

compare_posteriors <- function(x, ... , dropvars = c(), dodge_width = 0.5) {
  # put "dots" in list 
  dots <- rlang::dots_list(..., .named = TRUE) 
  # coerce if x is one fit-object and more objects are in "dots"
  # otherwise take list of fit-objects 
  if (class(x) != "list"){
    fits <- c(list(x), dots) 
  } else {
    fits <- x
  }
  
  # process fit-objects to get draw-arrays 
  draws <- lapply(fits, function(x) {
    if (class(x)[1] == "stanreg") {
      posterior::subset_draws(posterior::as_draws(x$stanfit),
                              variable = names(fixef(x))
      )
    } else if (class(x)[1] == "brmsfit") {
      brm_draws <- posterior::subset_draws(posterior::as_draws(x$fit),
                                           variable = paste0("b_", rownames(fixef(x))))
      posterior::variables(brm_draws) <- stringr::str_remove(posterior::variables(brm_draws), "b_")
      posterior::rename_variables(brm_draws, `(Intercept)` = Intercept) # if this line is commented out -> bayesplot error
    } else {
      stop(paste0(class(x)[1], " objects not supported."))
    }
  })
  
  intervals <- lapply(draws, bayesplot::mcmc_intervals_data)
  # names: 1,2,3,... but names could be sth. more descriptive
  names(intervals) <- 1:length(intervals) 
  combined <- dplyr::bind_rows(intervals, .id = "model") %>%
    mutate(model = factor(model, levels = unique(as.character(model)))) # get proper factors from list names
  
  # option to exclude vars
  combined <- combined %>%
       filter(!parameter %in% dropvars)
  
  # plot
  ggplot(combined, aes(x = m, y = parameter, color = model, group = model)) +
    geom_linerange(aes(xmin = l, xmax = h), size = 2, position = position_dodge(dodge_width)) +
    geom_linerange(aes(xmin = ll, xmax = hh), position = position_dodge(dodge_width)) +
    geom_point(color = "black", position = position_dodge(dodge_width), size = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed")
}

