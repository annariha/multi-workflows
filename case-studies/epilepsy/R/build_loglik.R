#! /usr/bin/Rscript --vanilla

# include Poisson and neg. Binomial families
build_loglik_2 <- function(row, ...){
  
  # get model fit
  # modelfit = build_fit(row, ...)
  modelfit = row[["modelfits"]][[1]]
  # get posterior draws
  # draws_df = posterior::as_draws_df(modelfit)
  draws_df = row[["draws_df"]][[1]]
  # extract outcome 
  outcome_name = row[["outcome"]]
  outcome = as.numeric(unlist(modelfit$data[outcome_name]))
  # empty matrix to build loglik array lateron
  log_lik = matrix(data=NA, nrow=brms::nchains(modelfit)*brms::niterations(modelfit), ncol=NROW(modelfit$data))
  
  # two ways of computing loglik depending on obs family ####
  if (unlist(modelfit$family)$family == "poisson"){
  # reformat draws to get z's and sd ####
    input_df <- draws_df |> 
      tidyr::nest(rs = starts_with("r_obs"),
                sd = matches("sd_obs__Intercept")) |>
      mutate(rs = map(rs, unlist),
             sd = map_dbl(sd, ~matrix(unlist(.x), ncol = 1))) |>
      rowwise() |>
      mutate(zs = list(unlist(rs) / sd))
  
    # extract linpred ####
    # from brms docs: "[posterior] draws before applying any link functions or other transformations"
    lin_pred = brms::posterior_linpred(modelfit)
    # standardized group-level effects
    zs_df = data.frame(matrix(unlist(input_df$zs), ncol=NROW(modelfit$data), byrow=T))
    # actual group-level effects
    rs_df = data.frame(matrix(unlist(input_df$rs), ncol=NROW(modelfit$data), byrow=T)) 
    lin_pred_without = lin_pred - rs_df # different values across iterations, same value for each obs
  
    # results for all observations and iterations with integrate() ####
    for (i in seq(NROW(input_df))){
      for (j in seq(NROW(modelfit$data))){
        zs <- zs_df[i,j]
        sd_obs <- input_df$sd[i]
        linpreds_minus_re <- lin_pred_without[i,j]
        y <- as.numeric(outcome[j])
        integrand <- function(zs, 
                              sd_obs,
                              y, 
                              linpreds_minus_re){
          # function defines integrand for integrate()
          # in Stan code: std_normal_lpdf(z_1)
          z_term <- dnorm(zs,
                          mean = 0, 
                          sd = 1,
                          log = TRUE)
          # in Stan code: poisson_log_lpmf(Yi[1] | r_1_1 + linpred_minus_re)
          fit_term <- dpois(x = y, 
                            lambda = exp((zs*sd_obs)  + linpreds_minus_re),
                            log = TRUE)
          result = exp(z_term + fit_term)
          return(result)
        }
        #print(paste0("Iteration: ", i, " Observation: ", j, " sd_obs: ", sd_obs, " linpreds: ", linpreds_minus_re, " y: ", y))
        log_lik[i,j] <- log(integrate(integrand, 
                                      lower = -Inf, #try: -9, -6 for faster comp. since tails of N(.,.) go down fast 
                                      upper = Inf, #try: 9, 6
                                      sd_obs = sd_obs,
                                      y = y,
                                      linpreds_minus_re = linpreds_minus_re)$value)
      }
    }
  } else if (unlist(modelfit$family)$family == "negbinomial") {
    # reformat draws to get z's and sd ####
    input_df <- draws_df |> 
      tidyr::nest(rs = starts_with("r_obs"),
                  sd = matches("sd_obs__Intercept"), 
                  shape = matches("shape")) |>
      mutate(rs = map(rs, unlist),
             sd = map_dbl(sd, ~matrix(unlist(.x), ncol = 1)),
             shape = map_dbl(shape, ~matrix(unlist(.x), ncol = 1))) |>
      rowwise() |>
      mutate(zs = list(unlist(rs) / sd))
    
    # extract linpred ####
    # from brms docs: "[posterior] draws before applying any link functions or other transformations"
    lin_pred = brms::posterior_linpred(modelfit)
    # standardized group-level effects
    zs_df = data.frame(matrix(unlist(input_df$zs), ncol=NROW(modelfit$data), byrow=T))
    # actual group-level effects
    rs_df = data.frame(matrix(unlist(input_df$rs), ncol=NROW(modelfit$data), byrow=T)) 
    lin_pred_without = lin_pred - rs_df # different values across iterations, same value for each obs
  
    # results for all observations and iterations with integrate() ####
    for (i in seq(NROW(input_df))){
      for (j in seq(NROW(modelfit$data))){
        zs <- zs_df[i,j]
        sd_obs <- input_df$sd[i]
        shape <- input_df$shape[i]
        linpreds_minus_re <- lin_pred_without[i,j]
        y <- as.numeric(outcome[j])
        integrand <- function(zs, 
                              sd_obs,
                              shape,
                              y, 
                              linpreds_minus_re){
          # function defines integrand for integrate()
          # in Stan code: std_normal_lpdf(z_1)
          z_term <- dnorm(zs,
                          mean = 0, 
                          sd = 1,
                          log = TRUE)
          # in Stan code: mu[n] += r_1_1[J_1[n]] * Z_1_1[n] 
          # in Stan code: neg_binomial_2_log_glm_lpmf(Y | Xc, mu, b, shape)
          # inverse overdispersion parameter phi = shape?
          # using https://stat.ethz.ch/R-manual/R-devel/library/stats/html/NegBinomial.html
          fit_term <- dnbinom(x = y, 
                              size = shape, #dispersion parameter
                              mu = exp((zs*sd_obs) + linpreds_minus_re), 
                              log = TRUE)
          result = exp(z_term + fit_term)
          return(result)
        }
        #print(paste0("Iteration: ", i, " Observation: ", j, " sd_obs: ", sd_obs, " shape: ", shape, " linpreds: ", linpreds_minus_re, " y: ", y))
        log_lik[i,j] <- log(integrate(integrand, 
                                      lower = -Inf,
                                      upper = Inf,
                                      sd_obs = sd_obs,
                                      shape = shape,
                                      y = y,
                                      linpreds_minus_re = linpreds_minus_re)$value)
      }
    }
  }
  # add names to matrix 
  colnames(log_lik) <- paste0("log_lik[", seq(NROW(modelfit$data)), "]")
  # convert matrix of log_lik values to array
  log_lik_array <- array(log_lik, c(brms::niterations(modelfit), brms::nchains(modelfit), NROW(modelfit$data)))
  # set dimnames of array
  dimnames(log_lik_array) <- list(iteration = seq(brms::niterations(modelfit)),
                                  chain = seq(brms::nchains(modelfit)),
                                  variable =  paste0("log_lik[", seq(NROW(modelfit$data)), "]"))
  # convert into draws array
  log_lik_array <- posterior::as_draws(log_lik_array)
  # return loglik array
  return(log_lik_array)
}