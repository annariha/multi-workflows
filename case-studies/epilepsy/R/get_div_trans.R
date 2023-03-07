################################################################################
# extract the number of divergent transitions ####
# "if you get only few divergences and you get good Rhat and ESS values, 
# the resulting posterior is often good enough to move forward"

get_div_trans <- function(modelfit){
  params_df <- brms::nuts_params(modelfit)
  # from brms documentation
  number_div_trans <- sum(subset(params_df, Parameter == "divergent__")$Value)
  return(number_div_trans)
}