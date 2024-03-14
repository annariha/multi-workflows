# function that takes in df of model combinations with modelfits and loo objects for all models
# Input: model combinations dataframe (model_combs_df) and loo object 
# Output: comparison dataframe with # high Pareto khats, MCSE, LOO-BB (=PBMA+) and PBMA weights 

build_comparison_df <- function(models_combinations, loos_object){
  # Input: model combinations dataframe (model_combs_df) and loo object 
  # Output: comparison dataframe with # high Pareto khats, MCSE, LOO-BB (=PBMA+) and PBMA weights 
  
  # compare models with loo ####
  comparison_df <- loo::loo_compare(loos_object)
  
  # add sum of Pareto k's > 0.7 for all models with default LOO ####
  comparison_df <- merge(comparison_df, 
                         purrr::map_dbl(purrr::map(loos_object, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)), 
                         by="row.names") 
  # set rownames to model names for merging
  rownames(comparison_df) <- comparison_df$Row.names
  # select everything despite Row.names
  comparison_df <- comparison_df[2:length(comparison_df)]
  # set descriptive name for new column 
  colnames(comparison_df)[ncol(comparison_df)] <- "n_high_pareto_ks"
  
  # add MCSE of elpd for all models with default LOO ####
  comparison_df <- merge(comparison_df, 
                         purrr::map_dbl(loos_object, loo::mcse_loo), 
                         by="row.names") 
  # set rownames to model names for merging
  rownames(comparison_df) <- comparison_df$Row.names
  # select everything despite Row.names
  comparison_df <- comparison_df[2:length(comparison_df)]
  # set descriptive name for new column 
  colnames(comparison_df)[ncol(comparison_df)] <- "mcse_elpd_loo"
  
  # add loo comparison table with default LOO ####
  full_df = merge(models_combinations, comparison_df, by=0)
  # set row names to model names
  rownames(full_df) <- full_df$Row.names
  # select everything despite Row.names
  full_df = full_df[2:length(full_df)]
  
  # add LOO-BB and PBMA weights with default LOO ####
  # likely to be better, based on experiments improves model averaging, less weights close to zero and 1 
  loo_bb_weights_default = loo_model_weights(loos_object, method="pseudobma")
  # likely to be more extreme compared to BB=TRUE, when BB is FALSE we should get same ranking order
  pbma_weights_default = loo_model_weights(loos_object, method="pseudobma", BB = FALSE)
  # joint df for both weights 
  pbma_df = data.frame(loo_bb_weight=as.numeric(loo_bb_weights_default),
                       pbma_weight = as.numeric(pbma_weights_default),
                       row.names=names(pbma_weights_default))
  
  # add weights with default LOO
  full_comparisons_df = merge(full_df, pbma_df, by=0)
  # set row names to model names (again) 
  rownames(full_comparisons_df) <- full_comparisons_df$Row.names
  # select everything despite Row.names
  full_comparisons_df = full_comparisons_df[2:length(full_comparisons_df)]
  # return the df with comparisons, # high Pareto khats, MCSE, LOO-BB and PBMA weights
  return(full_comparisons_df)
}