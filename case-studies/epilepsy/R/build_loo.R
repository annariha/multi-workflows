# loo: elpd and model comparison ####

build_loo <- function(row, ...){
  # print(build_name(row))
  file_name = paste0(digest::digest(build_name(row), algo="md5"), "_loo.rds")
  if(file.exists(file_name)){
    return(readRDS(file_name))
  }else{
    rv = loo(build_fit(row), model_names=c(build_name(row)))
    saveRDS(rv, file_name)
    return(rv)
  }
} 

build_loo_rstan <- function(row, ...){
  file_name = paste0(digest::digest(build_name(row), algo="md5"), "_loo_rstan.rds")
  rv = loo(build_fit_rstan(row), model_names=c(build_name(row)), moment_match = TRUE)
  saveRDS(rv, file_name)
  return(rv)
} 