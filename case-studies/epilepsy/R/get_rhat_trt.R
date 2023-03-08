# check Rhat for "b_Trt1" (and "b_zBase:Trt1" if present)
get_rhat_trt <- function(modelfit){
  rhats <- brms::rhat(modelfit)
  # find "b_Trt1" (and if present "b_zBase:Trt1")
  rhat_trt <- rhats[str_detect(names(rhats), regex("Trt1", ignore_case = TRUE))]
  rhat_trt <- as.numeric(rhat_trt[1])
  return(rhat_trt)
}
