# helper functions (from Andrew's code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# compare posteriors visually
# code slightly adapted from https://stackoverflow.com/questions/52875665/plotting-posterior-parameter-estimates-from-multiple-models-with-bayesplot
# new: input can be a list of brmsfit-objects 

compare_posteriors <- function(..., dodge_width = 0.5) {
  dots <- rlang::dots_list(..., .named = TRUE)
  draws <- lapply(dots, function(x) {
    if (class(x)[1] == "stanreg") {
      posterior::subset_draws(posterior::as_draws(x$stanfit),
                              variable = names(fixef(x))
      )
    } else if (class(x)[1] == "brmsfit") {
      brm_draws <- posterior::subset_draws(posterior::as_draws(x$fit),
                                           variable = paste0("b_", rownames(fixef(x)))
      )
      posterior::variables(brm_draws) <- stringr::str_split(posterior::variables(brm_draws), "_", simplify = T)[, 2]
      posterior::rename_variables(brm_draws, `(Intercept)` = Intercept)
    } else {
      stop(paste0(class(x)[1], " objects not supported."))
    }
  })
  intervals <- lapply(draws, bayesplot::mcmc_intervals_data)
  names(intervals) <- 1:length(intervals) # 1, 2, 3 etc but names could also be sth else
  combined <- dplyr::bind_rows(intervals, .id = "model") %>%
    mutate(model = factor(model, levels = unique(as.character(model)))) # get proper factors from list names
  ggplot(combined, aes(x = m, y = parameter, color = model, group = model)) +
    geom_linerange(aes(xmin = l, xmax = h), size = 2, position = position_dodge(dodge_width)) +
    geom_linerange(aes(xmin = ll, xmax = hh), position = position_dodge(dodge_width)) +
    geom_point(color = "black", position = position_dodge(dodge_width), size = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed")
}
