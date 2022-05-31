# helper functions (from Andrew's code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# adjust extract_variables from multiverse to work with other objects 
extract_vars_df <- function (x, ..., .results = .results){
  .results <- enquo(.results)
  mutate(x, extracted = lapply(!!.results, multiverse:::extract_from_env, ...)) %>%
    # added the below code instead of using unnest_wider()
    # from https://stackoverflow.com/questions/49689927/unnest-a-list-column-directly-into-several-columns
    mutate(r = map(extracted, ~ data.frame(t(.)))) %>%
    unnest(r) %>%
    select(-extracted)
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
      posterior::variables(brm_draws) <- stringr::str_split(posterior::variables(brm_draws), "_", simplify = T)[, 2]
      posterior::rename_variables(brm_draws, `(Intercept)` = Intercept)
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
