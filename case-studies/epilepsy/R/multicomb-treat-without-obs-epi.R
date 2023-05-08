#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, future, furrr, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr,
               ggdendro, cowplot)

# run once
# cmdstanr::install_cmdstan()

source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))
source(here::here("case-studies", "epilepsy", "R", "build_fit.R"))
source(here::here("case-studies", "epilepsy", "R", "get_rhat_trt.R"))
source(here::here("case-studies", "epilepsy", "R", "get_div_trans.R"))
source(here::here("case-studies", "epilepsy", "R", "build_loo.R"))

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load data ####

dat <- brms::epilepsy 
outcome_str <- "count"

# create dataframe of combinations of model components ####

# observation families
families <- list(poisson = poisson(), 
                 negbinomial = negbinomial())

# priors 
priors <- list(brms_default = NULL, 
               brms_horseshoe = set_prior("horseshoe(3)")
)

combinations_df <- expand.grid(
  family = names(families),
  prior = priors,
  # fixed effects 
  Trt = c("", "Trt"), 
  zBase = c("", "zBase"),
  zAge = c("", "zAge"),
  # random effects, no observation level r.e.
  patient = c("", "(1 | patient)"),
  visit = c("", "(1 | visit)")
)

combinations_df <- combinations_df |> 
  # add interaction effect in the rows where treatment was left out, (i.e., where Trt == "")
  mutate(zBaseTrt = factor(
    case_when(
      Trt == "Trt" ~ "",
      Trt == "" ~ "zBase * Trt"))) |> 
  # filter out rows with interaction and zBase
  filter(!(zBaseTrt == "zBase * Trt" & combinations_df$zBase == "zBase"))

combinations_df <- combinations_df |>  
  # add outcome name 
  mutate(outcome = rep_len(outcome_str, NROW(combinations_df))) |>
  # add prior names for easier summarising, plotting etc. 
  mutate(priors = names(combinations_df$prior)) |>
  # reorder to have outcome name, family and treatment effects first 
  select(outcome, family, priors, prior, Trt, zBaseTrt, everything())

# set row names to model names, needed for plotting
rownames(combinations_df) <- apply(combinations_df, 1, build_name)

# store combinations dataframe to use in other scripts 
write_rds(combinations_df, here::here("case-studies", "epilepsy", "data", "prelim", "combinations_df_without_obs.rds"))

# add model name and brms formula for each combination ####
comb_df <- combinations_df |> 
  mutate(
    model_name = apply(combinations_df, 1, build_name), 
    formula = apply(combinations_df, 1, build_brms_formula)) 

# make stancode for each model ####
helper_data <- comb_df |>
  select(formula, family, prior) |>
  mutate(data = list(brms::epilepsy)) |>
  mutate(save_model = here::here("case-studies", "epilepsy", "Stan", paste0("model_", 1:NROW(data), ".stan")))

# create stancode & save ####
purrr::pmap(helper_data, make_stancode)

# model fit with cmdstanr ####
#cmdstan_model(stan_file = poisson_re_int)

# data for cmdstanr ####
# samples with cmdstanr ####

# add model fits for each combination ####
tic()
comb_df <- comb_df |> 
  mutate(
    model_fit = apply(combinations_df, 1, build_fit)) 
toc()

tic()
test <- combinations_df %>%
  split(1:nrow(.)) %>%
  purrr::map(build_fit)
toc()

# get all modelfits 
# models <- apply(combinations_df, 1, build_fit)

# This gives the same result as using apply(...) 
#models <- combinations_df |>
#  group_nest(row_number()) |>
#  pull(data) |>
#  purrr::map(build_fit)


# 2nd step: computation ####

# divergent transitions
comb_df <- comb_df |>
  mutate(div_trans = purrr::map_dbl(model_fit, get_div_trans))

# models without divergent transitions ####
comb_df_filtered <- comb_df |>
  filter(div_trans == 0) 

# get all rhats 
# rhats = purrr::map(model_fit, brms::rhat)

# high Rhat for treatment ####
comb_df <- comb_df |> 
  mutate(
    # get rhats for treatment (i.e., only Rhats of "b_Trt1")
    rhats_raw = purrr::map_dbl(model_fit, get_rhat_trt))

# add an indicator for computational issues ####
comb_df  <- comb_df |>
  mutate(no_issues = ifelse(div_trans == 0 & rhats_raw <= 1.01, 1, 0))

# store comb dataframe to use in other scripts 
write_rds(comb_df, here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_without_obs.rds"))

# reduce df for plotting 
comb_df_for_plots_filter_comp_issues <- comb_df |>
  filter(div_trans == 0) |>
  filter(rhats_raw < 1.01) |>
  select(model_name, model_fit, visit, div_trans, rhats_raw)

# for plotting 
write_rds(comb_df_for_plots_filter_comp_issues, here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_for_plots_filter_comp_issues_without_obs.rds"))

# 1st step: elpd all models ####

# model comparison with loo elpd 
# loo() works with cmdstanr object but moment matching not possible -> dev version?
# loo(., moment_match = TRUE) requires backend = rstan & save_pars = save_pars(all = TRUE) in brm()
# integrated loo

# get loo 
tic()
loos <- apply(combinations_df, 1, build_loo)
toc()

write_rds(loos, here::here("case-studies", "epilepsy", "data", "prelim", "loo_df_all_epi.rds"))
loos <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "loo_df_all_epi.rds"))

# get high Pareto k's ####

# which models have Pareto k's > 0.7?
get_sum_high_ks <- function(x, ...){
  # x is a vector of pareto k's
  result = sum(x > 0.7)
  return(result)
}
# extract Pareto k's for all models
pareto_ks <- purrr::map(loos, ~.x$diagnostics$pareto_k)
# sum of high Pareto k's for each model
sum_high_pareto_ks <- purrr::map_dbl(pareto_ks, get_sum_high_ks)
# modelname and number of high Pareto k's
high_pareto_ks <- tibble(model_name = names(sum_high_pareto_ks), sum_high_pareto_ks = as.numeric(sum_high_pareto_ks)) 

# modelname, loos, Pareto k's
loo_df <- tibble(model_name = names(loos), loos = loos) |>
  mutate(sum_high_pareto_ks = purrr::map_dbl(purrr::map(loos, ~.x$diagnostics$pareto_k), get_sum_high_ks))

# all loos with number of bad Pareto k's < 5% of obs 
loo_df_without_5 <- loo_df |>
  filter(sum_high_pareto_ks < (NROW(dat) / 100) * 5)

# store intermediate result
write_rds(full_df, here::here("case-studies", "epilepsy", "data", "prelim", "loo_df_without_5_epi.rds"))
loo_df_without_5 <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "loo_df_without_5_epi.rds"))

# compare models with loo & model averaging weights for the selected models ####
comparison_df = loo::loo_compare(loo_df_without_5$loos)
# add loo comparison table 
full_df = merge(comb_df, comparison_df, by=0)
# set row names to model names
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# extract pseudo-BMA weights for the above models ####
pbma_weights = loo_model_weights(loo_df_without_5$loos, method="pseudobma")
pbma_df = data.frame(pbma_weight=as.numeric(pbma_weights), row.names=names(pbma_weights))
full_df = merge(full_df, pbma_df, by=0)
# set row names to model names (again) 
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# store intermediate result
write_rds(full_df, here::here("case-studies", "epilepsy", "data", "prelim", "comb_loo_df_without_95_epi.rds"))
full_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_loo_df_without_95_epi.rds"))

# all loos with number of bda Pareto k's == 0

# compare models with loo & model averaging weights for the selected models ####
comparison_df = loo::loo_compare(loo_df_without_5$loos)
# add loo comparison table 
full_df = merge(comb_df, comparison_df, by=0)
# set row names to model names
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# extract pseudo-BMA weights for the above models ####
pbma_weights = loo_model_weights(loo_df_without_5$loos, method="pseudobma")
pbma_df = data.frame(pbma_weight=as.numeric(pbma_weights), row.names=names(pbma_weights))
full_df = merge(full_df, pbma_df, by=0)
# set row names to model names (again) 
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# store intermediate result
write_rds(full_df, here::here("case-studies", "epilepsy", "data", "prelim", "comb_loo_df_without_95_epi.rds"))
full_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_loo_df_without_95_epi.rds"))

# reduce df for plotting 
comb_df_for_plots <- full_df |>
  filter(pbma_weight > 0.01) |>
  filter(abs(elpd_diff) < 4) |>
  filter(no_issues == 1) |>
  arrange(desc(elpd_diff)) |>
  select(model_name, model_fit, no_issues, elpd_diff, se_diff, pbma_weight)

# for plotting 
write_rds(comb_df_for_plots, here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_for_plots_without_obs_epi.rds"))

write_rds(comb_df_for_plots, here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_for_plots_minimal_without_obs_epi.rds"))

# compute PBMA weights for minimal set of models ####
loo_df_minimal <- full_df |>
  filter(pbma_weight > 0.01) |>
  filter(abs(elpd_diff) < 4) |>
  filter(no_issues == 1) |>
pbma_weights_minimal <- loo_model_weights(loo_df_without_95$loos, method="pseudobma")
pbma_df_minimal = data.frame(pbma_weight=as.numeric(pbma_weights_minimal), row.names=names(pbma_weights_minimal))

# Which models need moment matching (or integrated loo)? ####
# approx. 5% too high -> 12 or more bad values 

loo_df |>
  filter(sum_high_pareto_ks >= (NROW(dat) / 100) * 5)

# model 95 has issues 
# Error in mm_list[[ii]]$i : $ operator is invalid for atomic vectors
#In addition: Warning message:
#  In parallel::mclapply(X = I, mc.cores = cores, FUN = function(i) loo_moment_match_i_fun(i)) :
#  scheduled core 5 encountered error in user code, all values of the job will be affected
#Error: Moment matching failed. Perhaps you did not set 'save_pars = save_pars(all = TRUE)' when fitting your model?

# extra loo calculation for model 95
model_fit_95 <- build_fit_rstan(combinations_df[95,])
loo_95 <- build_loo_rstan(model_fit_95)
# this gives a weird error 

# moment matching: 14 problematic 
loo_95_mm <- loo(model_fit_95, model_names=c(build_name(combinations_df[95,])), moment_match = TRUE)
# still 14 obs problematic, recommends kfold()

# k-fold 
library(future)
plan(multisession)
kfold_95 <- kfold(model_fit_95, chains = 1)
write_rds(kfold_95, here::here("case-studies", "epilepsy", "data", "prelim", "kfold_95_without_obs.rds"))

# integrated loo

model_cmdstanr$init_model_methods()

# get loo 
tic()
loos_rstan <- apply(combinations_df, build_loo_rstan)
toc()

tic()
test <- combinations_df %>%
  split(1:nrow(.)) %>%
  purrr::map(build_loo_rstan)
toc()

library(future)
plan(multisession)

tic()
test <- combinations_df %>%
  split(1:nrow(.)) %>%
  furrr::future_map(build_loo_rstan)
toc()

# moment matching for models with high pareto k's 
test <- combinations_df[95,]
build_loo_rstan(test)
build_loo(test)

# compare models with loo & model averaging weights ####
comparison_df = loo::loo_compare(loos)

# get number of model parameters to compare with p_loo ####
comb_df <- comb_df |> 
  mutate(nparams = purrr::map_dbl(model_fit, brms::nvariables))

# extract pseudo-BMA weights
pbma_weights = loo_model_weights(loos, method="pseudobma")
pbma_df = data.frame(pbma_weight=as.numeric(pbma_weights), row.names=names(pbma_weights))

# extract stacking weights
stack_weights = loo_model_weights(loos, method="stacking")
stack_df = data.frame(stack_weight=as.numeric(stack_weights), row.names=names(stack_weights))

# add loo comparison table 
full_df = merge(combinations_df, comparison_df, by=0)
# set row names to model names
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# add sum of pareto k's over 0.7
full_df = merge(full_df, high_pareto_ks, by=0)
# set row names to model names
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# merge pseudo BMA weights 
full_df = merge(full_df, pbma_df, by=0)
# set row names to model names (again) 
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]
# merge stacking weights 
full_df = merge(full_df, stack_df, by=0)
# set row names to model names (again) 
rownames(full_df) <- full_df$Row.names
# select everything despite Row.names
full_df = full_df[2:length(full_df)]

# add posterior results for treatment ####
get_posterior_treat <- function(row){
  modelfit = build_fit(row)
  draws_df = posterior::as_draws_df(modelfit)
  draws_trt = draws_df$b_Trt1
  return(draws_trt)
}

get_posterior_treat2 <- function(row){
  modelfit = build_fit(row)
  draws_trt = spread_draws(modelfit, b_Trt1)
  return(draws_trt)
}
spread_draws(mod, b_Trt1)

treatment_sampless = apply(combinations_df, 1, get_posterior_treat)

test_row <- 
  test_fit <- build_fit(combinations_df[1,])

full_df = cbind(full_df, 
                model_name=model_names,
                treatment_mean=as.numeric(lapply(treatment_sampless, mean)),
                treatment_se=as.numeric(lapply(treatment_sampless, sd)),
                treatment_q05=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.05, names=FALSE))),
                treatment_q25=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.25, names=FALSE))),
                treatment_q75=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.75, names=FALSE))),
                treatment_q95=as.numeric(lapply(treatment_sampless, partial(quantile, probs=.95, names=FALSE)))
)

# Plots: visual inspection of pbma weights ####

plot_ordered_pbmaw <- pbma_df |> 
  arrange(pbma_weight) |>
  mutate(models = forcats::fct_inorder(rownames(pbma_df))) |> 
  ggplot(aes(x = pbma_weight, y = models)) + 
  geom_point(shape=21, size=2) +
  geom_vline(xintercept = 0, linetype="dotted") +
  ggtitle(paste0("All models (k=", NROW(pbma_df), ")")) + 
  theme_bw() + 
  theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_pbmaw_epi.png"), 
          plot_ordered_pbmaw, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# filter out Pseudo-BMA weights close to zero
# where close to zero means < 1e-5 (?) 

pbma_df |> arrange(pbma_weight) |>
  mutate(models = forcats::fct_inorder(rownames(pbma_df))) |>
  filter(pbma_weight >= 1e-05) |> 
  count()

# set cutoff for PBMA weight
epsilon = 1e-05

# visualise
plot_filtered_pbmaw <- pbma_df |> 
  arrange(pbma_weight) |>
  mutate(models = forcats::fct_inorder(rownames(pbma_df))) |>
  filter(pbma_weight >= epsilon) |> 
  {ggplot(., aes(x = pbma_weight, y = models)) + 
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), ")")) + 
      theme_bw() + 
      theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_pbmaw_epi.png"), 
          plot_filtered_pbmaw,
          base_height = 10, 
          base_aspect_ratio = 1.5)

# Plots: visual inspection of stacking weights ####

plot_ordered_stackw <- stack_df |> 
  arrange(stack_weight) |>
  mutate(models = forcats::fct_inorder(rownames(stack_df))) |> 
  {ggplot(., aes(x = stack_weight, y = models)) + 
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("All models (k=", NROW(.), ")")) + 
      theme_bw() +
      theme(axis.text.y = element_text(color = "grey20", size = 6, angle = 0, hjust = 1, vjust = 0, face = "plain"))
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_stackw_epi.png"), 
          plot_ordered_stackw, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# filter out stacking weights close to zero
# where close to zero means < 1e-04 (?) 

plot_filtered_stackw <- stack_df |> 
  arrange(stack_weight) |>
  mutate(models = forcats::fct_inorder(rownames(stack_df))) |>
  filter(stack_weight >= 1e-04) |> 
  {ggplot(., aes(x = stack_weight, y = models)) + 
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), ")")) + 
      theme_bw() + 
      theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_stackw_epi.png"), 
          plot_filtered_stackw,
          base_height = 15, 
          base_aspect_ratio = 1.5)

# Plots: visual inspection of elpd diff + se ####
plot_all_elpddiff <- full_df |>
  arrange(elpd_diff) |> 
  mutate(models = forcats::fct_inorder(rownames(.))) |>
  {ggplot(., aes(x = elpd_diff, y = models)) +
      geom_errorbar(width=.1, aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff)) +
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("All models (k=", NROW(.), ")")) + 
      theme_bw()
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_elpddiff_epi.png"), 
          plot_all_elpddiff,
          base_height = 15, 
          base_aspect_ratio = 1.5)

# mean is sensitive to outliers -> is it thus a more "conservative" filter?!
mean_filter = mean(full_df$elpd_diff)

# visualise filtered set of models 
plot_filter_mean_elpddiff <- full_df |>
  arrange(elpd_diff) |> 
  mutate(models = forcats::fct_inorder(rownames(.))) |>
  filter(elpd_diff >= mean_filter) |>
  {ggplot(., aes(x = elpd_diff, y = models)) +
      geom_errorbar(width=.1, aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff)) +
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), "), using mean")) + 
      theme_bw()
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_mean_elpddiff_epi.png"), 
          plot_filter_mean_elpddiff,
          base_height = 15, 
          base_aspect_ratio = 1.5)

median_filter = median(full_df$elpd_diff)

plot_filter_median_elpddiff <- full_df |>
  arrange(elpd_diff) |> 
  mutate(models = forcats::fct_inorder(rownames(.))) |>
  filter(elpd_diff >= median_filter) |>
  {ggplot(., aes(x = elpd_diff, y = models)) +
      geom_errorbar(width=.1, aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff)) +
      geom_point(shape=21, size=2) +
      geom_vline(xintercept = 0, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), "), using median")) + 
      theme_bw()
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_median_elpddiff_epi.png"), 
          plot_filter_median_elpddiff,
          base_height = 15, 
          base_aspect_ratio = 1.5)

# Which models are too similar to be meaningfully distinguished wrt pred. performance? -> se overlaps 0
# ...

# store model names of models with rhat_Trt > 1.01 ####
filter_out_rhat <- rhat_trt_df %>%
  filter(rhat_Trt > 1.01) %>%
  select(model_name)

# filter out models with "too many" high rhats ####
filter_out_high_pareto_k <- tibble(model_name = names(all_pareto_ks), pareto_k = all_pareto_ks) |>
  mutate(sum_pareto_k = purrr::map_dbl(pareto_k, get_sum_high_ks)) |>
  # filter out if more than 5% of Pareto k's are "bad"
  filter(sum_pareto_k > 12) |> 
  select(model_name)

# store names of models filtered out based on PBMA weight ####
filter_out_pbma <- full_df %>%
  mutate(model_name = rownames(.)) %>% 
  filter(pbma_weight < epsilon) %>%
  select(model_name)

# store model names of models with elpd_diff < mean(elpd_diff) ####
filter_out_elpd_diff <- full_df %>%
  mutate(model_name = rownames(.)) %>% 
  filter(elpd_diff < mean(elpd_diff)) %>%
  select(model_name)

# join all filters ####
model_names <- comb_df$model_name

# find the models that would be selected according to each "filter"
models_rhat <- setdiff(model_names, filter_out_rhat$model_name)
models_pareto_k <- setdiff(model_names, filter_out_high_pareto_k$model_name)
models_elpd_diff <- setdiff(model_names, filter_out_elpd_diff$model_name)
models_pbma <- setdiff(model_names, filter_out_pbma$model_name)

# from https://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

# intersect these vectors of model names to get the models that are left 
Reduce(intersect, list(models_rhat, models_pareto_k, models_elpd_diff, models_pbma))

Reduce(intersect, list(models_rhat, models_pareto_k))

# check how the result differs without PBMA weights 
Reduce(intersect, list(models_rhat, models_elpd_diff))
# get response
# response <- brms::get_y()

# sensitivity ####

# What are the "best" models? 
# For simplicity: models that are well-specified and have highest elpd_loo

# What are the worst models? 
# depends on axis of comparison