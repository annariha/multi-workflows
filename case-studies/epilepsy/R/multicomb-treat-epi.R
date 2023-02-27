#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr,
               ggdendro, cowplot)

# run once
# cmdstanr::install_cmdstan()

source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_brms_formula.R"))
source(here::here("case-studies", "epilepsy", "R", "get_rhat_trt.R"))

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# data
dat <- brms::epilepsy 
outcome_str <- "count"

# create combinations ####

families <- list(poisson = poisson(), 
                 negbinomial = negbinomial())

priors <- list(brms_default = NULL, 
               brms_horseshoe = set_prior("horseshoe(3)")
               #,rstanarm_default = set_prior("", class = "b")
               )

# get priors from brms model 
#get_prior(formula, dat)

# intercept
# coefficients 
# prior = normal(0, 2.5, autoscale = TRUE)
# auxiliary vars
# normal(location = 0, scale = NULL, autoscale = FALSE)
# student_t(df = 1, location = 0, scale = NULL, autoscale = FALSE)

# set_prior("<prior>", class = "b")

combinations_df <- expand.grid(
  family = names(families),
  prior = priors,
  # fixed effects 
  Trt = c("", "Trt"), 
  zBase = c("", "zBase"),
  zAge = c("", "zAge"),
  # random effects 
  patient = c("", "(1 | patient)"),
  visit = c("", "(1 | visit)"),
  obs = c("", "(1 | obs)")
  )

# add interaction effect in the rows where treatment was left out, (i.e., where Trt == "")
combinations_df <- combinations_df |> 
  mutate(zBaseTrt = factor(
    case_when(
      Trt == "Trt" ~ "",
      Trt == "" ~ "zBase * Trt"))) |> 
  # filter out rows with interaction and zBase
  filter(!(zBaseTrt == "zBase * Trt" & combinations_df$zBase == "zBase"))

combinations_df <- combinations_df |>
  # add outcome name 
  mutate(outcome = rep_len(outcome_str, NROW(combinations_df))) |>
  mutate(priors = names(combinations_df$prior)) |>
  # reorder to have outcome name, family and treatment effects first 
  select(outcome, family, priors, prior, Trt, zBaseTrt, everything())

# add prior names for easier summarising, plotting etc. 
combinations_df <- combinations_df |> 
  mutate(priors = names(combinations_df$prior))

# Plots: visualise treatment/control group and visit, patient level info ####

# Why random effect based on visits and patients?
plot_patient_visit_count <- 
  dat |> 
  mutate(treatment = ifelse(Trt == 0, "no treatment", "treatment")) |>
  ggplot(aes(x = visit, y = count, group = patient, color = patient)) +
  geom_point() +
  geom_line() +
  ylab("seizure count") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~treatment)

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_patient_visit_count_epi.png"), 
          plot_patient_visit_count)

# Why add an interaction effect? 
plot_base_count <-
  dat |> 
  mutate(treatment = ifelse(Trt == 0, "no treatment", "treatment")) |>
  ggplot(aes(x = Base)) +
  geom_point(aes(y = count, color = patient), size=0.8) +
  scale_x_continuous("base seizure count") +
  ylab("seizure count") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~treatment)

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_base_count_epi.png"), 
          plot_base_count)

# add model name and brms formula for each combination ####
comb_df <- combinations_df |> 
  mutate(
    model_name = apply(combinations_df, 1, build_name), 
    formula = apply(combinations_df, 1, build_brms_formula)) 

# compare every column rowwise in df ####
# *1 to turn T/F to 1/0 if value in column different 
# combn() gives combinations of all rows  
df <- as.matrix(combinations_df  |> select(!prior))
compare <- t(combn(nrow(df), 2, FUN = function(x) df[x[1],]!=df[x[2],])) * 1
vals <- rowSums(compare)

# create distance matrix with zero on diagonal ####
distmatrix <- diag(x = 0, nrow = NROW(combinations_df), ncol = NROW(combinations_df))
rownames(distmatrix) <- rownames(combinations_df)
colnames(distmatrix) <- rownames(combinations_df)

# add edit distance to lower and upper diagonal 
distmatrix[lower.tri(distmatrix)] <- vals
distmatrix[upper.tri(distmatrix)] <- t(distmatrix)[upper.tri(distmatrix)]
#forceSymmetric(distmatrix)

# cluster based on topology ####
hc_edit <- as.dendrogram(hclust(as.dist(distmatrix))) 
hc_edit_dendro <- dendro_data(hc_edit) 

# add model names as labels
dict <- setNames(paste0(comb_df$model_name,", ", comb_df$priors), 1:192)
hc_edit_dendro$labels$label <- sapply(hc_edit_dendro$labels$label, function(x) dict[[as.character(x)]])

# plot dendrogram with edit distances ####
dendrogram_plot_edit <- ggdendrogram(hc_edit_dendro, rotate = TRUE)  

save_plot(here::here("case-studies", "epilepsy", "figures", "dendrogram_edit_epi.png"), 
          dendrogram_plot_edit, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# fit model for each combination ####

build_fit <- function(row, ...){
  brm(
    formula=build_brms_formula(row), 
    data=epilepsy, 
    prior=row[["prior"]],
    file=digest::digest(build_name(row), algo="md5"),
    backend="cmdstanr", silent=2, refresh=0
  ) 
}

# high Rhat for treatment ####

test_row <- combinations_df[1,]
mod1 <- build_fit(test_row)

# get all modelfits 
models <- apply(combinations_df, 1, build_fit)

# This gives the same result as using apply(...) 
models <- combinations_df |>
  group_nest(row_number()) |>
  pull(data) |>
  purrr::map(build_fit)

# all rhats 
all_rhats <- purrr::map(models, brms::rhat)

# get rhats for treatment (and interaction) i.e., only Rhats of "b_Trt1" (and if present "b_zBase:Trt1")
rhats_trt <- purrr::map(models, get_rhat_trt)

# test
names(rhats_trt) <- comb_df$model_name

rhat_trt_df <- 
  tibble(model_name = comb_df$model_name, rhats_raw = rhats_trt) |>
  unnest(rhats_raw) |>
  group_by(model_name) |>
  mutate(key = row_number()) |>
  spread(key, rhats_raw) |>
  rename(rhat_Trt = 2, rhat_zBaseTrt = 3) |>
  # high rhat for treatment
  mutate(high_rhat_Trt = ifelse(rhat_Trt > 1.01, 1, 0),
         high_rhat_zBaseTrt = ifelse(rhat_zBaseTrt > 1.01, 1, 0))

# plot Rhats for treatment 
plot_rhats_trt <- 
  ggplot(data = rhat_trt_df, aes(rhat_Trt, model_name)) + 
  geom_point() + 
  geom_vline(xintercept = 1.01, linetype="dotted") + 
  ggtitle(paste0("All models (k=", NROW(rhat_trt_df), ")")) + 
  theme_bw() + 
  theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_rhats_trt_epi.png"), 
          plot_rhats_trt,
          base_height = 19, 
          base_aspect_ratio = 1.5)

plot_filtered_rhats_trt <- rhat_trt_df %>%
  filter(rhat_Trt < 1.01) %>%
  {ggplot(., aes(x = rhat_Trt, y = model_name)) + 
      geom_point() +
      geom_vline(xintercept = 1.01, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), ")")) + 
      theme_bw() + 
      theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_rhats_trt_epi.png"), 
          plot_filtered_rhats_trt,
          base_height = 19, 
          base_aspect_ratio = 1.5)

plot_high_rhats_trt <- rhat_trt_df %>%
  filter(rhat_Trt > 1.01) %>%
  {ggplot(., aes(x = rhat_Trt, y = model_name)) + 
      geom_point() +
      geom_vline(xintercept = 1.01, linetype="dotted") +
      ggtitle(paste0("Filtered set of models (k=", NROW(.), ")")) + 
      theme_bw() + 
      theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))
  }

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_high_rhats_trt_epi.png"), 
          plot_high_rhats_trt)

# loo: elpd and model comparison ####
build_loo <- function(row, ...){
  # print(build_name(row))
  file_name = paste0(digest::digest(build_name(row), algo="md5"), "_loo.rds")
  if(file.exists(file_name)){
    return(readRDS(file_name))
  }else{
    rv = loo(build_fit(row), model_names=c(build_name(row)), moment_match = TRUE)
    saveRDS(rv, file_name)
    return(rv)
  }
} 

#build_loo(test_row)

model_names = apply(combinations_df, 1, build_name)
rownames(combinations_df) <- model_names

# get loo 
tic()
loos <- apply(combinations_df, 1, build_loo)
toc()
# get high pareto k's
high_pareto_ks <- purrr::map(loos, ~.x$diagnostics$pareto_k > 0.7)

# compare models with loo & model averaging weights ####
comparison_df = loo::loo_compare(loos)
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

# visual inspection of convergence checks for treatment var ####
# ...

# visual inspection of pbma weights ####

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

# visual inspection of stacking weights ####

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

# visual inspection of elpd diff + se ####
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
models_elpd_diff <- setdiff(model_names, filter_out_elpd_diff$model_name)
models_pbma <- setdiff(model_names, filter_out_pbma$model_name)

# from https://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

# intersect these vectors of model names to get the models that are left 
Reduce(intersect, list(models_rhat, models_elpd_diff, models_pbma))

# check how the result differs without PBMA weights 
Reduce(intersect, list(models_rhat, models_elpd_diff))
# get response
# response <- brms::get_y()

# sensitivity ####

# What are the "best" models? 
# For simplicity: models that are well-specified and have highest elpd_loo

# What are the worst models? 
# depends on axis of comparison