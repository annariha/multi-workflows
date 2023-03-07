#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")

pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, bayesplot, cowplot, RColorBrewer,
               loo, multiverse, priorsense, modelr, ggdist)

# set seed
set.seed(42424242)

# data from brms: https://paul-buerkner.github.io/brms/reference/epilepsy.html
dat <- brms::epilepsy

# load data from results 
comb_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_without_obs.rds"))
loo_df_94_models <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "loo_df_94_models_epi.rds"))
comb_df_for_plots <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_for_plots_without_obs_epi.rds"))
comb_df_for_plots_minimal <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_for_plots_minimal_without_obs_epi.rds"))

# test 
# draws_list <- do.call(get_posterior_draws, list(multi_epi$mod_epi))
# intervals <- lapply(draws_list, bayesplot::mcmc_intervals_data)

# interaction effect btw treatment and base seizure count ####
# here: in the models with Trt1, zBase & interaction term assume a different intercept and different slope
# since Trt is binary, this means assuming that the slope depends on whether a person got the treatment or not 
# to get the effect the other way around, i.e. slope depending on levels in zBase, we would need to (maybe) group zBase? 

# test
test <- comb_df$model_fit[[1]]
  
# posterior draws as dataframe 
post <- as_draws_df(test)

# This code is only considering zBase = 1
post %>%
  mutate(gamma_treat = b_Trt1 + `b_zBase:Trt1`) %>%
  select(b_Trt1, gamma_treat)

# this would be gamma for zBase, depending on treat/no treat
post %>%
  transmute(gamma_treat = b_zBase + `b_zBase:Trt1`,
            gamma_nottreat = b_zBase) %>%
  gather(key, value) %>%
  group_by(key) %>%
  summarise(mean = mean(value))

# filter for models with interaction
comb_df |> 
  filter(zBaseTrt == "") |>
  purrr::map(., ~spread_draws(.x, b_zBase:Trt1))

# create df of posterior draws####

# regex allows to extract results for b_Trt1 (treatment) and b_zBase:Trt1 (interaction) (if present) 
draws_trt_test <- purrr::map(comb_df$model_fit, ~as_draws_df(.x, variable = "Trt", regex = TRUE))

# df for plotting 
test_df <- comb_df |> 
  mutate(posterior_draws_trt = draws_trt_test) |>
  arrange(no_issues) |>
  mutate(model_name = forcats::fct_inorder(model_name)) %>%
  select(model_name, no_issues, posterior_draws_trt) |>
  unnest(posterior_draws_trt)

write_rds(test_df, here::here("case-studies", "epilepsy", "data", "prelim", "test_df_without_obs.rds"))
test_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "test_df_without_obs.rds"))

# gradient interval plot of all models, color indicates computational issues ####
posterior_grad_treat <- ggplot(test_df, aes(x = b_Trt1, y = model_name, group = factor(no_issues), color = factor(no_issues))) + 
  stat_gradientinterval() +
  xlim(-1.15, 0.55) + 
  xlab("posterior treatment effect") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("red", "black")) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size=30),
        axis.title.x = element_blank(),
        legend.position = "none")
 
save_plot(here::here("case-studies", "epilepsy", "figures", "post_grad_treat_without_obs_issues_epi.png"), 
          posterior_grad_treat, 
          base_height = 28, 
          base_aspect_ratio = 1)

# dotplot of all models 
posterior_treat <- ggplot(test_df, aes(x = b_Trt1, y = model_name)) + 
  stat_interval() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme(axis.title.y = element_blank()) +
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "post_treat_without_obs_epi.png"), 
          posterior_treat, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# create df for filtered set of models ####

# filter for best models
model_names_elpd_pbma <- full_df |>
  mutate(model_name = row.names(full_df)) |>
  filter(abs(elpd_diff) < 4) |>
  filter(pbma_weight > 0.01) |>
  arrange(desc(elpd_diff)) |>
  select(model_name, elpd_diff, se_diff, pbma_weight)

model_names_pbma <- full_df |>
  mutate(model_name = row.names(full_df)) |>
  filter(pbma_weight > 0.01) |>
  arrange(desc(pbma_weight)) |>
  select(model_name, elpd_diff, se_diff, pbma_weight)

# regex allows to extract results for b_Trt1 (treatment) and b_zBase:Trt1 (interaction) (if present) 
draws_trt_test_reduced <- purrr::map(comb_df_for_plots$model_fit, ~as_draws_df(.x, variable = "Trt", regex = TRUE))

# df for plotting
test_df_reduced <- comb_df_for_plots |> 
  mutate(posterior_draws_trt = draws_trt_test_reduced) |>
  arrange(no_issues, elpd_diff) |>
  mutate(model_name = forcats::fct_inorder(model_name)) |>
  select(model_name, no_issues, posterior_draws_trt) |>
  unnest(posterior_draws_trt)

# gradient interval plot of filtered set of models 
posterior_grad_treat_reduced <- ggplot(test_df_reduced, aes(x = b_Trt1, y = model_name, group = factor(no_issues), color = factor(no_issues))) + 
  stat_gradientinterval() +
  xlim(-1.15, 0.55) + 
  scale_y_discrete(expand=c(0.1, 0)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("red", "black")) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size=30),
        axis.title.x = element_blank(),
        legend.position = "none")

save_plot(here::here("case-studies", "epilepsy", "figures", "post_grad_treat_reduced_without_obs_epi.png"), 
          posterior_grad_treat_reduced,
          base_height = 22, 
          base_aspect_ratio = 1)

# plotting minimal set of models 
draws_trt_test_reduced <- purrr::map(comb_df_for_plots$model_fit, ~as_draws_df(.x, variable = "Trt", regex = TRUE))

# df for plotting a minimal set of models
test_df_reduced <- comb_df_for_plots |> 
  mutate(posterior_draws_trt = draws_trt_test_reduced) |>
  arrange(no_issues, elpd_diff) |>
  mutate(model_name = forcats::fct_inorder(model_name)) |>
  select(model_name, no_issues, posterior_draws_trt) |>
  unnest(posterior_draws_trt)

# gradient interval plot of filtered set of models 
posterior_grad_treat_minimal <- ggplot(test_df_reduced, aes(x = b_Trt1, y = model_name)) + 
  stat_gradientinterval() +
  xlim(-1.15, 0.55) + 
  scale_y_discrete(expand=c(1, 0)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size=40),
        axis.text.x = element_text(size=30),
        axis.title.x = element_blank(),
        legend.position = "none")

save_plot(here::here("case-studies", "epilepsy", "figures", "post_grad_treat_minimal_without_obs_epi.png"), 
          posterior_grad_treat_minimal,
          base_height = 22, 
          base_aspect_ratio = 1)