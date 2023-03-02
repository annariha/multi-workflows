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
  arrange(visit) |>
  mutate(model_name = forcats::fct_inorder(model_name)) %>%
  select(model_name, posterior_draws_trt) |>
  unnest(posterior_draws_trt)

# gradient interval plot of all models 
posterior_grad_treat <- ggplot(test_df, aes(x = b_Trt1, y = model_name)) + 
  stat_gradientinterval() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "post_grad_treat_without_obs_epi.png"), 
          posterior_grad_treat, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# dotplot of all models 
posterior_treat <- ggplot(test_df, aes(x = b_Trt1, y = model_name)) + 
  stat_interval() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "post_treat_without_obs_epi.png"), 
          posterior_treat, 
          base_height = 19, 
          base_aspect_ratio = 1.5)

# halfeye plot of all models 
posterior_heye_treat <- ggplot(test_combined, aes(x = b_Trt1, y = model_id)) + 
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "post_heye_treat_epi.png"), 
          posterior_heye_treat, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

posterior_heye_fam <- posterior_heye_treat +
  facet_grid(cols = vars(family))

save_plot(here::here("case-studies", "epilepsy", "figures", "post_heye_fam_epi.png"), 
          posterior_heye_fam, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

posterior_heye_rhat_fam <- posterior_heye_treat +
  facet_grid(high_rhat_trt ~ family)

save_plot(here::here("case-studies", "epilepsy", "figures", "post_heye_rhat_fam_epi.png"), 
          posterior_heye_rhat_fam, 
          base_height = 5, 
          base_aspect_ratio = 1.4)