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
multi_epi <- read_rds(here::here("case-studies", "epilepsy", "results", "multiverse_epi.rds"))
multi_dict_epi <- read_rds( here::here("case-studies", "epilepsy", "results", "multi_dict_epi.rds"))

# test 
# draws_list <- do.call(get_posterior_draws, list(multi_epi$mod_epi))
# intervals <- lapply(draws_list, bayesplot::mcmc_intervals_data)
draws_trt <- purrr::map(multi_epi$mod_epi, ~spread_draws(.x, b_Trt1))

# create df

test_df <- multi_dict_epi %>% 
  select(model_id, family, formula, high_rhat_trt, elpd_loo, se_elpd_loo) %>% 
  mutate(model_id = factor(model_id, levels = unique(as.character(model_id))),
         family = factor(family, levels = unique(as.character(family))), 
         formula = factor(formula),
         high_rhat_trt = factor(unlist(high_rhat_trt), labels = c("Rhat ok", "Rhat > 1.01")),
         elpd_loo = as.numeric(unlist(elpd_loo)), 
         se_elpd = as.numeric(unlist(se_elpd_loo))) %>% 
  mutate(posterior_draws_trt = draws_trt)

test_combined <- test_df %>% 
  select(model_id, family, high_rhat_trt, posterior_draws_trt, elpd_loo, se_elpd) %>% 
  unnest(posterior_draws_trt)

# dotplot of all models 
posterior_dots_treat <- ggplot(test_combined, aes(x = b_Trt1, y = model_id)) + 
  stat_dotsinterval(quantiles = 100) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "post_dots_treat_epi.png"), 
          posterior_dots_treat, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

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

# visualize elpd ####
plot_elpd_sd <- ggplot(test_combined, aes(x = model_id, y = elpd_loo)) + 
  geom_errorbar(width=.1, aes(ymin = elpd_loo - se_elpd, ymax = elpd_loo + se_elpd)) +
  geom_point(shape=21, size=2, fill="white") +
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_sd_epi.png"), 
          plot_elpd_sd, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# sort descending by highest elpd-sd ####

test_df_sorted <- test_combined %>%
  mutate(elpd_low = elpd_loo - se_elpd, 
         elpd_high = elpd_loo + se_elpd) %>% 
  arrange(desc(elpd_low)) %>%
  select(model_id, elpd_loo, elpd_low, elpd_high)

plot_elpd_sd_sorted <- ggplot(test_df_sorted, aes(x = reorder(model_id, elpd_loo), y = elpd_loo)) + 
  geom_errorbar(width=.1, aes(ymin = elpd_low, ymax = elpd_high)) +
  geom_point(shape=21, size=2, fill="white") +
  theme_bw()

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_sorted_epi.png"), 
          plot_elpd_sd_sorted, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# Example: filter out "worst" models ####

test_df_sorted_filtered <- test_combined %>%
  mutate(elpd_low = elpd_loo - se_elpd, 
         elpd_high = elpd_loo + se_elpd) %>% 
  arrange(desc(elpd_low)) %>%
  filter(!model_id %in% c(1,5)) %>% 
  select(model_id, family, formula, elpd_loo, elpd_low, elpd_high)

plot_elpd_sd_sorted_filtered <- ggplot(
  test_df_sorted_filtered, 
  aes(x = reorder(model_id, elpd_loo), y = elpd_loo,
      text = paste0("elpd:", elpd_loo, "<br>",
                    "family:", "<br>",
                    "equation:", equation))) + 
  geom_errorbar(width=.2, aes(ymin = elpd_low, ymax = elpd_high)) +
  geom_point(shape=21, size=2, fill="white") +
  theme_bw()

ggplotly(tooltip = "text")

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_sorted_filtered_epi.png"), 
          plot_elpd_sd_sorted_filtered, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# PPC count data #### 

for (i in c(1,24)){
  y <- brms::epilepsy$count
  yrep <- posterior_predict(multi_epi$mod_epi[[i]])
  library(bayesplot)
  library(cowplot)
  color_scheme_set("brightblue")
  plot <- ppc_rootogram(y, yrep)
  
  save_plot(here::here("case-studies", "epilepsy", "figures", paste0("plot_ppc_", i, "_epi.png")), 
            plot, 
            base_height = 5, 
            base_aspect_ratio = 1.4)
}

# add model id 
names(draws_trt) <- 1:length(draws_trt) 

# unpack posterior draws for treatment in one column 
combined <- dplyr::bind_rows(draws_trt, .id = "model") %>%
  mutate(model = factor(model, levels = unique(as.character(model)))) # get proper factors from list names
  mutate(family = factor()) 
  
combined %>% 
ggplot(aes(x = b_Trt1, y = model)) + 
  stat_dotsinterval(quantiles = 100)

get_variables(multi_epi$mod_epi[[28]])

draws_trt <- multi_epi$mod_epi[[1]] %>% 
  spread_draws(b_Trt1)

# get posterior draws for treatment effect 
multi_epi$mod_epi[[1]] %>% 
  spread_draws(b_Trt1) %>% 
  ggplot(aes(x = b_Trt1)) + 
  stat_dotsinterval(quantiles = 100)

# now add grouping 

add_epred_draws(multi_epi$mod_epi[[1]])




# filter by high and low elpd

# filter by convergence issues and now convergence issues

# get vector of all names despite "treat"
param_names <-
  map(multi_epi$mod_epi, posterior::variables) %>% 
  unlist() %>% 
  unique() %>%
  stringr::str_remove(., "b_")
param_names[2] <- "treat"

# only plot treatment effect
drop_vec <- param_names[! param_names %in% c("treat")]

# ungrouped 
posterior_plot_ungrouped <- do.call(
  compare_posteriors, 
  list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)")))

save_plot(here::here("case-studies", "epilepsy", "figures", "post_treat_epi_all.png"), 
          posterior_plot_ungrouped, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# grouped 
to_mods <- as_labeller(c(`TRUE` = "poisson", `FALSE` = "negbinom"))
posterior_plot_treat <- 
  do.call(compare_posteriors, 
          list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)"))) + 
  facet_grid(rows = NULL, 
             vars(model %in% c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)), 
             labeller = to_mods)

save_plot(here::here("case-studies", "epilepsy", "figures", "post_treat_epi.png"), 
          posterior_plot_treat, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# poterior treatment with dotplots

# from tidybayes:

m %>%
  spread_draws() %>%
  ggplot(aes(x = m, y = parameter, color = model, group = model)) +
  stat_dotsinterval(quantiles = 100)

# PPC

y_rep = multi_epi$y_rep
y = data_epi$outcome 

bayesplot$ppc_dens_overlay(y, yrep_poisson[1:50, ])

# dendrogram und quantile dotplots 