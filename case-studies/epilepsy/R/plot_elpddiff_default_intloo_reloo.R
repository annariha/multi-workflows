#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, patchwork)

# load scripts
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))
source(here::here("case-studies", "epilepsy", "R", "get_plot_elpddiffs.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load default and intloo+reloo results ####
full_df_elpddiff_pbma <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds"))
full_df_intloo_reloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_intloo_reloo.rds"))

# create plot of elpd diff +/- se diff for default LOO ####
df_plot_elpddiff_default <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "default") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, elpd_diff, se_diff, pbma_weight, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpddiff_all_default <- 
  get_plot_elpddiffs(df_plot_elpddiff_default, 
                     pointsize = 0.9, 
                     subtitle_char = "Default PSIS-LOO-CV", 
                     ylabel_char = "Models") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -50, -500,-1500),
                     limits = c(-1500,3)) +
  theme(axis.text.y = element_blank(), legend.position = "none")

# create plot of remaining set of models for default LOO ####
df_plot_elpddiff_indist_default <- df_plot_elpddiff_default |>
  #filter(elpd_diff + 2*se_diff >= 0) 
  #filter(abs(elpd_diff) < 4) 
  filter(pbma_weight > 0.01)

#full_df_elpddiff_pbma |>
# mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
# filter(loo_computation == "default") |>
# mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>

plot_elpddiff_indist_default <- get_plot_elpddiffs(df_plot_elpddiff_indist_default) +
  theme(axis.title.y = element_blank(), legend.position = "none")

# create plot of LOO-BB weights for remaining set of models for default LOO ####
# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=8),
                  plot.title = element_text(size=8),
                  axis.title = element_text(size=8),
                  axis.text = element_text(size=8)))

# prepare data for plotting 
plot_df <- df_plot_elpddiff_indist_default |>
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, pbma_weight, model_id, high_pareto_ks)

plot_loo_bb_weights <- ggplot(plot_df, 
                                 aes(x = pbma_weight, y = model_id, col = high_pareto_ks, shape = family)) +
  geom_point(size=2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$w tiny LOO-BB$") +
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

#plot_loo_bb_weights <- get_plot_loo_bb_weights(df_plot_loo_bb_weights_indist_default, arrange_var = )

plot_loo_bb_weights

# Combine elpddiff for all and indist. models for default PSIS-LOO-CV ####
plot_elpddiff_all_indist_default <- plot_elpddiff_all_default | plot_elpddiff_indist_default | plot_loo_bb_weights
plot_elpddiff_all_indist_default

save_tikz_plot(plot = plot_elpddiff_all_indist_default, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_all_indist_loo_bb_default.tex")
)

# create plot of elpd diff +/- se diff for integrated LOO + reloo() ####
df_plot_elpddiff_intloo_reloo <- full_df_intloo_reloo |>
  mutate(modelnames = rownames(full_df_intloo_reloo)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  #arrange(elpd_diff) |>
  #mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, elpd_diff, se_diff, pbma_weight, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpddiff_all_intloo_reloo <- 
  get_plot_elpddiffs(df_plot_elpddiff_intloo_reloo, 
                     pointsize = 0.9,
                     subtitle_char = "Integrated PSIS-LOO-CV + brute-force LOO-CV", 
                     ylabel_char = "Models") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -50, -500,-1500),
                     limits = c(-1500,3)) +
  theme(axis.text.y = element_blank(), legend.position = "none")

plot_elpddiff_all_intloo_reloo

# create plot of elpd diff +/- se diff for integrated LOO and reloo() ####
df_plot_elpddiff_indist_intloo_reloo <- df_plot_elpddiff_intloo_reloo |>
  #filter(elpd_diff + se_diff >= 0) 
  #filter(abs(elpd_diff) < 4) 
  filter(pbma_weight > 0.01)

plot_elpddiff_indist_intloo_reloo <- get_plot_elpddiffs(df_plot_elpddiff_indist_intloo_reloo) +
  theme(axis.title.y = element_blank(), legend.position = "none")

plot_elpddiff_indist_intloo_reloo

# plot PBMA weights 
# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=8),
                  plot.title = element_text(size=8),
                  axis.title = element_text(size=8),
                  axis.text = element_text(size=8)))

# prepare data for plotting 
plot_df_intloo_reloo <- df_plot_elpddiff_indist_intloo_reloo |>
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, pbma_weight, model_id, high_pareto_ks)

plot_loo_bb_weights_intloo_reloo <- ggplot(plot_df_intloo_reloo, aes(x = pbma_weight, y = model_id, col = high_pareto_ks, shape = family)) +
  geom_point(size=2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$w tiny LOO-BB$") +
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
  theme(axis.title.y = element_blank(), legend.position = "none")

# Combine elpddiff for all and indist. models for for integrated LOO and reloo() ####
plot_elpddiff_all_indist_intloo_reloo <- plot_elpddiff_all_intloo_reloo | plot_elpddiff_indist_intloo_reloo | plot_loo_bb_weights_intloo_reloo
plot_elpddiff_all_indist_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_all_indist_intloo_reloo, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_all_indist_loo_bb_intloo_reloo.tex")
)

# Combine all four plots ####
plot_elpddiff_combined_default_intloo_reloo <- (plot_elpddiff_all_default | plot_elpddiff_indist_default) / (plot_elpddiff_all_intloo_reloo | plot_elpddiff_indist_intloo_reloo)

plot_elpddiff_combined_default_intloo_reloo <- plot_elpddiff_all_indist_default / plot_elpddiff_all_indist_intloo_reloo
plot_elpddiff_combined_default_intloo_reloo
save_tikz_plot(plot = plot_elpddiff_combined_default_intloo_reloo,
               width = 6.5,
               height = 5.8,
               filename = here::here("case-studies", "epilepsy", "figures", "epi-2", "plot_elpddiff_combined_all_indist_loo_bb_default_intloo_reloo.tex"))

# Combine plots for all models for the two different estimation methods ####
plot_elpddiff_all_default_reloo <- plot_elpddiff_all_default | plot_elpddiff_all_intloo_reloo
plot_elpddiff_all_default_reloo

save_tikz_plot(plot = plot_elpddiff_all_default_reloo, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_all_default_reloo.tex")
)

# Combine plots for indistinguishable models for the two different estimation methods ####
plot_elpddiff_indist_reloo <- plot_elpddiff_indist_default | plot_elpddiff_indist_intloo_reloo
plot_elpddiff_indist_reloo

save_tikz_plot(plot = plot_elpddiff_indist_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_indist_reloo.tex")
)

# create plot of elpd diff +/- se diff for integrated LOO ####
df_plot_elpddiff_intloo <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "integrated LOO") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  #filter(elpd_diff + se_diff >= 0) 
  filter(abs(elpd_diff) < 4)  

plot_elpddiff_intloo <- get_plot_elpddiffs(df_plot_elpddiff_intloo)
plot_elpddiff_intloo

# Combine all three plots for indistinguishable models for default, integrated and integrated+exact ####
plot_elpddiff_indist_default_intloo_reloo <-  plot_elpddiff_default | plot_elpddiff_intloo | plot_elpddiff_intloo_reloo
plot_elpddiff_indist_default_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_indist_default_intloo_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_indist_default_intloo_reloo.tex")
)