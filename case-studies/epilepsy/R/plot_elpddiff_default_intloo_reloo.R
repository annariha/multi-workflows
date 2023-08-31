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
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpddiff_all_default <- ggplot(data = df_plot_elpddiff_default, aes(elpd_diff, model_id, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=1, size=0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$Delta widehat textrmelpd$") +
  ylab("Models") +
  labs(subtitle = "Default PSIS-LOO-CV") +
  scale_color_manual(values=c("yes" = "red", "no" = "black"), name="Too-high Pareto $hat k$") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -10,-100,-500,-1500),
                     limits = c(-1500,3)) +
  theme(axis.text.y = element_blank(), legend.position = "none")

plot_elpddiff_all_default

# create plot of elpd diff +/- se diff for integrated LOO + reloo() ####
df_plot_elpddiff_intloo_reloo <- full_df_intloo_reloo |>
  mutate(modelnames = rownames(full_df_intloo_reloo)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, family, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpddiff_all_intloo_reloo <- ggplot(data = df_plot_elpddiff_intloo_reloo, aes(elpd_diff, model_id, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=1, size=0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$Delta widehat textrmelpd$") +
  labs(subtitle = "Integrated PSIS-LOO-CV and exact LOO-CV") + 
  scale_color_manual(values=c("yes" = "red", "no" = "black"),  name="Too-high Pareto $hat k$") +
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -10,-100,-500,-1500),
                     limits = c(-1500,3)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

plot_elpddiff_all_intloo_reloo

plot_elpddiff_all_default_reloo <- plot_elpddiff_all_default | plot_elpddiff_all_intloo_reloo
plot_elpddiff_all_default_reloo

save_tikz_plot(plot = plot_elpddiff_all_default_reloo, 
               width = 6.5,
               height = 5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_all_default_reloo.tex")
)

# create plot of models indistinguishable by elpd diff +/- se diff for default LOO ####
df_plot_elpddiff_default <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "default") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  filter(elpd_diff + se_diff >= 0) 

plot_elpddiff_default <- get_plot_elpddiffs(df_plot_elpddiff_default)
plot_elpddiff_default

# create plot of elpd diff +/- se diff for integrated LOO ####
df_plot_elpddiff_intloo <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "integrated LOO") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  filter(elpd_diff + se_diff >= 0) 

plot_elpddiff_intloo <- get_plot_elpddiffs(df_plot_elpddiff_intloo)
plot_elpddiff_intloo

# create plot of elpd diff +/- se diff for integrated LOO and reloo() ####
df_plot_elpddiff_intloo_reloo <- full_df_intloo_reloo |>
  mutate(modelnames = rownames(full_df_intloo_reloo)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
  filter(elpd_diff + se_diff >= 0) 

plot_elpddiff_intloo_reloo <- get_plot_elpddiffs(df_plot_elpddiff_intloo_reloo)
plot_elpddiff_intloo_reloo

# combine plots and save ####
plot_elpddiff_indist_reloo <- plot_elpddiff_default | plot_elpddiff_intloo_reloo
plot_elpddiff_indist_reloo

save_tikz_plot(plot = plot_elpddiff_indist_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_indist_reloo.tex")
)

plot_elpddiff_indist_default_intloo_reloo <-  plot_elpddiff_default | plot_elpddiff_intloo | plot_elpddiff_intloo_reloo
plot_elpddiff_indist_default_intloo_reloo

save_tikz_plot(plot = plot_elpddiff_indist_default_intloo_reloo, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpddiff_indist_default_intloo_reloo.tex")
)