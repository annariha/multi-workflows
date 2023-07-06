#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, cowplot)

# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank()))

# load scripts
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))

# check and create dir if needed
filedir = here::here("case-studies", "epilepsy", "figures")
if (!dir.exists(filedir)) {dir.create(filedir)}

# load elpd diff & PBMA df 
full_df_elpddiff_pbma <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds"))

# create plot of elpd diff +/- se diff for default LOO ####
df_plot_elpddiff_default <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "default") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpd_diff_default <- ggplot(data = df_plot_elpddiff_default, aes(elpd_diff, model_id, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=21) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlab("$Delta elpdPlain$") +
  ylab("Models")+
  scale_color_manual(values=c("black","red"), name="Too-high Pareto $hat k$") +
  # change scales to sth that makes sense
  scale_x_continuous(trans = "pseudo_log", breaks = c(0, -500, -1000, -2000)) +
  theme(
        axis.text.y = element_blank(),
        #axis.text.x = element_text(size=15)
        legend.position = c(0.2,0.9)
        )

plot_elpd_diff_default

save_tikz_plot(plot = plot_elpd_diff_default, 
               width = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpd_diff_default.tex")
               )

# create plot of elpd diff +/- se diff for integrated LOO ####
df_plot_elpddiff_intloo <- full_df_elpddiff_pbma |>
  mutate(modelnames = rownames(full_df_elpddiff_pbma)) |>
  filter(loo_computation == "integrated LOO") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  arrange(elpd_diff) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(modelnames, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks, loo_computation)

plot_elpd_diff_intloo <- ggplot(data = df_plot_elpddiff_intloo, aes(elpd_diff, model_id, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=21) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlab("$Delta elpdPlain$") +
  ylab("Models")+
  scale_color_manual(values=c("black","red"), name="Too-high Pareto $hat k$") +
  # change scales to sth that makes sense
  scale_x_continuous(trans = "pseudo_log", breaks = c(0, -500, -1000, -2000)) +
  theme(
    axis.text.y = element_blank(),
    #axis.text.x = element_text(size=15)
    legend.position = c(0.2,0.9)
  )

plot_elpd_diff_intloo

save_tikz_plot(plot = plot_elpd_diff_intloo, 
               width = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_elpd_diff_intloo.tex")
)


save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_diff_intloo.png"), 
          plot_elpd_diff_intloo, 
          base_height = 15, 
          base_aspect_ratio = 1.5)

# indistinguishable models 
df_plot_indisting_default <- df_plot_elpddiff_default |>
  filter(elpd_diff + se_diff >= 0) 

plot_elpd_diff_indisting_default <- ggplot(data = df_plot_indisting_default, aes(elpd_diff, modelnames, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=21) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("black","red")) +
  theme(axis.title.y = element_blank(),
        #axis.text.y = element_text(size=15),
        #axis.text.x = element_text(size=15)
        #,legend.position = "none"
  )

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_diff_indisting_default.png"), 
          plot_elpd_diff_indisting_default,
          base_height = 10,
          base_aspect_ratio = 1.5)

df_plot_indisting_intloo <- df_plot_elpddiff_intloo |>
  filter(elpd_diff + se_diff >= 0) 

plot_elpd_diff_indisting_intloo <- ggplot(data = df_plot_indisting_intloo, aes(elpd_diff, modelnames, col = high_pareto_ks)) +
  geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), shape=21) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("black","red")) +
  theme(axis.title.y = element_blank(),
        #axis.text.y = element_text(size=15),
        #axis.text.x = element_text(size=15)
        #,legend.position = "none"
  )

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_elpd_diff_indisting_intloo.png"), 
          plot_elpd_diff_indisting_intloo,
          base_height = 10,
          base_aspect_ratio = 1.5)
