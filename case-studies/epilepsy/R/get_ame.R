#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, brms, Matrix, tidyverse, emmeans, modelr, tidybayes, ggplot2, patchwork)

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

# data for plotting ####
test <- full_df_elpddiff_pbma |>
  # we only need 1/2 of the df, since we are not comparing LOO computation here
  filter(loo_computation == "default") |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  #filter(zBaseTrt != "") |>
  #slice_sample(n=1) |>
  select(model_id, modelnames, modelfits, high_pareto_ks)

get_epred_emmeans <- function(modelfit){
  effect_of_interest_draws <- modelfit |>
    emmeans::emmeans(~Trt, epred = TRUE) |>
    emmeans::contrast(method = "revpairwise") |>
    tidybayes::gather_emmeans_draws()
  return(effect_of_interest_draws)
}

# adding | zBase to the emmeans() removes the warning, gives same results for the contrasts and gives the same value for all 4000 draws?
# adding simple="each" to contrast() still gives warning and does not change result- NOTE: Results may be misleading due to involvement in interactions

tic()
test <- test |>
  mutate(effect_draws = purrr::map(purrr::map(modelfits, pluck), get_epred_emmeans))
toc()

# test$effect_draws[[1]] %>% median_hdi()

ggplot(test$effect_draws[[1]], aes(x = .value)) +
  stat_halfeye() +
  labs(x = "Average marginal effect of treatment", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(test, aes(x = effect_draws, y = model_id)) +
  stat_halfeye() +
  labs(x = "Average marginal effect of treatment", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom")

# On average, having received the treatment is associated with a -0.917 point lower seizure count. 

plot_df_ame_trt_all <- test |>
  #mutate(effect_draws = purrr::map(purrr::map(modelfits, pluck), get_epred_emmeans)) |>
  mutate(median_ame_trt = purrr::map_dbl(purrr::map(effect_draws, ".value"), median)) |>
  arrange(median_ame_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  unnest(effect_draws)

readr::write_rds(plot_df_ame_trt_all, here::here("case-studies", "epilepsy", "results", "plot_df_ame_trt_all.rds"))

# gradient interval plot of all models, color indicates computational issues ####
plot_ame_trt_all <- ggplot(plot_df_ame_trt_all, aes(x = .value, y = model_id, color = high_pareto_ks)) + 
  stat_pointinterval(.width = c(.5, .95)) +
  #xlim(-1.3, 0.8) + 
  xlab("Average marginal effect of treatment on seizure counts") +
  geom_vline(xintercept = 0) + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.text.y=element_text(size=4),
        axis.title.y = element_blank(),
        legend.position = "none")

plot_ame_trt_all

save_tikz_plot(plot = plot_ame_trt_all, 
               width = 6.5,
               height = 8.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_ame_trt_all.tex"))
