#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, ggdist, patchwork)

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

# load 
df <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "full_df_elpddiff_pbma.rds")) 

# create df 
# we are only interested in reliable elpd estimates and thus choose the elpd results obtained with a combination of integrated LOO and default LOO
df_intloo <- df |>
  mutate(modelnames = rownames_to_column(df)) |>
  # filter for results with improved computation for reliable elpd estimates
  filter(loo_computation == "integrated LOO")

readr::write_rds(df_intloo, here::here("case-studies", "epilepsy", "results", "df_elpddiff_pbma_intloo.rds"))

# load (again)
df_intloo <- readr::read_rds(here::here("case-studies", "epilepsy", "results", "df_elpddiff_pbma_intloo.rds"))

# filter for best models 
df_intloo_best <- df_intloo |>
  mutate(modelnames = rownames_to_column(df_intloo)) |>
  # 1. filter for models with highest elpd (and reliable elpd estimates) 
  #filter(elpd_diff + 1.96*se_diff >= 0) |>
  #filter(elpd_diff >= -4) |> 
  filter(elpd_diff + se_diff >= 0) |>
  # 2. from these models, pick neg Binomial models 
  filter(family == "negbinomial") |>
  # 3. from these models, pick the most complex models 
  filter(nvars == max(nvars))

# adjusted from: https://stackoverflow.com/questions/35553244/count-leading-zeros-between-the-decimal-point-and-first-nonzero-digit
get_n_zero_digits <- function(x) {
  # this used to only work for up to 7 digits being zero bc of all.equal()
  # works when comparing with "==" 
  if (isTRUE(round(x) == x)){
    final = 0 # as y would be -Inf for integer values
    return(final)
    break
  }
  y <- log10(abs(x)-floor(abs(x)))   
  final <- ifelse(isTRUE(all.equal(round(y),y)), round(-y-1), -ceiling(y)) # corrects case ending with ..01
  return(final)
}

# create df for table 
table_df_intloo_best <- df_intloo_best |>
  mutate(n_zero_digits = purrr::map_dbl(mcse_elpd_loo, get_n_zero_digits)) |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(model_id, n_high_pareto_ks, mcse_elpd_loo, n_zero_digits, elpd_loo, se_elpd_loo, elpd_diff, se_diff)
  # adjust by MCSE of elpd 

# create df for plotting 
plot_df_trt_intloo_best <- df_intloo_best |>
  mutate(high_pareto_ks = ifelse(n_high_pareto_ks > (NROW(brms::epilepsy) / 100) * 5, "yes", "no")) |>
  select(draws_df, family, model_id, high_pareto_ks, loo_computation) |>
  mutate(posterior_draws_trt = purrr::map(purrr::map(draws_df, purrr::pluck), "b_Trt1" )) |>
  mutate(median_post_trt = purrr::map_dbl(posterior_draws_trt, median)) |>
  arrange(median_post_trt) |>
  mutate(model_id = forcats::fct_inorder(model_id)) |>
  select(posterior_draws_trt, median_post_trt, model_id, family, high_pareto_ks) |>
  unnest(posterior_draws_trt)

readr::write_rds(plot_df_trt_intloo_best, here::here("case-studies", "epilepsy", "results", "plot_df_trt_intloo_best.rds"))

# create plot 
plot_posterior_trt_intloo_best <- ggplot(plot_df_trt_intloo_best, aes(x = posterior_draws_trt, y = model_id, color = high_pareto_ks)) + 
  stat_halfeye(.width=c(0.5, 0.95), shape=1) +
  #xlim(-1.15, 0.55) + 
  xlab("Coefficient for treatment") +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_trt_intloo_best

# save as tikz ####
save_tikz_plot(plot = plot_posterior_trt_intloo_best, 
               width = 5.5,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_posterior_trt_coeff_intloo_best.tex"))
