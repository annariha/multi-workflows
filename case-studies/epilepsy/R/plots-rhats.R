# load dataframe of combinations
comb_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "comb_df_without_obs.rds"))

# create df for plotting 
rhat_trt_df <- 
  tibble(model_name = comb_df$model_name, rhats_raw = purrr::map(comb_df$model_fit, get_rhat_trt)) |>
  unnest(rhats_raw) |>
  group_by(model_name) |>
  mutate(key = row_number()) |>
  spread(key, rhats_raw) |>
  rename(rhat_Trt = 2, rhat_zBaseTrt = 3) |>
  # high rhat for treatment
  mutate(high_rhat_Trt = ifelse(rhat_Trt > 1.01, 1, 0),
         high_rhat_zBaseTrt = ifelse(rhat_zBaseTrt > 1.01, 1, 0))

# Plots: visual inspection of convergence checks (here: Rhat) for treatment var ####
plot_rhats_trt <- 
  ggplot(data = rhat_trt_df, aes(rhat_Trt, model_name)) + 
  geom_point() + 
  geom_vline(xintercept = 1.01, linetype="dotted") + 
  ggtitle(paste0("All models (k=", NROW(rhat_trt_df), ")")) + 
  theme_bw() + 
  theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_rhats_epi.png"), 
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

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_filter_rhats_epi.png"), 
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

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_high_rhats_epi.png"), 
          plot_high_rhats_trt)