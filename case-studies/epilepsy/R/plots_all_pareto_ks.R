# extract Pareto k's for all models
pareto_ks <- purrr::map(loos, ~.x$diagnostics$pareto_k)

# Plots: visualise Pareto k's for all models ####
plot_all_pareto_ks <- tibble(model_name = names(all_pareto_ks), pareto_k = all_pareto_ks) %>%
  mutate(sum_pareto_k = purrr::map_dbl(pareto_k, get_sum_high_ks)) %>% 
  arrange(sum_pareto_k) %>%
  mutate(model_name = forcats::fct_inorder(model_name)) %>%
  unnest(pareto_k) %>%
  {ggplot(., aes(x = pareto_k, y = model_name)) + 
      geom_point(shape = 3, color = "grey20") +
      geom_vline(xintercept = 0.7) +
      theme_bw() + 
      theme(axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"))}

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_all_pareto_ks_epi.png"), 
          plot_all_pareto_ks, 
          base_height = 19, 
          base_aspect_ratio = 1.5)