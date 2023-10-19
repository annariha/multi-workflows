get_plot_loo_bb_weights <- function(df, subtitle_char = ""){
  # set ggplot theme
  theme_set(theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=7),
                    plot.title = element_text(size=7),
                    axis.title = element_text(size=7),
                    axis.text = element_text(size=7)))
  
  # prepare data for plotting 
  plot_df <- df |>
    mutate(high_pareto_ks = ifelse(n_high_pareto_ks >= 1, "yes", "no")) |>
    arrange(elpd_diff) |>
    mutate(model_id = forcats::fct_inorder(model_id)) |>
    select(modelnames, family, elpd_diff, se_diff, loo_bb_weight, n_high_pareto_ks, model_id, high_pareto_ks)
  
  plot_pbma_plus_weights <- ggplot(plot_df, aes(x = loo_bb_weight, y = model_id, col = high_pareto_ks, fill = high_pareto_ks, shape = family)) +
    geom_point() + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    labs(subtitle = subtitle_char) + 
    xlab("$w tiny LOO-BB$") +
    scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_shape_manual(values=c("poisson" = 21, "negbinomial" = 25)) +
    scale_fill_manual(values = c("yes" = "red", "no" = "black")) +
    theme(axis.title.y = element_blank(), legend.position = "none")
}