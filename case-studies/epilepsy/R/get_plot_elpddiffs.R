get_plot_elpddiffs <- function(df, subtitle_char = "", ylabel_char = ""){
  # set ggplot theme
  theme_set(theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank()))
  
  # prepare data for plotting 
  df_plot <- df |>
    arrange(elpd_diff) |>
    mutate(model_id = forcats::fct_inorder(model_id)) |>
    select(modelnames, family, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks)
  
  # create plot 
  plot_elpddiffs <- ggplot(data = df_plot, aes(elpd_diff, model_id, col = high_pareto_ks, shape = family)) +
    geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    labs(subtitle = subtitle_char) + 
    xlab("$Delta widehat textrmelpd$") +
    ylab(ylabel_char) + 
    scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6))
  
  return(plot_elpddiffs)
}