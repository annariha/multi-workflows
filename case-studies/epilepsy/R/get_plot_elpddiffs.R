get_plot_elpddiffs <- function(df, pointsize = 3, subtitle_char = "", ylabel_char = "", fontsize=8){
  # set ggplot theme
  theme_set(theme_classic() +
              theme(legend.position = "none", 
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=fontsize),
                    plot.title = element_text(size=fontsize),
                    axis.title = element_text(size=fontsize),
                    axis.text = element_text(size=fontsize)))
  
  # prepare data for plotting 
  df_plot <- df |>
    mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
    arrange(elpd_diff) |>
    mutate(model_id = forcats::fct_inorder(model_id)) |>
    select(modelnames, family, elpd_diff, se_diff, n_high_pareto_ks, model_id, high_pareto_ks)
  
  # create plot 
  plot_elpddiffs <- ggplot(data = df_plot, aes(elpd_diff, model_id, col = high_pareto_ks, shape = family)) +
    geom_pointrange(aes(xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff), fatten = .5, size = pointsize) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    labs(subtitle = subtitle_char) + 
    xlab("$Delta widehatmathrmelpd$") +
    ylab(ylabel_char) + 
    scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) 
  
  return(plot_elpddiffs)
}