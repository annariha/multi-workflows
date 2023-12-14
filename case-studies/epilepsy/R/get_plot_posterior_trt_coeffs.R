get_plot_posterior_trt_coeffs <- function(df, alpha=1, pointsize=3, ytextsize=7){
  # gradient interval plot of all models in df
  
  # set ggplot theme
  theme_set(theme_classic() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=7),
                    plot.title = element_text(size=7),
                    axis.title = element_text(size=7),
                    axis.text = element_text(size=7)))
  
  plot_posterior_trt_coeffs <- ggplot(df, aes(x = posterior_draws_trt, y = model_id, shape = family)) + 
    stat_pointinterval(.width = c(.5, .95), alpha=alpha, point_size = pointsize) +
    xlab("Coefficient for treatment") +
    geom_vline(xintercept = 0, linetype = "longdash") + 
    #scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_shape_manual(values=c("poisson" = 1, "negbinomial" = 6)) +
    theme(axis.text.y = element_text(size=ytextsize),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  return(plot_posterior_trt_coeffs)
}