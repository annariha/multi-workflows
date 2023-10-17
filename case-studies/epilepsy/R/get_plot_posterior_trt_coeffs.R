get_plot_posterior_trt_coeffs <- function(df, ytextsize = 8){
  # gradient interval plot of all models in df, color indicates computational issues
  
  # set ggplot theme
  theme_set(theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=8),
                    plot.title = element_text(size=8),
                    axis.title = element_text(size=8),
                    axis.text = element_text(size=8)))
  
  plot_posterior_trt_coeffs <- ggplot(df, aes(x = posterior_draws_trt, y = model_id)) + 
    stat_pointinterval(.width = c(.5, .95)) +
    xlab("Coefficient for treatment") +
    geom_vline(xintercept = 0, linetype = "longdash") + 
    #scale_color_manual(values=c("yes" = "red", "no" = "black")) +  
    theme(axis.text.y = element_text(size=ytextsize),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  return(plot_posterior_trt_coeffs)
}