# gradient interval plot of all models, color indicates computational issues ####
get_plot_posterior_trt_coeffs <- function(df){
  
  # set ggplot theme
  theme_set(theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank()))
  
  plot_posterior_trt_coeffs <- ggplot(df, aes(x = posterior_draws_trt, y = model_id)) + 
    stat_pointinterval(.width = c(.5, .95)) +
    xlab("Coefficient for treatment") +
    geom_vline(xintercept = 0) + 
    #scale_color_manual(values=c("yes" = "red", "no" = "black")) +  
    theme(axis.text.y = element_text(size=4),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  return(plot_posterior_trt_coeffs)
}