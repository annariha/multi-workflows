# function to create plots of elpd diff, se diff, and LOO-BB and pseudo-BMA weights 
# input: comparison df for all models for the same dataset

get_plot_elpddiffs_loobb <- function(df){
  # requires: tidyverse, ggplot2, patchwork
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
  
  # prepare data for plotting 
  df_plot <- as.data.frame(df) |>
    rownames_to_column(var = "model_name") |>
    mutate(high_pareto_ks = ifelse(n_high_pareto_ks > 0, "yes", "no")) |>
    arrange(elpd_diff) |>
    mutate(model_name = forcats::fct_inorder(model_name)) |>
    select(model_name, elpd_diff, se_diff, loo_bb_weight, pbma_weight, n_high_pareto_ks, high_pareto_ks)
  
  # create elpddiffs plot 
  plot_elpddiffs <- ggplot(data = df_plot, aes(elpd_diff, model_name, col = high_pareto_ks)) +
    geom_pointrange(aes(xmin=elpd_diff-2*se_diff, xmax=elpd_diff+2*se_diff), fatten = .5, size = 1.5) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    xlab(unname(TeX("$\\Delta \\widehat{elpd}$"))) +
    # xlab("$\\Delta{\\widehat{\\mathrm{elpd}}}$") + 
    scale_color_manual(values=c("yes" = "red", "no" = "black"))+
    theme(axis.title.y = element_blank(), legend.position = "none")
  
  plot_elpddiffs <- ggExtra::ggMarginal(plot_elpddiffs, 
                                        type = "histogram", 
                                        margins = "x", 
                                        size=8, 
                                        xparams = list(bins=50))
  
  # create LOO-BB plot 
  plot_loobb <- ggplot(df_plot, aes(x = loo_bb_weight, y = model_name, col = high_pareto_ks, fill = high_pareto_ks)) +
    geom_point() + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    xlab(unname(TeX("$w_{\\tiny LOO-BB}$"))) +
    #xlab("$w_{\\text{tiny LOO-BB}}$") +
    scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_fill_manual(values = c("yes" = "red", "no" = "black")) +
    #geom_rug(sides="t") +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")
  
  plot_loobb <- ggExtra::ggMarginal(plot_loobb, 
                                    type = "histogram", 
                                    margins = "x", 
                                    size=8, 
                                    xparams = list(bins=50))
  
  # create pseudo-BMA plot 
  plot_pbma <- ggplot(df_plot, aes(x = pbma_weight, y = model_name, col = high_pareto_ks, fill = high_pareto_ks)) +
    geom_point() + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    xlab(unname(TeX("$w_{\\tiny PBMA}$"))) +
    #xlab("$w_{\\text{tiny PBMA}}$") +
    scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
    scale_fill_manual(values = c("yes" = "red", "no" = "black")) +
    #geom_rug(sides="t") +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")
  
  plot_pbma <- ggExtra::ggMarginal(plot_pbma, 
                                    type = "histogram", 
                                    margins = "x", 
                                    size=8, 
                                    xparams = list(bins=50))
    
  # create LOO-BB vs pseudo-BMA plot 
  plot_pbma_vs_loobb <- ggplot(df_plot, aes(x = loo_bb_weight, y = pbma_weight)) + 
    geom_point() + 
    geom_abline() + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    xlab(unname(TeX("$w_{\\tiny LOO-BB}$"))) +
    #xlab("$w_{\\text{tiny LOO-BB}}$") +
    ylab(unname(TeX("$w_{\\tiny PBMA}$")))
    #ylab("$w_{\\text{tiny PBMA}}$")
  
  # combine the plots 
  #plot_joint <- (plot_elpddiffs | plot_loobb | plot_pbma) / plot_pbma_vs_loobb
  
  plot_top_row <- cowplot::plot_grid(plot_elpddiffs, plot_loobb, plot_pbma, ncol=3, nrow=1, rel_widths = c(1.8,1,1)) 
  plot_joint <- cowplot::plot_grid(plot_top_row, plot_pbma_vs_loobb, ncol=1, nrow=2)
  
  return(plot_joint)
}