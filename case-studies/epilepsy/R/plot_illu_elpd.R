#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, gtools, ggtext, latex2exp)

# create data
set.seed(42)
mean_elpd_diff <- c(-10, -8, -5.5, -5, -4.8, -4.7, -3.66, -1.5, -0.5, -0.2, -0.1, -0.05, 0)
se_elpd <- abs(rnorm(n=NROW(mean_elpd_diff)-1, mean = 0.8, sd = 0.5))
models <- factor(seq(1:NROW(mean_elpd_diff)))  

plot_df <- tibble(
  Models = factor(as.character(models), levels = mixedsort(as.character(models))), 
  mean_elpd_diff, 
  se_diff = c(se_elpd, 0)) 

plot_illu_elpd <- ggplot(data = plot_df, aes(x = mean_elpd_diff, y = Models)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbar(aes(xmin=mean_elpd_diff-se_diff, xmax=mean_elpd_diff+se_diff), width=.2) + 
  xlab("Difference in elpd") + 
  annotate("rect", xmin = -2.4, xmax = 2.1, ymin = 8.5, ymax = 13.5, alpha = .2) +
  #ggtext::geom_richtext(x = 2.5, y = 11, label = "indistinguishable", angle=90) +
  annotate("label", x = -0.15, y = 13.6, label = "indistinguishable models", size=3) +
  scale_y_discrete(expand = expansion(mult=c(0.05,0.1))) +
  theme_bw()

cowplot::save_plot(here::here("case-studies", "epilepsy", "figures", "plot_illu_elpd.png"), plot_illu_elpd)
