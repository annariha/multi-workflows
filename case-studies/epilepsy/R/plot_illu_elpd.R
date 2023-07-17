#! /usr/bin/Rscript --vanilla

# setup ####
# setup ####
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, tidyverse, ggplot2, patchwork)

# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank()))

# load scripts
source(here::here("case-studies", "epilepsy", "R", "save_tikz_plot.R"))

# create data
set.seed(42)
mean_elpd_diff <- c(-105, -100, -80, -78, -55, -50, -48, -47, -30, -15, -5, -2, 0)
se_elpd <- abs(rnorm(n=NROW(mean_elpd_diff)-1, mean = 30, sd = 10))
models <- factor(seq(1:NROW(mean_elpd_diff)))  

plot_df <- tibble(
  Models = factor(as.character(models), levels = gtools::mixedsort(as.character(models))), 
  mean_elpd_diff, 
  se_diff = c(se_elpd, 0)) 

plot_illu_elpd <- ggplot(data = plot_df, aes(x = mean_elpd_diff, y = Models)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin=mean_elpd_diff-se_diff, xmax=mean_elpd_diff+se_diff), shape=1) + 
  xlab("Difference in elpd") + 
  annotate("rect", xmin = -82, xmax = 52, ymin = 8.5, ymax = 13.4, alpha = .2) +
  #ggtext::geom_richtext(x = 2.5, y = 11, label = "indistinguishable", angle=90) +
  annotate("label", x = -55, y = 13.2, label = "indistinguishable models", size=3) +
  scale_y_discrete(expand = expansion(mult=c(0.05, 0.07))) +
  theme(axis.text.y = element_blank())

plot_illu_elpd

save_tikz_plot(plot = plot_illu_elpd, 
               width = 6,
               filename = here::here("case-studies", "epilepsy", "figures", "plot_illu_elpd.tex")
)