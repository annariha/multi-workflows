#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(tidyverse) 

# load helper functions
source(here::here("case-studies", "R", "save_tikz_plot.R"))

download.file("https://nsiccha.github.io/blog/posts/multiverse-birthday/data/multiverse.csv", 
              destfile=here::here("case-studies", "birthdays", "data", "data.csv"))

df <- read.csv(here::here("case-studies", "birthdays", "data", "data.csv"))

df_plot <- df |>
  mutate(divergent = ifelse(n_divergent > 0, "yes", "no")) |>
  mutate(family = factor(family)) |>
  mutate(parametrization = forcats::fct_recode(parametrization, "Adapted parameterisation" = "adapted", "Manual parameterisation" = "noncentered"))
  
df_plot_manual <- df_plot |>
  filter(parametrization == "Manual parameterisation") |>
  arrange(elpd_diff) |>
  mutate(label = forcats::fct_inorder(label)) |>
  select(label, elpd_diff, elpd_diff_se, loo_bb_weight_mean, divergent, family)

df_plot_adapted <- df_plot |>
  filter(parametrization == "Adapted parameterisation") |>
  arrange(elpd_diff) |>
  mutate(label = forcats::fct_inorder(label)) |>
  select(label, elpd_diff, elpd_diff_se, loo_bb_weight_mean, divergent, family)

df_plot_manual_loobb_remaining <- df_plot_manual |>
  filter(loo_bb_weight_mean > 0.01)

df_plot_adapted_loobb_remaining <- df_plot_adapted |>
  filter(loo_bb_weight_mean > 0.01)

# set ggplot theme
theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=7),
                  plot.title = element_text(size=7),
                  plot.subtitle = element_text(size=7),
                  axis.title = element_text(size=7),
                  axis.text = element_text(size=7),
                  strip.text = element_text(size=7)))

# plots of elpddiff for all models 
p1 = ggplot(df_plot_manual, aes(elpd_diff, label, color=divergent, shape=family)) + 
  geom_pointrange(aes(xmin=elpd_diff-elpd_diff_se, xmax=elpd_diff+elpd_diff_se), fatten = .5, size = 0.8) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  scale_color_manual(values=c("no" = "black", "yes" = "red")) + 
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) + 
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -100, -1500, -15000),
                     limits = c(-15000,3)) +
  labs(subtitle = "Manual reparameterisation") + 
  xlab("$Delta widehat textrmelpd$") +
  ylab("Models") + 
  theme(axis.text.y = element_blank(), 
        legend.position = "none")
p1

p2 = ggplot(df_plot_adapted, aes(elpd_diff, label, color=divergent, shape=family)) + 
  geom_pointrange(aes(xmin=elpd_diff-elpd_diff_se, xmax=elpd_diff+elpd_diff_se), fatten = .5, size = 0.8) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  scale_color_manual(values=c("no" = "black", "yes" = "red")) + 
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) + 
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, -5, -100, -1500, -15000),
                     limits = c(-15000,3)) +
  labs(subtitle = "Adapted reparameterisation") + 
  xlab("$Delta widehat textrmelpd$") +
  ylab("Models") + 
  theme(axis.text.y = element_blank(), 
        legend.position = "none")
p2

# plots of elpddiff for remaining models 

p3 = ggplot(df_plot_manual_loobb_remaining, aes(elpd_diff, label, color=divergent, shape=family)) + 
  geom_pointrange(aes(xmin=elpd_diff-elpd_diff_se, xmax=elpd_diff+elpd_diff_se), fatten = .5, size= 3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  scale_color_manual(values=c("no" = "black", "yes" = "red")) + 
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) + 
  xlab("$Delta widehat textrmelpd$") +
  theme(axis.title.y = element_blank(), legend.position = "none")

p3

p4 = ggplot(df_plot_adapted_loobb_remaining, aes(elpd_diff, label, color=divergent, shape=family)) + 
  geom_pointrange(aes(xmin=elpd_diff-elpd_diff_se, xmax=elpd_diff+elpd_diff_se), fatten = .5, size= 3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  scale_color_manual(values=c("no" = "black", "yes" = "red")) + 
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) + 
  xlab("$Delta widehat textrmelpd$") +
  theme(axis.title.y = element_blank(), legend.position = "none")

p4

# plots of LOO-BB weights of remaining models  

p5 <- ggplot(df_plot_manual_loobb_remaining, aes(x = loo_bb_weight_mean, y = label, col = divergent, fill = divergent, shape=family)) +
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$w tiny LOO-BB$") +
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  scale_shape_manual(values=c("normal" = 21, "student's t" = 24, "RHS" = 23)) + 
  scale_fill_manual(values = c("yes" = "red", "no" = "black")) +
  theme(axis.title.y = element_blank(), legend.position = "none")

p5

p6 <- ggplot(df_plot_adapted_loobb_remaining, aes(x = loo_bb_weight_mean, y = label, col = divergent, fill = divergent, shape=family)) +
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  xlab("$w tiny LOO-BB$") +
  scale_color_manual(values=c("yes" = "red", "no" = "black")) + 
  scale_shape_manual(values=c("normal" = 21, "student's t" = 24, "RHS" = 23)) + 
  scale_fill_manual(values = c("yes" = "red", "no" = "black")) +
  theme(axis.title.y = element_blank(), legend.position = "none")

p6 

# combine everything 
plot_elpddiffs_loobb_combined <- (p1 | p3 | p5) / (p2 | p4 | p6)
plot_elpddiffs_loobb_combined

# save as tikzpicture 
save_tikz_plot(plot = plot_elpddiffs_loobb_combined,
               width = 6,
               #height = 5.8,
               filename = here::here("case-studies", "birthdays", "figures", "plot_birthdays_elpddiff_loobb_combined.tex"))