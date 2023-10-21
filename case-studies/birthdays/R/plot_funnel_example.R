library(ggplot2)
library("rjson")

# load helper functions
source(here::here("case-studies", "R", "save_tikz_plot.R"))

download.file("https://nsiccha.github.io/blog/posts/multiverse-birthday/samples/birthday/0_0_0_0_0_3_2_bias.json", destfile="data.json")
bias <- fromJSON(file="data.json")

df1 = data.frame(
  group=rep("manual", 1000),
  log_scale=bias$draws$noncentered[[1]],
  treatment=bias$draws$noncentered[[2]],
  divergent=bias$divergent$noncentered
)

df2 = data.frame(
  group=rep("adapted", 1000),
  log_scale=bias$draws$posthoc[[1]],
  treatment=bias$draws$posthoc[[2]],
  divergent=bias$divergent$posthoc
)

df = rbind(df1, df2)

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

parameterisation_names <- c("manual" = "Manual parameterisation", 
                            "adapted" = "Adapted parameterisation")

plot_birthdays_funnel_example <- ggplot(df, aes(treatment, log_scale, color=as.factor(divergent))) + 
  geom_point(size=1, alpha=0.3) + 
  scale_color_manual(values=c("0" = "black", "1" = "red")) + 
  xlab("Effect of Halloween") + 
  ylab("log local shrinkage") +
  facet_grid(~factor(group, levels=c("manual", "adapted")), labeller = as_labeller(parameterisation_names)) +
  theme(legend.position = "none") +
  theme(panel.spacing.x = unit(1, "lines"))

plot_birthdays_funnel_example

# save as tikz ####
save_tikz_plot(plot = plot_birthdays_funnel_example, 
               width = 5,
               filename = here::here("case-studies", "birthdays", "figures", "plot_birthdays_funnel_example.tex"))
