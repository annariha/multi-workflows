#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(tidyverse) 
library(ggdist)
library(rjson)

# load helper functions
source(here::here("R", "save_tikz_plot.R"))

#download.file("https://nsiccha.github.io/blog/posts/multiverse-birthday/data/multiverse.csv", destfile=here::here("case-studies", "birthdays", "data", "data.csv"))

# plot theme 
theme_set(theme_classic() +
            theme(legend.position = "none", 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  text = element_text(size=8),
                  plot.title = element_text(size=8),
                  axis.title = element_text(size=8),
                  axis.text = element_text(size=8)))
pointsize <- 4

# download data 
download.file("https://users.aalto.fi/~sicchan1/multiverse/multiverse.csv", destfile=here::here("case-studies", "birthdays", "data", "multiverse.csv"))
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_2_2_noncentered_qoi.json", destfile=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_2_2_noncentered_qoi.json"))
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_3_2_noncentered_qoi.json", destfile=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_3_2_noncentered_qoi.json"))
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_2_2_posthoc_qoi.json", destfile=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_2_2_posthoc_qoi.json"))
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_3_2_posthoc_qoi.json", destfile=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_3_2_posthoc_qoi.json")) 

# load data 
df <- read.csv(here::here("case-studies", "birthdays", "data", "multiverse.csv"))

# for plot with all models 
df_p1 <- df |>
  filter(parametrization == "adapted")

# with adapted parameterisation 
list_adapted_142 <- fromJSON(file=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_2_2_posthoc_qoi.json"))
list_adapted_144 <- fromJSON(file=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_3_2_posthoc_qoi.json"))
treatment_adapted_142 <- list_adapted_142[[3]]
treatment_adapted_144 <- list_adapted_144[[3]]
df_adapted_142 <- data.frame(model_id = rep("Model 142", NROW(treatment_adapted_142)), 
                     parametrization = rep("adapted", NROW(treatment_adapted_142)),
                     family = rep("student's t", NROW(treatment_adapted_142)),
                     treatment = list_adapted_142[[3]])
df_adapted_144 <- data.frame(model_id = rep("Model 144", NROW(treatment_adapted_144)),
                     parametrization = rep("adapted", NROW(treatment_adapted_144)),
                     family = rep("RHS", NROW(list_adapted_144[[3]])), 
                     treatment = list_adapted_144[[3]])
df_treatment_adapted <- rbind(df_adapted_142, df_adapted_144)
#df_plot <- df |> mutate(parametrization = forcats::fct_recode(parametrization, "Adapted parameterisation" = "adapted", "Manual parameterisation" = "noncentered"))

# with manual reparameterisation 
list_manual_142 <- fromJSON(file=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_2_2_noncentered_qoi.json"))
list_manual_144 <- fromJSON(file=here::here("case-studies", "birthdays", "data", "32_16_32_1_1_3_2_noncentered_qoi.json"))
treatment_manual_142 <- list_manual_142[[3]]
treatment_manual_144 <- list_manual_144[[3]]
df_manual_142 <- data.frame(model_id = rep("Model 142", NROW(treatment_manual_142)), 
                             parametrization = rep("manual", NROW(treatment_manual_142)),
                             family = rep("student's t", NROW(treatment_manual_142)),
                             treatment = list_manual_142[[3]])
df_manual_144 <- data.frame(model_id = rep("Model 144", NROW(treatment_manual_144)),
                             parametrization = rep("manual", NROW(treatment_manual_144)),
                             family = rep("RHS", NROW(treatment_manual_144)), 
                             treatment = list_manual_144[[3]])
df_treatment_manual <- rbind(df_manual_144, df_manual_142)
#df_treatment_manual 

p1 = ggplot(df_p1, aes(x=treatment_mean, y=reorder(label, treatment_q50), shape=family)) + 
  geom_pointrange(aes(xmin=treatment_q25, xmax=treatment_q75), fatten = .1, size = pointsize, linewidth=1) +
  geom_pointrange(aes(xmin=treatment_q2.5, xmax=treatment_q97.5), fatten = .1, size = pointsize) +
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
  xlab("Halloween effect") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p1

p2 = ggplot(df_treatment_adapted, aes(x=treatment, y=model_id, shape=family)) + 
  stat_halfeye(.width=c(0.5, 0.95)) +
  ggtitle("Adapted reparameterisation") +
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
  xlim(c(-.3, +.1)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p2

p3 = ggplot(df_treatment_manual, aes(x=treatment, y=reorder(model_id, treatment, FUN = median), shape=family)) + 
  stat_halfeye(.width=c(0.5, 0.95)) +
  ggtitle("Manual reparameterisation") +
  scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
  xlab("Halloween effect") +
  xlim(c(-.3, +.1)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme(axis.text.y = element_text(size=8), 
        axis.title.y = element_blank(),
        legend.position = "none")

plot_posterior_halloween_effect <- p1 | (p2 / p3)
plot_posterior_halloween_effect

# preprint format 
save_tikz_plot(plot = plot_posterior_halloween_effect,
               width = 5.5,
               filename = here::here("case-studies", "birthdays", "figures", "plot_birthdays_posterior_trt.tex"))

# submission format 
save_tikz_plot(plot = plot_posterior_halloween_effect,
               width = 4.75,
               filename = here::here("case-studies", "birthdays", "figures", "submission", "plot_birthdays_posterior_trt.tex"))