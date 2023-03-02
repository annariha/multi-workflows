# Plots: visualise treatment/control group and visit, patient level info ####

dat <- brms::epilepsy

# Why random effect based on visits and patients?
plot_patient_visit_count <- 
  dat |> 
  mutate(treatment = ifelse(Trt == 0, "no treatment", "treatment")) |>
  ggplot(aes(x = visit, y = count, group = patient, color = patient)) +
  geom_point() +
  geom_line() +
  ylab("seizure count") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~treatment)

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_patient_visit_count_epi.png"), 
          plot_patient_visit_count)

# Why add an interaction effect? 
plot_base_count <-
  dat |> 
  mutate(treatment = ifelse(Trt == 0, "no treatment", "treatment")) |>
  ggplot(aes(x = Base)) +
  geom_point(aes(y = count, color = patient), size=0.8) +
  scale_x_continuous("base seizure count") +
  ylab("seizure count") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~treatment)

save_plot(here::here("case-studies", "epilepsy", "figures", "plot_base_count_epi.png"), 
          plot_base_count)