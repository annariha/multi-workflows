get_one_ecdf_overlay <- function(df, y, model_char = "", fontsize=8){
  # set ggplot theme
  theme_set(theme_classic() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=fontsize),
                    plot.title = element_text(size=fontsize),
                    axis.title = element_text(size=fontsize),
                    axis.text = element_text(size=fontsize)))
  
  # bayesplot colour scheme
  bayesplot::color_scheme_set("gray")
  
  # get predictions for one model 
  yrep <- df |>
    filter(model_id == model_char) |>
    pull(ypred)
  
  # get model family
  modelfamily <- df |>
    filter(model_id == model_char) |>
    mutate(family = recode(family, "poisson" = "Poisson", "negbinomial" = "Negative Binomial")) |>
    pull(family)
  
  # get model name 
  modelname_long <- df |>
    filter(model_id == model_char) |>
    pull(modelnames)
  
  # remove info on prior for plotting 
  modelname <- substr(modelname_long,1,regexpr(",",modelname_long)-1)
  
  # create plot
  plot <- ppc_ecdf_overlay(y = y, yrep = yrep[[1]][1:100,], discrete = TRUE) +
    #scale_x_continuous(limits=c(0,110)) +
    scale_x_continuous(trans="pseudo_log", 
                       breaks=c(0, 5, 20, 50, 100), 
                       limits=c(0,110)) +
    labs(title = paste0(modelfamily))
  
  return(plot)
}