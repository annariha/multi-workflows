# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, haven, tictoc, knitr, tidyverse, tidybayes, brms, bayesplot, cowplot, loo, purrr, multiverse)

# load data 
path <- here::here("Data", "159422-V2", "pnas_povreductioneeg.dta")
data_eeg <- read_dta(path)

# data without NAs in absalpha
withoutNA <- !is.na(data_eeg$absalpha)
data_eeg <- data_eeg[withoutNA,]

# helper functions (from Andrews code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# create multiverse
# 1. different pre_score definitions
# 2. contrasts as outcome 
# 3. include different variables
# 4. normal model vs. log-normal model

M = multiverse()

inside(M, {
  
  # preprocessing (from Andrews code)
  df <- data_eeg %>%
    mutate(
      girl = as.numeric(cfemalea0),
      birthweight = as.numeric(cweightlba0_r),
      gestage = as.numeric(cgestagewksa0_r),
      momedu = mean_impute(as.numeric(momeduyrsa0)),
      income = mean_impute(as.numeric(hhrevisedincomea0)),
      white = as.numeric(mracea0) == 1,
      black = as.numeric(mracea0) == 2,
      momhealth = as.numeric(mgoodhealtha0),
      smoking = as.numeric(mcigduringavgwka0_r),
      drinking = as.numeric(malcduringavgwka0_r)
    )
  
  # 1. different pre_score definitions - pre_score1 from Andrews code
  df <- df %>%
    mutate(pre_score = branch(pre_score_calculation,
                    "pre_score1" ~ standardize(birthweight) + standardize(gestage) + 
                      standardize(momedu) + standardize(income) + white - black + 
                      standardize(momhealth) - standardize(smoking) - standardize(drinking),
                    "pre_score2" ~ standardize(momedu) + standardize(income) + white - black,
                    "pre_score3" ~ standardize(birthweight) + standardize(gestage) + 
                      standardize(momhealth) - standardize(smoking) - standardize(drinking)))
  
  # 2. contrasts as alternative outcome 
  # 3. include different variables: treat, pre_score, girl 
  # 4. normal vs. log-normal model 
  mod <- brm(formula = 
               branch(outcome,
                      "abs" ~ absalpha
                      #,"contrast" ~ log(absalpha) + log(absgamma) - log(abstheta)
                      ) ~ 
               branch(predictors, 
                      "eq_1" ~ treat,
                      "eq_2" ~ treat + pre_score,
                      "eq_3" ~ treat + pre_score + girl,
                      "eq_4" ~ treat + girl + birthweight + gestage + 
                        momedu + income + white + black + momhealth + 
                        smoking + drinking),
             data = df, 
             family = 
               branch(obs_model, 
                      "normal" ~ gaussian(), 
                      "lognormal" ~ lognormal()))
  
})

# check multiverse settings
expand(M)

# execute entire multiverse analysis
tic()
execute_multiverse(M)
toc()

# access results 
multiverse_table <- multiverse::expand(M) %>% 
  extract_variables(mod)

# save results 
saveRDS(multiverse_table, "multiverse-ex1.rds")

# access list of brmsfits (mod)
d <- multiverse_table %>%
  select(pre_score_calculation, predictors, obs_model, mod) %>%
  arrange(predictors)

# plot all posterior results from list of models 
do.call(compare_posteriors, fits_list)

# vector of obs. outcome values alpha
y_alpha <- as.vector(data_eeg$absalpha)

# posterior predictive checks
plot_post <- function(postarray, printit = FALSE, prob_val = 0.8, prob_outer_val = 0.99){
  
  color_scheme_set("green")
  
  plot1 <- mcmc_intervals(postarray, prob = prob_val, prob_outer = prob_outer_val)
  plot2 <- mcmc_areas(postarray, pars = c("treat"), prob = prob_val, prob_outer = prob_outer_val)
  
  # add the title
  title <- ggdraw() + 
    draw_label(
      "Posterior distributions with median and 80% credible intervals",
      x = 0,
      hjust = 0,
      fontfamily = "serif",
      size = 11,
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 4)
    )
  
  plot_row  <- plot_grid(plot1, plot2,
                         ncol = 1, align = "hv", 
                         labels = "AUTO",
                         label_fontfamily = "serif",
                         label_size = 12,
                         label_x = 0, label_y = 0, # to put labels on the bottom
                         hjust = -0.5, vjust = -0.5) # to put labels on the bottom
  
  post_pred_plot <- plot_grid(title, plot_row, ncol = 1,  rel_heights = c(0.1, 1))
  
  # option to print & save or only save
  if (printit == TRUE) {
    ggsave("post_pred_plot.png")
    return(post_pred_plot)
    }else{
    ggsave("post_pred_plot.png")
    }
}

# example plot 
ex_post <- d$postarray[[10]]
plot_post(ex_post, printit = TRUE)

# plot all posterior distr for treat

# add grouping: simple, medium, complex model 

plot_all_treat <- function(postarray, printit = FALSE){}

# compare all models to 
# 1. model average 
# 2. encompassing model

# instead of log(outcome) - use log_normal()
# models are not nested anymore BUT target is on same scale 

# focus on log_normal model as normal model is probably not true
