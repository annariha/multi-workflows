`epilepsy/epi-1`: 

Iterative filtering of multiple candidate models for analysing seizure counts and treatment effects of an anti-convulsant therapy with data `brms::epilepsy` available in the [brms](https://paul-buerkner.github.io/brms/) package by Paul-Christian Bürkner. 
The data contains $236$ observations of a randomised controlled trial of patients with epilepsy. 
It was initially published by [Leppik et al. (1987)](https://n.neurology.org/content/37/6/963), and previously analysed, for example, by [Thall and Vail (1990)](https://pubmed.ncbi.nlm.nih.gov/2242408/) and [Breslow and Clayton (1993)](https://www.jstor.org/stable/2290687).

`epilepsy/epi-2`: 

The second part of the epilepsy case study extends the filtered set of models from `epi-1` by including varying effects on the level of each visit, patient and observation. 
Previously, with help of posterior predictive checking, we were able to avoid resolving computational issues in estimation of expected log predictive densities (elpd) for some models. 
With the extended set of models, iteration towards improved reliability of estimates for elpd is obligatory, as conclusions about the filtered set of models are affected.

# Set up

To activate the profile for the two case studies using the epilepsy dataset run `renv::activate(profile = "epilepsy")`and check that R is restarting and that you see a message similar to 
```
- Project '.../multi-workflows' loaded. [renv 1.0.5; using profile 'epilepsy']
```
If more packages need to be installed, you will see a message about this and can check `renv::status()` which will provide the instructions for installing the missing packages. 

# Running

To get all results required as inputs for the scripts in `epi-1` and `epi-2`, run 
```
sh get_all_results.sh 
```

# Plotting

For `epi-1`, posterior results for the coefficient of treatment are visualised with `02_plot_posterior_trt_epi1.R` in Figure 3. 
`03_plot_elpddiffs_epi1.R` is used to obtain plots of differences in elpd in all models and filtered set of models in Figure 4. 
`05_plot_ppc_ecdf_21_22_epi1.R` provides an example of visual PPC using ECDF plots for Model 21 and Model 22 in Figure 5. 

For `epi-2`, we obtain plots of the posterior results for the coefficient of treatment in Figure 6 by first creating data frames for plotting in `02_plot_dfs_posterior_trt_epi2.R` and then building the plots in `03_plot_posterior_trt_epi2.R`. 
`04_plot_elpddiffs_epi2.R` is used to visualise the results for differences in elpd in Figure 7. 

# Additional Targets 

In `epi-1`, we obtain ECDF plots for all models with `04_plot_ppc_ecdf_all_epi1.R` (see, e.g., Figure 13). 
`06_plot_conditional_effects.R` creates the conditional effects plot for all models including an interaction effect in Figure 14.

In `epi-2`, `05_inspect_pointwise_elpds_epi2.R` analyses point-wise elpds and creates Figure 15. 