# Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis

This repository contains the code for the case studies in "Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis" by Anna Elisabeth Riha, Nikolas Siccha, Antti Oulasvirta and Aki Vehtari.
The paper is available as a [preprint on arXiv](https://arxiv.org/abs/2404.01688). 

We illustrate the suggested iterative filtering approaches in the following `case-studies`:

- `epilepsy/epi-1`: 

Iterative filtering of multiple candidate models for analysing seizure counts and treatment effects of an anti-convulsant therapy with data `brms::epilepsy` available in the [brms](https://paul-buerkner.github.io/brms/) package by Paul-Christian Bürkner. 
The data contains $236$ observations of a randomised controlled trial of patients with epilepsy. 
It was initially published by [Leppik et al. (1987)](https://n.neurology.org/content/37/6/963), and previously analysed, for example, by [Thall and Vail (1990)](https://pubmed.ncbi.nlm.nih.gov/2242408/) and [Breslow and Clayton (1993)](https://www.jstor.org/stable/2290687).

- `epilepsy/epi-2`: 

The second part of the epilepsy case study extends the filtered set of models from `epi-1` by including varying effects on the level of each visit, patient and observation. 
Previously, with help of posterior predictive checking, we were able to avoid resolving computational issues in estimation of expected log predictive densities (elpd) for some models. 
With the extended set of models, iteration towards improved reliability of estimates for elpd is obligatory, as conclusions about the filtered set of models are affected.

- `birthdays`: 

Iterative filtering of models for the registered number of daily births in the USA using natality data from the National Vital Statistics System provided by Google BigQuery and exported by Chris Mulligan and Robert Kern. 
The data contains $7305$ observations of the number of births with the corresponding day, month, and year as well as day of the year and day of the week in the USA from $01.01.1969$ to $31.12.1988$.

Each case study has subfolders, for example, for scripts or inputs. 
Scripts and helper functions used for both `epi-1` and `epi-2` are located in `case-studies/epilepsy/R`. 
The `README.md` files in `case-studies/epilepsy` and `case-studies/birthdays` explain in detail how to setup and run each of the case studies. 

## Environments and profiles

This repository is using the [renv](https://rstudio.github.io/renv/articles/renv.html) package by Kevin Ushey and Hadley Wickham for creating a reproducible environment. 
After cloning the repository, install the required version of renv. 
The case studies in `epi-1` and `epi-2` are using the same profile and `birthdays` is using a separate profile. 
To activate the profile for the two case studies using the epilepsy dataset run `renv::activate(profile = "epilepsy")`and check that R is restarting and that you see a message similar to 
```
- Project '.../multi-workflows' loaded. [renv 1.0.5; using profile 'epilepsy']
```
If more packages need to be installed, you will see a message about this and can check `renv::status()` which will provide the instructions for installing the missing packages. 
To activate the profile for the birthdays case study, run `renv::activate(profile = "birthdays")` instead and follow the same steps as above. 
