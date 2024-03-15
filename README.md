# Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis

This repository contains the code for the case studies in "Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis" by Anna Elisabeth Riha, Nikolas Siccha, Antti Oulasvirta and Aki Vehtari.
The paper is available as a [preprint on arXiv](). 

We illustrate iterative filtering approaches in the following `case-studies`:

- `epilepsy/epi-1`: iterative filtering of models for analysing seizure counts and treatment effects of anti-convulsant therapy with data $\texttt{brms::epilepsy}$ available in the [brms](https://paul-buerkner.github.io/brms/) package by Paul-Christian Bürkner, previously analysed, for example, by [Thall and Vail (1990)](https://pubmed.ncbi.nlm.nih.gov/2242408/) and [Breslow and Clayton (1993)](https://www.jstor.org/stable/2290687).
The data contains $236$ observations of a randomised controlled trial of patients with epilepsy. 
- `epilepsy/epi-2`: extending the filtered set of models from `epi-1` by including varying effects on the level of each visit, patient and observation and applying iterative filtering
- `birthdays`: iterative filtering of models for the registered number of daily births in the USA using natality data from the National Vital Statistics System provided by Google BigQuery and exported by Chris Mulligan and Robert Kern. 
The data contains $7305$ observations of the number of births with the corresponding day, month, and year as well as day of the year and day of the week in the USA from $01.01.1969$ to $31.12.1988$.
- ...

Each case study has subfolders, for example: 

- `data`: contains the data, if applicable 
- `R`: $\texttt{R}$ scripts specific to the case study 

Additionally, scripts and helper functions used for both `epi-1` and `epi-2` are located in `case-studies/epilepsy/R`. 

This repository is using the [renv](https://rstudio.github.io/renv/articles/renv.html) package by Kevin Ushey and Hadley Wickham for creating a reproducible environment. 
After cloning the repository, the required version of renv is automatically installed when opening the project and necessary packages can be installed with $\texttt{renv::restore()}$.  
