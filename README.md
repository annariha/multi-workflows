# Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis

This repository contains the code for the case studies in "Supporting Bayesian modelling workflows with iterative filtering for multiverse analysis" by Anna ELisabeth Riha, Nikolas Siccha, Antti Oulasvirta and Aki Vehtari.
The paper is available as a preprint on arXiv. 

There are three case studies in **case-studies**:

- **epilepsy/epi-1**: iterative filtering for a set of models for analysing treatment effects of anti-convulsant therapy with data $\texttt{brms::epilepsy}$.
The data contains $236$ observations. 
- **epilepsy/epi-2**: extending the filtered set of models from **epi-1** by including varying effects on the level of each visit, patient and observation and applying iterative filtering to the extended set of models 
- **birthdays**: iterative filtering for a set of models of varying complexity for the registered number of daily births in the USA using natality data from the National Vital Statistics System provided by Google BigQuery and exported by Chris Mulligan and Robert Kern. 
The data contains $7305$ observations of the number of births with the corresponding day, month, and year as well as day of the year and day of the week in the USA from $01.01.1969$ to $31.12.1988$.
- ...

Each case study in **case-studies** has (some of) the following subfolders: 

- **data**: contains the data, if applicable 
- **R**: R-code 
- **results**: .rds or .csv-files, if applicable  
- **figures**: visualisations as .tex
