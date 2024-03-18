# Set up

* Install julia 1.10 (see e.g. [https://github.com/JuliaLang/juliaup](https://github.com/JuliaLang/juliaup)) and instantiate project via [`julia --project=. -e "using Pkg; Pkg.instantiate()"`](https://pkgdocs.julialang.org/v1/environments/#Using-someone-else's-project).
* Install [`poetry`](https://python-poetry.org/) and install dependencies via [`poetry install`](https://python-poetry.org/docs/basic-usage/#installing-dependencies).
* Initialize [`renv`](https://rstudio.github.io/renv/index.html) via [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)

# Running

You may need to change the flags for snakemake below. 
`-c4` instructs snakemake to use 4 cores, see [https://snakemake.readthedocs.io/en/stable/executing/cli.html](https://snakemake.readthedocs.io/en/stable/executing/cli.html) for more info.

Run 

* `poetry run snakemake quick -c4` for a quick "multiverse" with just two different models (may take a few minutes) or 
* `poetry run snakemake full -c4` for the full multiverse (may take a few hours).

# Evaluation

Running either of the above snakemake commands generates the files `output/quick_multiverse.csv` or `output/full_multiverse.csv` respectively
and various files containing posterior draws in the subfolder `output/samples`. 

# Plotting

The snakefile exposes the targets `output/plots/{which}_treatment.{ext}`, where 

* `which` can be either of `quick`, `medium` or `full`, and
* `ext` can be either `png` (for quick inspection) or `tex` (for the manuscript).

# Additional targets

* `output/plots/funnel.{ext}`: Visualization of the funnel and divergences for a simple model.
* `output/plots/{which}_elpd.{ext}`: Quick elpd comparison of models within a chosen multiverse.