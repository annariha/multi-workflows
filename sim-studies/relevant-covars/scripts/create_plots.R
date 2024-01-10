library(tidyverse)
library(ggplot2)
library(latex2exp)
library(ggExtra)
library(patchwork)
library(cowplot)

if (exists("snakemake")){
  plot_df <- readRDS(snakemake@input[[1]])
  plot_png_file <- snakemake@output[[1]]
} else {
  # for testing 
  idx <- 1
  plot_df <- readRDS(here::here("sim-studies", "relevant-covars", "outputs", "tmp", paste0("comparison_plot_df_", idx, ".rds")))
  plot_png_file <- here::here("sim-studies", "relevant-covars", "outputs", "tmp", "plots", paste0("plot_elpddiff_loobb_pbma_", idx, ".png"))
  }

# helper function 
source(here::here("sim-studies", "relevant-covars", "scripts", "build_plot_elpddiff_loobb_pbma.R")) 
#source(here::here("sim-studies", "relevant-covars", "scripts", "save_tikz_plot.R"))

# create plot
plot <- get_plot_elpddiffs_loobb(plot_df)

# save 
cowplot::save_plot(plot_png_file, plot, bg = "white")
