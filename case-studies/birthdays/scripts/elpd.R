source("scripts/utils.R")

df = read.csv(snakemake@input[[1]])
sdf = filter(df, df$loo_bb_weight_mean > 1e-2)

p1 = ggplot(df, aes(elpd_diff, -ranking, color=n_divergent == 0)) + xlab("ELPD difference") + geom_point() + facet_grid(rows=vars(parametrization))
p2 = ggplot(sdf, aes(elpd_diff, label, color=n_divergent == 0)) + xlab("ELPD difference") +geom_point() + facet_grid(rows=vars(parametrization))
p3 = ggplot(sdf, aes(loo_bb_weight_mean, label, color=n_divergent == 0)) + xlab("LOO-BB weight") +geom_point() + facet_grid(rows=vars(parametrization))

save_plot(p1 | p2 | p3, snakemake@output[[1]])