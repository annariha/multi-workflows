source("scripts/utils.R")
bias <- fromJSON(file=snakemake@input[[1]])

df1 = data.frame(
    group=rep("manual", 1000),
    log_scale=bias$draws$noncentered[[1]],
    treatment=bias$draws$noncentered[[2]],
    divergent=bias$divergent$noncentered
)

df2 = data.frame(
    group=rep("adapted", 1000),
    log_scale=bias$draws$posthoc[[1]],
    treatment=bias$draws$posthoc[[2]],
    divergent=bias$divergent$posthoc
)

df = rbind(df1, df2)

p = ggplot(df, aes(treatment, log_scale, color=divergent)) + geom_point() + facet_grid(cols=vars(group)) + ylab("log scale")

save_plot(p, snakemake@output[[1]])
