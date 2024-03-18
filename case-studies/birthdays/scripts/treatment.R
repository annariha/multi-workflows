source("scripts/utils.R")

theme_set(theme_classic() +
              theme(legend.position = "none", 
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.background = element_blank(),
                    text = element_text(size=7),
                    plot.title = element_text(size=7),
                    axis.title = element_text(size=7),
                    axis.text = element_text(size=7)))

pointsize = 4
ytextsize = 4

multiverse <- read.csv(file=snakemake@input[[1]])

path_to_df <- function(path){
    json = fromJSON(file=path)
    split_cfg = unlist(strsplit(as.character(json["cfg"]), split="_"))
    cfg1 = paste(split_cfg[1:7], sep="_", collapse="_")
    cfg2 = split_cfg[8]
    if(cfg2 == "posthoc"){cfg2 = "adapted"}
    print(c(cfg1, cfg2))
    row = multiverse |> filter(cfg == cfg1) |> filter(parametrization == cfg2)
    print(row$label)
    data.frame(
        model=rep(row$label, 4000), 
        family=rep(row$family, 4000), 
        parametrization=rep(row$parametrization, 4000), 
        treatment=json$treatment
    )
}

treatment = rbind(
    path_to_df(snakemake@input[[2]]),
    path_to_df(snakemake@input[[3]]),
    path_to_df(snakemake@input[[4]]),
    path_to_df(snakemake@input[[5]])
)

p1 = ggplot(filter(multiverse, parametrization=="adapted"), aes(x=treatment_mean, y=reorder(label, treatment_q50), shape=family)) + 
    geom_pointrange(aes(xmin=treatment_q25, xmax=treatment_q75), fatten = .1, size = pointsize, linewidth=1) +
    geom_pointrange(aes(xmin=treatment_q2.5, xmax=treatment_q97.5), fatten = .1, size = pointsize) +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p1


p2 = ggplot(filter(treatment, parametrization=="adapted"), aes(x=treatment, y=model, shape=family)) + 
    stat_halfeye(.width=c(0.5, 0.95)) +
    ggtitle("Adapted reparameterisation") +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") +
    xlim(c(-.3, +.1)) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_text(size=7), 
          axis.title.y = element_blank(),
          legend.position = "none")

p3 = ggplot(filter(treatment, parametrization=="noncentered"), aes(x=treatment, y=model, shape=family)) + 
    stat_halfeye(.width=c(0.5, 0.95)) +
    ggtitle("Manual reparameterisation") +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") +
    xlim(c(-.3, +.1)) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_text(size=7), 
          axis.title.y = element_blank(),
          legend.position = "none")

save_plot(p1|p2/p3, snakemake@output[[1]])
