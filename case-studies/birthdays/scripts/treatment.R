source("scripts/utils.R")

# define plot saving method
save_tikz_plot <- function(
        plot, filename, width = NA, height = NA, asp = NA
) {
    # automatic scaling
    if (is.na(asp)) asp <- 1.618
    if (is.na(width) && is.na(height)) {
        height <- 3.71
        width <- height * asp
    }
    else if (is.na(width)) {
        width <- height * asp
    }
    else if (is.na(height)) {
        height <- width / asp
    }
    
    # make tex
    tikz(file = filename, width = width, height = height)
    print(plot)
    dev.off()
    
    # patch cropping issues
    lines <- readLines(con = filename)
    lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
    lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
    writeLines(lines, con = filename)
}

download.file("https://users.aalto.fi/~sicchan1/multiverse/multiverse.csv", destfile="multiverse.csv")
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_2_2_noncentered_qoi.json", destfile="32_16_32_1_1_2_2_noncentered_qoi.json")
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_3_2_noncentered_qoi.json", destfile="32_16_32_1_1_3_2_noncentered_qoi.json")
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_2_2_posthoc_qoi.json", destfile="32_16_32_1_1_2_2_posthoc_qoi.json")
download.file("https://users.aalto.fi/~sicchan1/multiverse/32_16_32_1_1_3_2_posthoc_qoi.json", destfile="32_16_32_1_1_3_2_posthoc_qoi.json")

# theme_set(theme_bw() +
#               theme(panel.grid.major = element_blank(),
#                     panel.grid.minor = element_blank(),
#                     strip.background = element_blank(),
#                     panel.background = element_blank(),
#                     text = element_text(size=7),
#                     plot.title = element_text(size=7),
#                     axis.title = element_text(size=7),
#                     axis.text = element_text(size=7)))

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

multiverse <- read.csv(file="multiverse.csv")
treatment = rbind(
    data.frame(
        model=rep("Model 142", 4000), 
        family=rep("student's t", 4000), 
        parametrization=rep("manual", 4000), 
        treatment=fromJSON(file="32_16_32_1_1_2_2_noncentered_qoi.json")$treatment
    ),
    data.frame(
        model=rep("Model 144", 4000), 
        family=rep("RHS", 4000),
        parametrization=rep("manual", 4000), 
        treatment=fromJSON(file="32_16_32_1_1_3_2_noncentered_qoi.json")$treatment
    ),
    data.frame(
        model=rep("Model 142", 4000), 
        family=rep("student's t", 4000),
        parametrization=rep("adapted", 4000), 
        treatment=fromJSON(file="32_16_32_1_1_2_2_posthoc_qoi.json")$treatment
    ),
    data.frame(
        model=rep("Model 144", 4000),  
        family=rep("RHS", 4000),
        parametrization=rep("adapted", 4000), 
        treatment=fromJSON(file="32_16_32_1_1_3_2_posthoc_qoi.json")$treatment
    )
)

p1 = ggplot(filter(multiverse, parametrization=="adapted"), aes(x=treatment_mean, y=reorder(label, treatment_q50), shape=family)) + 
    geom_pointrange(aes(xmin=treatment_q25, xmax=treatment_q75), fatten = .1, size = pointsize, linewidth=1) +
    geom_pointrange(aes(xmin=treatment_q2.5, xmax=treatment_q97.5), fatten = .1, size = pointsize) +
    # scale_shape_manual(values=c("normal" = 21, "student's t" = 25, "RHS" = 23)) +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position = "none")
p1


p2 = ggplot(filter(treatment, parametrization=="adapted"), aes(x=treatment, y=model, shape=family)) + 
    # stat_pointinterval(.width = c(.5, .95)) +
    stat_halfeye(.width=c(0.5, 0.95)) +
    ggtitle("Adapted reparameterisation") +
    # facet_grid(rows=vars(parametrization)) +
    # scale_shape_manual(values=c("normal" = 21, "student's t" = 25, "RHS" = 23)) +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") +
    xlim(c(-.3, +.1)) +
    # scale_color_manual(values=c("manual" = "red", "adapted" = "black")) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_text(size=7), 
          axis.title.y = element_blank(),
          legend.position = "none")

p3 = ggplot(filter(treatment, parametrization=="manual"), aes(x=treatment, y=model, shape=family)) + 
    # stat_pointinterval(.width = c(.5, .95)) +
    stat_halfeye(.width=c(0.5, 0.95)) +
    # facet_grid(rows=vars(parametrization)) +
    ggtitle("Manual reparameterisation") +
    # scale_shape_manual(values=c("normal" = 21, "student's t" = 25, "RHS" = 23)) +
    scale_shape_manual(values=c("normal" = 1, "student's t" = 2, "RHS" = 5)) +
    xlab("Halloween effect") +
    xlim(c(-.3, +.1)) +
    # scale_color_manual(values=c("manual" = "red", "adapted" = "black")) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme(axis.text.y = element_text(size=7), 
          axis.title.y = element_blank(),
          legend.position = "none")

p1 | p2 / p3

save_tikz_plot(p1|p2/p3, "birthday-treatment.tex")
