# GP data generating process
# inspired from https://www.r-bloggers.com/2019/07/sampling-paths-from-a-gaussian-process/

# load libraries 
library(MASS)

# setup 
n_obs <- 100 #as.numeric(snakemake@wildcards$n_obs)
n_relevant <- 11
n_irrelevant <- 4
n_total <- n_relevant + n_irrelevant

x <- runif(n = n_obs, min = -1, max = 1) # x-coordinates
#x <- seq(-1, 1, length.out = n_obs)  
y <- matrix(0, nrow = n_obs, ncol = n_total)


# generate covariance matrix for points in `x` using given kernel function
cov_matrix <- function(x, kernel_fn, ...) {
  outer(x, x, function(a, b) kernel_fn(a, b, ...))
}

# given x coordinates, take N draws from kernel function at those points
draw_gp_samples <- function(x, N, seed = 1, kernel_fn, ...) {
  Y <- matrix(NA, nrow = length(x), ncol = N)
  set.seed(seed)
  for (n in 1:N) {
    K <- cov_matrix(x, kernel_fn, ...)
    Y[, n] <- mvrnorm(1, mu = rep(0, times = length(x)), Sigma = K)
  }
  Y
}

se_kernel <- function(x, y, sigma = 1, lengthscale = 1) {
  sigma^2 * exp(- (x - y)^2 / (2 * lengthscale^2))
}

Y <- draw_samples(x, N, kernel_fn = se_kernel, lengthscale = 0.2)

col_list <- c("red", "blue", "black")  # for line colors
plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
     main = "SE kernel, length = 0.2")

for (n in 1:N) {
  lines(x, Y[, n], col = col_list[n], lwd = 1.5)
}
