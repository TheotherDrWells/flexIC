## ----setup, include=FALSE-----------------------------------------------------
set.seed(1)
x <- data.frame(A = runif(100), B = rexp(100))
res <- flexIC(x)

cor(x, method = "spearman")
cor(res$data, method = "spearman")

plot_marginals_grid(x, res)

