#' flexIC: Tunable Iman–Conover Rank-Correlation Imposition
#'
#' Applies a rank-based correlation structure to a numeric matrix using
#' a flexible, iterative variant of the Iman–Conover algorithm.
#' The method reorders each column of \code{x} based on the rank structure
#' of a multivariate normal draw whose correlation matrix matches \code{target_r}.
#' If \code{eps} is specified, the algorithm will iteratively draw candidates and
#' select the one with the closest match to the target Spearman structure.
#' The marginal distributions of \code{x} are preserved exactly.
#'
#' @param x Numeric matrix or data frame. Columns should be independent prior to transformation.
#' @param target_r Target Spearman correlation matrix to impose. Must be square, symmetric, and positive-definite.
#' @param eps Convergence tolerance (maximum absolute deviation allowed between achieved and target Spearman correlation).
#'   If \code{eps = "none"}, no convergence test is performed and the first draw is used (equivalent to classic Iman–Conover).
#' @param max_iter Maximum number of candidate draws to evaluate when \code{eps} is numeric.
#'
#' @return A numeric matrix with same dimensions as \code{x}, with transformed columns preserving marginal distributions
#'   and approximately matching the specified rank correlation structure.
#' @importFrom MASS mvrnorm
#' @importFrom stats cor qnorm
#' @examples
#' set.seed(1)
#' x <- cbind(rexp(100), rbinom(100, 5, 0.4))
#' R_target <- matrix(c(1, 0.6, 0.6, 1), 2)
#' out <- flexIC(x, R_target, eps = 0.02, max_iter = 50)
#' cor(out, method = "spearman")
#'
#' @export
flexIC <- function(x, target_r, eps = "none", max_iter = 20) {
  if (missing(target_r)) stop("You must supply a target correlation matrix.")
  n <- nrow(x)
  k <- ncol(x)
  
  if (eps == "none") {
    Z <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = target_r)
    ranks <- apply(Z, 2, rank, ties.method = "random")
    out <- matrix(NA_real_, n, k)
    for (j in seq_len(k)) {
      out[order(ranks[, j]), j] <- sort(x[, j])
    }
    return(out)
  }
  
  best <- NULL
  best_err <- Inf
  for (i in seq_len(max_iter)) {
    Z <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = target_r)
    S_hat <- cor(Z, method = "spearman")
    err <- max(abs(S_hat[upper.tri(S_hat)] - target_r[upper.tri(target_r)]))
    if (err < best_err) {
      best_err <- err
      best <- Z
      if (best_err <= eps) break
    }
  }
  
  ranks <- apply(best, 2, rank, ties.method = "random")
  out <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    out[order(ranks[, j]), j] <- sort(x[, j])
  }
  out
}
