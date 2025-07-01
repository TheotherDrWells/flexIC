#' @title One-Shot Iman–Conover Transformation
#' @name ic_exact
#' @description
#' Applies the classic Iman–Conover procedure to reorder the columns of a numeric matrix
#' to approximately match a target rank correlation structure, while preserving marginals.
#'
#' @param x A numeric matrix or data frame with independent columns (desired marginals).
#' @param target_r A square, positive-definite correlation matrix to impose.
#' @return A numeric matrix with the same marginal distributions as \code{x} and
#'   approximately matching the target Spearman correlation.
#' @importFrom MASS mvrnorm
#' @importFrom stats cor qnorm
#' @examples
#' set.seed(123)
#' x <- matrix(rnorm(300), ncol = 3)
#' R_target <- matrix(c(1, 0.5, 0.3,
#'                      0.5, 1, 0.4,
#'                      0.3, 0.4, 1), 3)
#' out <- ic_exact(x, R_target)
#' cor(out, method = "spearman")
#' @export
ic_exact <- function(x, target_r) {
  x <- as.matrix(x)
  n <- nrow(x)
  k <- ncol(x)
  
  # Step 1: Generate Z ~ MVN(0, target_r)
  Z <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = target_r)
  
  # Step 2: Rank Z to get rank scaffold
  ranks <- apply(Z, 2, rank, ties.method = "first")
  
  # Step 3: Permute x's columns according to ranks
  out <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    out[order(ranks[, j]), j] <- sort(x[, j])
  }
  
  # Attach max rank-correlation error (optional)
  spearman_err <- max(abs(cor(out, method = "spearman")[upper.tri(target_r)] -
                            target_r[upper.tri(target_r)]))
  attr(out, "err") <- spearman_err
  
  return(out)
}
