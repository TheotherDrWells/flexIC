#' @name flexIC
#' @title Run Iman–Conover Rank-Preserving Transform
#' @param x A numeric matrix or data.frame to transform.
#' @param target_r Target rank correlation matrix (optional).
#' @param eps Convergence tolerance (default = 0.04).
#' @param max_iter Maximum number of iterations (default = 50).
#' @importFrom stats cor qnorm
#' @export
flexIC <- function(x, target_r = NULL, eps = .04, max_iter = 50) {
  if (is.null(target_r)) target_r <- cor(x, method = 'spearman')
  best <- NULL
  best_err <- Inf
  best_iter <- NA
  for (i in seq_len(max_iter)) {
    cand <- ic_exact(x, target_r)
    err <- attr(cand, 'err')
    if (err < best_err) {
      best <- cand
      best_err <- err
      best_iter <- i
    }
    if (best_err <= eps) break
  }
  list(data = best, max_abs_diff = best_err, iter = best_iter)
}
