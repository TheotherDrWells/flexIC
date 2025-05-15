#' Run the flexible Iman–Conover (flexIC) transform
#'
#' Applies the “best-of-m” Iman–Conover algorithm to coerce a data matrix
#' to a target Spearman rank-correlation structure while preserving each
#' column’s marginal distribution.
#'
#' @param x Numeric matrix or data frame to transform.
#' @param target_r Optional target rank-correlation matrix; defaults to
#'   \code{cor(x, method = "spearman")}.
#' @param eps Convergence tolerance for the maximum absolute r-difference.
#' @param max_iter Maximum number of candidate draws to evaluate.
#'
#' @return A list with three components
#'   \describe{
#'     \item{data}{Numeric matrix with transformed columns.}
#'     \item{max_abs_diff}{Largest absolute difference between achieved
#'       and target correlations.}
#'     \item{iter}{Index of the candidate draw that achieved
#'       \code{max_abs_diff}.}
#'   }
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(300), ncol = 3)
#' target <- matrix(c(1, .6, .3,
#'                    .6, 1, .4,
#'                    .3, .4, 1), 3)
#' out <- flexIC(x, target, eps = 0.02, max_iter = 10)
#' str(out)
#'
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
