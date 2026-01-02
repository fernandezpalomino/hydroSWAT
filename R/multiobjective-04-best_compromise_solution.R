# -------------------------------------------------------------------------
# best_compromise_solution
# -------------------------------------------------------------------------
#' @title Select a best-compromise Pareto solution by Euclidean distance
#'
#' @description
#' Selects the solution on a Pareto front with the smallest Euclidean
#' distance to the ideal point.
#'
#' @param pareto_solutions matrix/data.frame/tibble. Each row is a solution
#'   (objective vector) and each column is an objective; values must be
#'   numeric (no NAs) and oriented for minimization.
#' @param normalize logical. Whether to min–max normalize objectives
#'   column-wise before distance calculation (default \code{TRUE}).
#'
#' @details
#' The ideal point is the vector of column minima in the (normalized)
#' objective space. Euclidean distances are computed from each solution to
#' this ideal point; the closest solution is returned. If several solutions
#' have equal minimum distance, the first occurrence (by row order) is
#' returned.
#'
#' @return
#' A list with:
#' \itemize{
#'   \item \code{objectives}: numeric vector with the objective values of
#'     the selected solution (original, unnormalized scale).
#'   \item \code{index}: integer row index of the selected solution in
#'     \code{pareto_solutions}.
#' }
#'
#' @family Multiobjective calibration
#'
#' @examples
#' pareto_solutions <- matrix(
#'   c(0.1554, 0.0282, 0.30,
#'     0.1309, 0.0608, 0.28,
#'     0.1418, 0.0460, 0.25),
#'   ncol = 3, byrow = TRUE
#' )
#' best     <- best_compromise_solution(pareto_solutions)
#' best_raw <- best_compromise_solution(pareto_solutions, normalize = FALSE)
#'
#' @export
best_compromise_solution <- function(pareto_solutions, normalize = TRUE) {

  # Accept only matrix/data.frame/tibble
  if (!is.matrix(pareto_solutions) && !is.data.frame(pareto_solutions)) {
    stop("'pareto_solutions' must be a matrix, data frame, or tibble.")
  }

  # Single validation after coercion
  ps <- as.matrix(pareto_solutions)
  if (!is.numeric(ps)) {
    stop("'pareto_solutions' must be fully numeric.")
  }
  if (anyNA(ps)) {
    stop("'pareto_solutions' must not contain missing values.")
  }

  # Optional normalization with zero-variance safeguard
  if (normalize) {
    normalized <- apply(ps, 2, function(x) {
      rng <- max(x) - min(x)
      if (rng == 0) rep(0, length(x)) else (x - min(x)) / rng
    })
  } else {
    normalized <- ps
  }

  # Ideal point and Euclidean distances
  ideal <- apply(normalized, 2, min)
  d <- apply(normalized, 1, function(sol) sqrt(sum((sol - ideal)^2)))

  # Select best (minimum distance)
  idx <- which.min(d)
  list(objectives = as.numeric(ps[idx, ]), index = idx)
}

