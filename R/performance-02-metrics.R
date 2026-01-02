# -------------------------------------------------------------------------
# gof_metrics
# -------------------------------------------------------------------------
#' @title Goodness-of-fit (GOF) metrics and FDC-based signatures
#'
#' @description
#' Computes a set of goodness-of-fit (GOF) metrics and flow duration curve
#' (FDC) signatures to evaluate model performance by comparing simulated
#' vs. observed values.
#'
#' @param obs numeric. Observed values.
#' @param sim numeric. Simulated values.
#' @param metric character. Name of the metric to compute. See Details for
#'   the list of available metrics.
#' @param minimize logical. Whether to transform the metric so that it can
#'   be used as a minimization objective in optimization-based model
#'   calibration workflows. Default: \code{FALSE}.
#' @param all_metrics logical. Whether to compute all available metrics.
#'   If \code{TRUE}, a tibble with all metrics is returned. Default: \code{FALSE}.
#' @param digits integer. Number of decimal places for rounding. Default: 4.
#'
#' @details
#' The following metrics are available. Parentheses indicate value domains
#' (e.g., -Inf to Inf), and brackets indicate exceedance probability
#' segments of FDC (e.g., \code{[-EP: 0–2\%]}):
#' \itemize{
#'   \item \code{NSE}: Nash-Sutcliffe efficiency (-Inf to 1).
#'   \item \code{log_NSE}: Log-transformed NSE (-Inf to 1).
#'   \item \code{mNSE}: Modified NSE (-Inf to 1).
#'   \item \code{rNSE}: Relative NSE (-Inf to 1).
#'   \item \code{KGE}: Kling-Gupta efficiency (-Inf to 1).
#'   \item \code{KGElf}: KGE for low values (-Inf to 1).
#'   \item \code{KGEnp}: Non-parametric KGE (-Inf to 1).
#'   \item \code{KGEkm}: Knowable Moments KGE (-Inf to 1).
#'   \item \code{RMSE}: Root mean squared error (>= 0).
#'   \item \code{TRMSE}: Box-Cox transformed RMSE (>= 0).
#'   \item \code{MAE}: Mean absolute error (>= 0).
#'   \item \code{PBIAS}: Percent bias (-Inf to Inf).
#'   \item \code{VE}: Volumetric efficiency (-Inf to 1).
#'   \item \code{RSR}: Ratio of RMSE to SD of observations (0 to +Inf).
#'   \item \code{rPearson}: Pearson correlation (-1 to 1).
#'   \item \code{rSpearman}: Spearman correlation (-1 to 1).
#'   \item \code{R2}: Coefficient of determination (0 to 1).
#'   \item \code{FDCsign}: Aggregated FDC signature (>= 0).
#'   \item \code{FDC_Speak}: Percent bias in peak segment volume
#'     [-EP: 0–2\%] (-Inf to Inf).
#'   \item \code{FDC_Shigh}: Percent bias in high segment volume
#'     [-EP: 2–20\%] (-Inf to Inf).
#'   \item \code{FDC_Smid}: Percent bias in midsegment slope
#'     [-EP: 20–70\%] (-Inf to Inf).
#'   \item \code{FDC_Slow}: Percent bias in low segment volume
#'     [-EP: 70–100\%] (-Inf to Inf).
#' }
#'
#' Metrics \code{TRMSE} and all \code{FDC*} signatures are computed using
#' functions from this package (\link{trmse} and \link{fdc_signature}),
#' while the remaining metrics are computed with the \pkg{hydroGOF} package.
#'
#' @return
#' If \code{all_metrics = FALSE}, a numeric value for the requested metric.
#' If \code{all_metrics = TRUE}, a tibble with two columns:
#' \itemize{
#'   \item \code{gof}: Metric name.
#'   \item \code{value}: Metric value (rounded).
#' }
#'
#' @family Performance evaluation
#'
#' @examples
#' # Synthetic daily flow data
#' set.seed(123)
#' obs <- abs(rnorm(730, mean = 50, sd = 20))
#' sim <- obs * runif(730, min = 0.8, max = 1.5)
#'
#' # Compute a single metric (NSE)
#' gof_metrics(obs, sim, "NSE")
#'
#' # Compute all metrics
#' gof_metrics(obs, sim, all_metrics = TRUE)
#'
#' @export


gof_metrics <- function(obs, sim, metric, minimize = FALSE, all_metrics = FALSE,
                        digits = 4
) {
  # Validate inputs
  if (!is.numeric(obs) || !is.numeric(sim)) {
    stop("Both 'obs' and 'sim' must be numeric vectors.")
  }
  if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must have the same length.")
  }

  # Remove NA values
  valid_indices <- !is.na(obs) & !is.na(sim)
  obs <- obs[valid_indices]
  sim <- sim[valid_indices]

  # Define a list of metric functions
  metric_functions <- list(
    NSE = function() hydroGOF::NSE(sim, obs, na.rm = TRUE),
    log_NSE = function() hydroGOF::NSE(
      sim, obs, fun=log, epsilon.type="otherValue", epsilon.value=0.01
    ),
    mNSE = function() hydroGOF::mNSE(sim, obs, na.rm = TRUE),
    rNSE = function() hydroGOF::rNSE(sim, obs, na.rm = TRUE),
    KGE = function() hydroGOF::KGE(sim, obs, method = "2012"),
    KGElf = function() hydroGOF::KGElf(sim, obs, method = "2012"),
    KGEnp = function() hydroGOF::KGEnp(sim, obs),
    KGEkm = function() hydroGOF::KGEkm(sim, obs, method = "2012"),
    RMSE = function() hydroGOF::rmse(sim, obs),
    TRMSE = function() trmse(sim, obs),
    MAE = function() hydroGOF::mae(sim, obs),
    PBIAS = function() hydroGOF::pbias(sim, obs),
    VE = function() hydroGOF::VE(sim, obs),
    RSR = function() hydroGOF::rsr(sim, obs),
    rPearson = function() hydroGOF::rPearson(sim, obs),
    rSpearman = function() hydroGOF::rSpearman(sim, obs),
    R2 = function() hydroGOF::R2(sim, obs),
    FDCsign = function() fdc_signature(sim, obs)$FDCsign,
    FDC_Speak = function() fdc_signature(sim, obs)$Speak,
    FDC_Shigh = function() fdc_signature(sim, obs)$Shigh,
    FDC_Smid = function() fdc_signature(sim, obs)$Smid,
    FDC_Slow = function() fdc_signature(sim, obs)$Slow
  )

  # If all_metrics is TRUE, compute all metrics
  if (all_metrics) {
    metric_list <- lapply(names(metric_functions), function(m) {
      tryCatch(
        {
          value <- round(metric_functions[[m]](), digits)
          tibble(gof = m, value = value)
        },
        error = function(e) {
          # message(paste(
          #   "Issue calculating", m, ":", e$message, "\n",
          #   "Returning NA for this metric."
          # ))

          warning(paste(
            "Issue calculating", m, ":",
            "Data is too short. Returning NA for this metric."
          ), call. = FALSE)

          tibble(gof = m, value = NA)
        }
      )
    })
    return(dplyr::bind_rows(metric_list))
  }


  # Match the metric argument to the available functions
  metric <- match.arg(metric, choices = names(metric_functions))

  # Compute the metric
  metric_value <- metric_functions[[metric]]()

  # Adjust for minimization if required
  if (minimize) {
    metric_value <- switch(
      metric,
      NSE = 1 - metric_value,
      log_NSE = 1 - metric_value,
      rNSE = 1 - metric_value,
      mNSE = 1 - metric_value,
      KGE = 1 - metric_value,
      KGElf = 1 - metric_value,
      KGEnp = 1 - metric_value,
      KGEkm = 1 - metric_value,
      RMSE = metric_value,
      TRMSE = metric_value,
      MAE = metric_value,
      PBIAS = abs(metric_value)/100,
      VE = metric_value,
      RSR = metric_value,
      rPearson = abs(metric_value),
      rSpearman = abs(metric_value),
      R2 = metric_value,
      FDCsign = metric_value,
      FDC_Speak = abs(metric_value)/100,
      FDC_Shigh = abs(metric_value)/100,
      FDC_Smid = abs(metric_value)/100,
      FDC_Slow = abs(metric_value)/100,
      stop("Unsupported metric: ", metric)
    )
  }

  # Return the calculated metric (either original or minimized)
  return(round(metric_value, digits))
}



# -------------------------------------------------------------------------
# trmse
# -------------------------------------------------------------------------
#' @title Transformed root mean squared error (TRMSE)
#'
#' @description
#' Computes the TRMSE between simulated and observed values using a Box-Cox
#' transformation, reducing the influence of extreme values.
#'
#' @param sim numeric. Simulated values.
#' @param obs numeric. Observed values.
#' @param lambda numeric. Box-Cox transformation parameter. Default: 0.3.
#'
#' @details
#' TRMSE applies a Box-Cox transformation to both observed and simulated
#' values before computing RMSE:
#' \deqn{
#'   TRMSE =
#'   \sqrt{\frac{1}{n} \sum_{i=1}^{n} \bigg(
#'     \frac{(S_i + 1)^{\lambda} - 1}{\lambda} -
#'     \frac{(O_i + 1)^{\lambda} - 1}{\lambda}
#'   \bigg)^2}
#' }
#' where \eqn{S_i} and \eqn{O_i} are simulated and observed values,
#' respectively. NA values are removed before computation.
#'
#' @return numeric. The TRMSE value.
#'
#' @references
#' van Werkhoven, K., Wagener, T., Reed, P., & Tang, Y. (2009).
#' Sensitivity-guided reduction of parametric dimensionality for multi-objective
#' calibration of watershed models.
#'
#' @examples
#' # Synthetic daily flow data
#' set.seed(123)
#' obs <- abs(rnorm(730, mean = 50, sd = 20))
#' sim <- obs * runif(730, min = 0.8, max = 1.5)
#'
#' # Compute TRMSE
#' trmse(sim, obs)
#'
#' @export

trmse <- function(sim, obs, lambda = 0.3) {
  # Check input validity
  if (!is.numeric(sim) || !is.numeric(obs)) {
    stop("Both 'sim' and 'obs' must be numeric!")
  }
  if (length(sim) != length(obs)) {
    stop("'sim' and 'obs' must have the same length!")
  }
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0) {
    stop("'lambda' must be a single positive numeric value.")
  }

  # Remove NA values
  valid_indices <- !is.na(sim) & !is.na(obs)
  sim <- sim[valid_indices]
  obs <- obs[valid_indices]

  # Apply Box-Cox transformation
  boxcox_sim <- ((sim + 1)^lambda - 1) / lambda
  boxcox_obs <- ((obs + 1)^lambda - 1) / lambda

  # Compute TRMSE
  trmse_value <- sqrt(mean((boxcox_sim - boxcox_obs)^2))

  # Return TRMSE value
  return(trmse_value)
}


# -------------------------------------------------------------------------
# get_gof_metrics
# -------------------------------------------------------------------------
get_gof_metrics <- function(){
  gof_metrics <- c(
    "NSE", "log_NSE", "mNSE", "rNSE", "KGE", "KGElf", "KGEnp", "KGEkm",
    "RMSE", "TRMSE", "MAE", "PBIAS", "VE", "RSR", "rPearson", "rSpearman",
    "R2", "FDCsign", "FDC_Speak", "FDC_Shigh", "FDC_Smid", "FDC_Slow"
  )

  return(gof_metrics)
}
