# -------------------------------------------------------------------------
# fdc_signature
# -------------------------------------------------------------------------
#' @title Flow Duration Curve (FDC) signature
#'
#' @description
#' Computes \code{FDCsign}, an aggregated index combining four percent-bias
#' metrics for distinct flow regimes (peak, high, intermediate, and low),
#' summarizing model performance across the flow regime spectrum.
#' See Details for its calculation and component definitions.
#'
#' @param sim numeric. Simulated flow values.
#' @param obs numeric. Observed flow values.
#'
#' @details
#' The index \code{FDCsign} is computed as:
#' \deqn{
#'   FDCsign =
#'   0.25 \times \big( |Speak/100| + |Shigh/100| + |Smid/100| + |Slow/100| \big)
#' }
#' where each component is defined as:
#' \itemize{
#'   \item \code{Speak}: Percent bias in the FDC peak segment volume
#'     (EP: 0–2\%), computed with \link{pbias_fdc_target_segment}.
#'   \item \code{Shigh}: Percent bias in the FDC high segment volume
#'     (EP: 2–20\%), computed with \link{pbias_fdc_target_segment}.
#'   \item \code{Smid}: Percent bias in the FDC midsegment slope
#'     (EP: 20–70\%), computed with \link{pbias_fdc_midsegment}.
#'   \item \code{Slow}: Percent bias in the FDC low segment volume
#'     (EP: 70–100\%), computed with \link{pbias_fdc_target_segment}.
#' }
#'
#' @return
#' A tibble with five columns: \code{Speak}, \code{Shigh}, \code{Smid},
#' \code{Slow} (all expressed as percent bias), and \code{FDCsign}
#' (aggregated index, unitless).
#'
#' @family Performance evaluation
#'
#' @examples
#' # Synthetic daily flow data
#' set.seed(123)
#' obs <- abs(rnorm(730, mean = 50, sd = 20))
#' sim <- obs * runif(730, min = 0.8, max = 1.5)
#'
#' # Compute FDC signature
#' fdc_signature(sim, obs)
#'
#' @export

fdc_signature <- function(sim, obs){
  Speak <- pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.02, highQ_EP = 0)
  Shigh <- pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.02)
  Smid <- pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2)
  Slow <- pbias_fdc_target_segment(sim, obs, lowQ_EP = 1, highQ_EP = 0.7)

  FDCsign = 0.25*(abs(Speak/100) + abs(Shigh/100) + abs(Smid/100) + abs(Slow/100))
  FDCsign_df <- dplyr::tibble(Speak, Shigh, Smid, Slow, FDCsign)

  return(FDCsign_df)
}


# -------------------------------------------------------------------------
# pbias_fdc_midsegment
# -------------------------------------------------------------------------
#' @title Percent bias in the midsegment slope of a Flow Duration Curve (FDC)
#'
#' @description
#' Computes the percent bias for the midsegment slope of the flow duration
#' curves (FDC) between simulated and observed flows, delimited by
#' exceedance probability (EP) thresholds, useful for evaluating model
#' performance in intermediate flow regimes.
#'
#' @param sim numeric. Simulated flow values.
#' @param obs numeric. Observed flow values.
#' @param lowQ_EP numeric. Upper EP bound of the midsegment (e.g., 0.7 for 70\%).
#' @param highQ_EP numeric. Lower EP bound of the midsegment (e.g., 0.2 for 20\%).
#' @param na.rm logical. Whether to remove NA values before computation.
#'   Default: TRUE.
#' @param plot logical. Whether to generate a log-scale plot highlighting the
#'   selected segment. Default: FALSE.
#' @param ... additional arguments passed to \code{plot()}.
#'
#' @details
#' The EP thresholds must satisfy \eqn{1 \ge lowQ\_EP > highQ\_EP \ge 0}.
#'
#' Percent bias for the midsegment slope is calculated as:
#' \deqn{
#'   pbiasFDC_{\text{midsegment}} =
#'   \frac{\big[(S_{m1} - S_{m2}) - (O_{m1} - O_{m2})\big]}
#'        {(O_{m1} - O_{m2})} \times 100
#' }
#' where \eqn{S_{m1}}, \eqn{S_{m2}} and \eqn{O_{m1}}, \eqn{O_{m2}} are the
#' simulated and observed flows at the EP thresholds defining the segment.
#'
#' @return numeric. Percent bias for the FDC midsegment slope.
#'
#' @family Performance evaluation
#'
#' @examples
#' # Synthetic daily flow data
#' set.seed(123)
#' obs <- abs(rnorm(730, mean = 50, sd = 20))
#' sim <- obs * runif(730, min = 0.8, max = 1.5)
#'
#' # Percent bias for midsegment slope (20–70% EP)
#' pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2)
#'
#' # Plot selected segment
#' pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2,
#'                      plot = TRUE)
#' @export

pbias_fdc_midsegment <- function(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2,
                                 na.rm = TRUE, plot = FALSE, ...) {

  # Validate inputs and handle NA values
  if (na.rm) {
    valid_idx <- !is.na(sim) & !is.na(obs)
    sim <- sim[valid_idx]
    obs <- obs[valid_idx]
    if (length(sim) == 0 || length(obs) == 0) {
      stop("Both 'sim' and 'obs' must contain non-NA values after NA removal.")
    }
  }

  # Validate thresholds
  if (lowQ_EP <= highQ_EP || lowQ_EP > 1 || highQ_EP < 0) {
    stop("'lowQ_EP' must be greater than 'highQ_EP', and both must be within [0, 1].")
  }

  # Compute FDCs for observations and simulations
  compute_fdc <- function(flow) {
    sorted_flow <- sort(flow, decreasing = TRUE)
    exceedance_prob <- seq_along(sorted_flow) / (length(sorted_flow) + 1)
    data.frame(Flow = sorted_flow, ExceedanceProbability = exceedance_prob)
  }

  obs_fdc <- compute_fdc(obs)
  sim_fdc <- compute_fdc(sim)

  # Find flow values corresponding to low and high exceedance thresholds
  Qposition <- function(fdc, Qprob) {
    stats::approx(fdc$ExceedanceProbability, fdc$Flow, xout = Qprob)$y
  }

  obs_lowQ <- Qposition(obs_fdc, lowQ_EP)
  obs_highQ <- Qposition(obs_fdc, highQ_EP)
  sim_lowQ <- Qposition(sim_fdc, lowQ_EP)
  sim_highQ <- Qposition(sim_fdc, highQ_EP)

  if (any(is.na(c(obs_lowQ, obs_highQ, sim_lowQ, sim_highQ)))) {
    stop("Thresholds are outside the range of exceedance probabilities.
          Adjust 'lowQ_EP' or 'highQ_EP'.")
  }

  # Calculate denominator and percent bias
  denominator <- (obs_highQ - obs_lowQ)
  if (denominator > 0) {
    pbiasfdc <- 100 * (((sim_highQ - obs_highQ) - (sim_lowQ - obs_lowQ)) /
                         denominator)
  } else {
    pbiasfdc <- NA
    warning("Unable to compute percent bias: denominator (obs_highQ - obs_lowQ) = 0.")
  }

  # Plot the FDCs if requested
  if (plot) {
    plot(obs_fdc$ExceedanceProbability, obs_fdc$Flow, type = "l", col = "#1f78b4",
         log = "y", xlab = "Exceedance probability", ylab = "Q [m3/s]",
         main = "Flow duration curve", ...)
    graphics::lines(sim_fdc$ExceedanceProbability, sim_fdc$Flow, col = "#e31a1c")
    graphics::abline(v = c(lowQ_EP, highQ_EP), col = "gray", lty = 2)
    graphics::legend("topright", legend = c("Obs", "Sim"), col = c("#1f78b4", "#e31a1c"),
                     lty = 1, bty = "n", cex = 0.9)
  }

  return(pbiasfdc)
}


# -------------------------------------------------------------------------
# pbias_fdc_target_segment
# -------------------------------------------------------------------------
#' @title Percent bias in a Flow Duration Curve (FDC) segment
#'
#' @description
#' Computes the percent bias between simulated and observed flows in a
#' specified segment of their flow duration curves (FDC), delimited by
#' exceedance probability (EP) thresholds. Both curves are interpolated onto a
#' common EP grid (defined by `resolution`), ensuring that simulated and
#' observed flows are compared at exactly the same probabilities. Facilitates
#' evaluation of model performance across flow regimes (e.g., peak, high,
#' or low flows).
#'
#' @param sim numeric. Simulated flow values.
#' @param obs numeric. Observed flow values.
#' @param lowQ_EP numeric. Upper EP bound of the segment (e.g., 0.2 for 20\%).
#' @param highQ_EP numeric. Lower EP bound of the segment (e.g., 0 for 0\%).
#' @param resolution numeric. Resolution for EP interpolation, defining the
#'   common grid where simulated and observed flows are compared. Default: 0.0001.
#' @param na.rm logical. Whether to remove NA values before computation.
#'   Default: TRUE.
#' @param plot logical. Whether to generate a log-scale plot highlighting the
#'   selected segment. Default: FALSE.
#' @param ... additional arguments passed to \code{plot()}.
#'
#' @details
#' The EP thresholds must satisfy \eqn{1 \ge lowQ\_EP > highQ\_EP \ge 0}.
#'
#' Percent bias is calculated as:
#' \deqn{
#'   pbiasFDC_{\text{segment}} =
#'   \frac{\sum_{p=1}^P (S_p - O_p)}{\sum_{p=1}^P O_p} \times 100
#' }
#' where \eqn{S_p} and \eqn{O_p} are simulated and observed flows at
#' EP \eqn{p} on the interpolated grid.
#'
#' @return numeric. Percent bias for the specified FDC segment.
#'
#' @family Performance evaluation
#'
#' @examples
#' # Synthetic daily flow data
#' set.seed(123)
#' obs <- abs(rnorm(730, mean = 50, sd = 20))
#' sim <- obs * runif(730, min = 0.8, max = 1.5)
#'
#' # Percent bias for peak flows (0–2% EP)
#' pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.02, highQ_EP = 0)
#'
#' # Percent bias for high flows (2–20% EP)
#' pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.02)
#'
#' # Percent bias for low flows (70–100% EP)
#' pbias_fdc_target_segment(sim, obs, lowQ_EP = 1, highQ_EP = 0.7)
#'
#' # Plot selected segment
#' pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.02,
#'                          plot = TRUE)
#' @export


pbias_fdc_target_segment <- function(sim, obs, lowQ_EP, highQ_EP,
                                     resolution = 0.0001, na.rm = TRUE,
                                     plot = FALSE, ...) {

  # Validate inputs and handle NA values
  if (na.rm) {
    valid_idx <- !is.na(sim) & !is.na(obs)
    sim <- sim[valid_idx]
    obs <- obs[valid_idx]
    if (length(sim) == 0 || length(obs) == 0) {
      stop("Both 'sim' and 'obs' must contain non-NA values after NA removal.")
    }
  }

  # Validate thresholds
  if (lowQ_EP <= highQ_EP || lowQ_EP > 1 || highQ_EP < 0) {
    stop("'lowQ_EP' must be greater than 'highQ_EP', and both must be within
         [0, 1].")
  }

  # Compute FDCs for observations and simulations
  compute_fdc <- function(flow) {
    sorted_flow <- sort(flow, decreasing = TRUE)
    exceedance_prob <- seq_along(sorted_flow) / (length(sorted_flow) + 1)
    data.frame(Flow = sorted_flow, ExceedanceProbability = exceedance_prob)
  }

  obs_fdc <- compute_fdc(obs)
  sim_fdc <- compute_fdc(sim)

  # Define a common set of exceedance probabilities (EP) for interpolation
  ep_values <- seq(0, 1, by = resolution)

  # Interpolate FDCs on the common EP scale
  obs_interp <- stats::approx(obs_fdc$ExceedanceProbability, obs_fdc$Flow,
                              xout = ep_values)$y
  sim_interp <- stats::approx(sim_fdc$ExceedanceProbability, sim_fdc$Flow,
                              xout = ep_values)$y

  # Identify indices corresponding to the low and high Q thresholds (EP)
  lowQ_index <- which.min(abs(ep_values - lowQ_EP))
  highQ_index <- which.min(abs(ep_values - highQ_EP))

  # Extract the segments of flow corresponding to the range of EP
  obs_segment <- obs_interp[lowQ_index:highQ_index]
  sim_segment <- sim_interp[lowQ_index:highQ_index]

  # Remove NA values in segments
  valid_idx_int <- !is.na(sim_segment) & !is.na(obs_segment)
  obs_segment <- obs_segment[valid_idx_int]
  sim_segment <- sim_segment[valid_idx_int]

  if (length(obs_segment) == 0 || length(sim_segment) == 0) {
    stop(paste("The input data is too short or outside the specified EP range.",
               "Adjust 'lowQ_EP' or 'highQ_EP'."))
  }

  # Calculate the denominator (sum of observed values in the segment)
  denominator <- sum(obs_segment)

  # Calculate percent bias
  if (denominator > 0) {
    pbiasfdc <- 100 * sum(sim_segment - obs_segment) / denominator
  } else {
    pbiasfdc <- NA
    warning("Sum of 'obs_segment' is zero, percent bias cannot be computed.")
  }

  # Plot the FDCs if requested
  if (plot) {
    plot(ep_values, obs_interp, type = "l", col = "#1f78b4",
         ylim = range(c(obs_interp, sim_interp), na.rm = TRUE), log = "y",
         xlab = "Exceedance Probability", ylab = "Q [m3/s]",
         main = "Flow Duration Curve", ...)
    graphics::lines(ep_values, sim_interp, col = "#e31a1c")
    graphics::abline(v = c(lowQ_EP, highQ_EP), col = "gray", lty = 2)
    graphics::legend("topright", legend = c("Obs", "Sim"), col = c("#1f78b4", "#e31a1c"),
                     lty = 1, bty = "n", cex = 0.9)

    # Add labels to the threshold lines
    graphics::text(lowQ_EP, min(obs_interp, na.rm = TRUE),
                   labels = sprintf("EP: %.2f", lowQ_EP), col = "gray", pos = 4,
                   cex = 0.7, srt = 90)
    graphics::text(highQ_EP, min(obs_interp, na.rm = TRUE),
                   labels = sprintf("EP: %.2f", highQ_EP), col = "gray", pos = 4,
                   cex = 0.7, srt = 90)
  }

  # Return the percent bias value
  return(pbiasfdc)
}

