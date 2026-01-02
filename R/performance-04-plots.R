# -------------------------------------------------------------------------
# plot_timeseries
# -------------------------------------------------------------------------
#' @title Plot time series of simulated and observed data
#'
#' @description
#' Generates a time series plot comparing simulated and observed values
#' (e.g., streamflow) from the outputs of \link{evaluate_swat}.
#'
#' @param data data frame. Time series of simulated and observed values,
#' typically one of the outputs from \link{evaluate_swat} (daily_data,
#' monthly_data, or annual_data).
#' @param x_label character. Label for the x-axis. Default is "Time".
#' @param y_label character. Label for the y-axis. Default is "Q [m3/s]".
#' @param show_legend logical. Whether to display the legend. Default is TRUE.
#'
#' @return A ggplot object representing the time series plot of observed and
#' simulated values.
#'
#' @family Performance evaluation
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual theme_bw
#' element_text margin labs guides guide_legend
#' @export
#'
#' @examples
#' tmpdir <- tempdir()
#' get_swat_example(tmpdir)
#' rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
#' rch_data <- output_rch(
#'   file = rch_file, variable = "FLOW_OUTcms",
#'   target_id = 3, time_step = "daily",
#'   output_start_date = "2011-01-01"
#' )
#'
#' observed_data <- tibble::tibble(
#'   Date = qobserved$Date,
#'   value = qobserved$Flow,
#'   target_id = 3
#' )
#'
#' evaluation_results <- evaluate_swat(
#'   rch_output = rch_data,
#'   observed_data = observed_data,
#'   target_id = 3,
#'   variable = "FLOW_OUTcms",
#'   metrics = c("NSE","KGE","MAE","PBIAS"),
#'   start_dates = c("2011-01-01","2014-01-01"),
#'   end_dates = c("2013-12-31","2015-12-31")
#' )
#'
#' # Plot daily time series of observed vs simulated flow
#' plot_timeseries(evaluation_results$daily_data)

plot_timeseries <- function(data,
                            x_label = "Time",
                            y_label = "Q [m3/s]",
                            show_legend = TRUE
) {
  # Validate input data
  if (!all(c("date", "value", "source") %in% names(data))) {
    stop("Data must contain columns: date, value, and source.")
  }

  # Validate x_label
  if (!is.character(x_label) || length(x_label) != 1) {
    stop("x_label must be a single character string.")
  }

  # Validate y_label
  if (!is.character(y_label) || length(y_label) != 1) {
    stop("y_label must be a single character string.")
  }

  # Validate show_legend
  if (!is.logical(show_legend) || length(show_legend) != 1) {
    stop("show_legend must be a logical value (TRUE or FALSE).")
  }

  # Reorder source levels to have "obs" on top
  data$source <- factor(data$source, levels = c("sim", "obs"))

  # Handle missing values
  data <- stats::na.omit(data)

  # Create the plot
  p <- ggplot(data, aes(x = date, y = value, color = source, group = source)) +
    geom_line(linewidth = 0.2) +
    labs(x = x_label, y = y_label) +
    scale_color_manual(values = c("obs" = "#1f78b4", "sim" = "#e31a1c")) +
    theme_bw() +
    theme(
      text = element_text(size = 7.5),
      axis.title = element_text(size = 7.5),
      axis.text = element_text(size = 7.5),
      plot.title = element_blank(),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # Manage legend visibility
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "inside",
      legend.position.inside = c(0.92, 0.92),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.text = element_text(size = 7.5),
      legend.title = element_blank()
    ) +
      guides(color = guide_legend(
        keyheight = grid::unit(0.5, "lines"),
        keywidth = grid::unit(1, "lines"),
        title.position = "top",
        label.position = "right"
      ))
  }

  return(p)
}



# -------------------------------------------------------------------------
# plot_mean_annual_cycle
# -------------------------------------------------------------------------
#' @title Plot mean annual cycle by evaluation periods
#'
#' @description
#' Generates a plot of the mean annual cycle for simulated and observed values
#' (e.g., streamflow) by evaluation periods. Includes an option to adjust the
#' starting month for the water year.
#'
#' @param data data frame. Mean monthly values of simulated and observed data,
#' typically the \code{mean_annual_cycle} output from \link{evaluate_swat}.
#' Must include columns: \code{source}, \code{period}, \code{month}, and
#' \code{value}.
#' @param water_year logical. Whether the data should follow a water year
#' (starting in a month other than January). Default: \code{FALSE}.
#' @param water_year_start numeric. The month number (1 to 12) that the water
#' year starts. Required when \code{water_year = TRUE} and ignored when
#' \code{water_year = FALSE}.
#' @param x_label character. Label for the x-axis. Default: "Month".
#' @param y_label character. Label for the y-axis. Default: "Q [m3/s]".
#' @param show_legend logical. Whether to display the legend. Default:
#' \code{TRUE}.
#'
#' @return A \code{ggplot} object representing the mean annual cycle plot.
#' @family Performance evaluation
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual facet_wrap
#' theme_bw element_text margin labs guides guide_legend
#' @export
#'
#' @examples
#' tmpdir <- tempdir()
#' get_swat_example(tmpdir)
#' rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
#' rch_data <- output_rch(
#'   file = rch_file, variable = "FLOW_OUTcms",
#'   target_id = 3, time_step = "daily",
#'   output_start_date = "2011-01-01"
#' )
#'
#' observed_data <- tibble::tibble(
#'   Date = qobserved$Date,
#'   value = qobserved$Flow,
#'   target_id = 3
#' )
#'
#' evaluation_results <- evaluate_swat(
#'   rch_output = rch_data,
#'   observed_data = observed_data,
#'   target_id = 3,
#'   variable = "FLOW_OUTcms",
#'   metrics = c("NSE","KGE","MAE","PBIAS"),
#'   start_dates = c("2011-01-01","2014-01-01"),
#'   end_dates = c("2013-12-31","2015-12-31")
#' )
#'
#' # Plot mean annual cycle by evaluation periods
#' plot_mean_annual_cycle(evaluation_results$mean_annual_cycle)

plot_mean_annual_cycle <- function(data,
                                   water_year = FALSE,
                                   water_year_start,
                                   x_label = "Month",
                                   y_label = "Q [m3/s]",
                                   show_legend = TRUE) {

  # Validate arguments
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }

  if (!all(c("source", "period", "month", "value") %in% names(data))) {
    stop("Data must contain columns: source, period, month, and value.")
  }

  if (!is.logical(water_year)) {
    stop("The 'water_year' argument must be a logical value (TRUE or FALSE).")
  }

  # Validate 'water_year_start' argument
  if (water_year && missing(water_year_start)) {
    stop("The 'water_year_start' argument must be provided when 'water_year' is",
         " TRUE.")
  }

  if (water_year && (!is.numeric(water_year_start) ||
                     water_year_start < 1 || water_year_start > 12)) {
    stop("The 'water_year_start' must be a numeric value between 1 and 12",
         " when 'water_year' is TRUE.")
  }

  if (!is.character(x_label) || length(x_label) != 1) {
    stop("The 'x_label' argument must be a single character string.")
  }

  if (!is.character(y_label) || length(y_label) != 1) {
    stop("The 'y_label' argument must be a single character string.")
  }

  if (!is.logical(show_legend)) {
    stop("The 'show_legend' argument must be a logical value (TRUE or FALSE).")
  }

  # Reorder months if using water year
  if (water_year && water_year_start != 1) {
    new_order <- c(water_year_start:12, 1:(water_year_start - 1))
  } else {
    new_order <- 1:12
  }
  data$month <- factor(data$month, levels = new_order)

  # Reorder source levels to have "obs" on top
  data$source <- factor(data$source, levels = c("sim", "obs"))

  # Remove missing values
  data <- stats::na.omit(data)

  # Create the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = month, y = value, color = source,
                                          group = source)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_color_manual(values = c("obs" = "#1f78b4", "sim" = "#e31a1c")) +
    ggplot2::facet_wrap(~period) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 7.5),
      axis.title = ggplot2::element_text(size = 7.5),
      axis.text = ggplot2::element_text(size = 7.5),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Manage legend visibility
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.92, 0.92),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.text = ggplot2::element_text(size = 7.5),
      legend.title = ggplot2::element_blank()
    ) +
      ggplot2::guides(color = ggplot2::guide_legend(
        keyheight = grid::unit(0.5, "lines"),
        keywidth = grid::unit(1, "lines"),
        title.position = "top",
        label.position = "right"
      ))
  }

  return(p)
}



# -------------------------------------------------------------------------
# plot_fdc
# -------------------------------------------------------------------------
#' @title Plot Flow Duration Curve (FDC) by evaluation periods
#'
#' @description
#' Generates a Flow Duration Curve (FDC) comparing simulated and observed
#' values (e.g., streamflow), grouped by evaluation periods.
#'
#' @param data data frame. Daily flow values of simulated and observed data,
#' typically the \code{daily_fdc_data} output from \link{evaluate_swat}.
#' Must include columns \code{date}, \code{source}, \code{period}, and \code{value}.
#' @param exceedance_thresholds numeric vector. Exceedance probability
#' thresholds (e.g., \code{c(0, 0.02, 0.2, 0.75, 1)}) for vertical reference
#' lines. Default: \code{NULL} (no lines added).
#' @param x_label character. Label for the x-axis. Default:
#' "\% of time flow is equaled or exceeded".
#' @param y_label character. Label for the y-axis. Default: "Q [m3/s]".
#' @param epsilon_value numeric. Value added to flows before log transformation
#' to avoid issues with zeros. Default: \code{0.01}.
#' @param show_legend logical. Whether to display the legend. Default: \code{TRUE}.
#'
#' @return A \code{ggplot} object visualizing the Flow Duration Curve (FDC).
#' @family Performance evaluation
#' @importFrom ggplot2 ggplot aes geom_line scale_y_log10 scale_x_continuous
#' scale_color_manual theme_bw theme element_text element_blank margin
#' expand_limits facet_wrap geom_vline element_rect guides guide_legend
#' @importFrom dplyr filter group_by reframe mutate
#' @importFrom grid unit
#' @export
#'
#' @examples
#' tmpdir <- tempdir()
#' get_swat_example(tmpdir)
#' rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
#' rch_data <- output_rch(
#'   file = rch_file, variable = "FLOW_OUTcms",
#'   target_id = 3, time_step = "daily",
#'   output_start_date = "2011-01-01"
#' )
#'
#' observed_data <- tibble::tibble(
#'   Date = qobserved$Date,
#'   value = qobserved$Flow,
#'   target_id = 3
#' )
#'
#' evaluation_results <- evaluate_swat(
#'   rch_output = rch_data,
#'   observed_data = observed_data,
#'   target_id = 3,
#'   variable = "FLOW_OUTcms",
#'   metrics = c("NSE","KGE","MAE","PBIAS"),
#'   start_dates = c("2011-01-01","2014-01-01"),
#'   end_dates = c("2013-12-31","2015-12-31")
#' )
#'
#' # Plot Flow Duration Curve
#' plot_fdc(evaluation_results$daily_fdc_data,
#'          exceedance_thresholds = c(0, 0.02, 0.2, 0.75, 1))

plot_fdc <- function(data,
                     exceedance_thresholds = NULL,
                     x_label = "% of time flow is equaled or exceeded",
                     y_label = "Q [m3/s]",
                     epsilon_value = 0.01,
                     show_legend = TRUE
) {

  # Validate arguments
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }

  if (!all(c("source", "date", "value", "period") %in% names(data))) {
    stop("Data must contain columns: source, date, value, and period.")
  }

  if (!is.null(exceedance_thresholds)) {
    if (!is.numeric(exceedance_thresholds)) {
      stop("The 'exceedance_thresholds' argument must be numeric or NULL.")
    }
    if (any(exceedance_thresholds < 0 | exceedance_thresholds > 1)) {
      stop("All values of 'exceedance_thresholds' must be between 0 and 1.")
    }
  }

  if (!is.character(x_label) || length(x_label) != 1) {
    stop("The 'x_label' argument must be a single character string.")
  }

  if (!is.character(y_label) || length(y_label) != 1) {
    stop("The 'y_label' argument must be a single character string.")
  }

  if (!is.logical(show_legend)) {
    stop("The 'show_legend' argument must be a logical value (TRUE or FALSE).")
  }

  # Filter out NA values
  data <- dplyr::filter(data, !is.na(value))

  # Apply epsilon_value to avoid issues with zero values before log transformation
  data <- data %>%
    dplyr::mutate(value = value + epsilon_value)

  # Extract data from each source
  obs_data <- dplyr::filter(data, source == "obs")
  sim_data <- dplyr::filter(data, source == "sim")

  # Identify common dates
  valid_dates <- unique(c(obs_data$date, sim_data$date))
  valid_dates <- valid_dates[valid_dates %in% obs_data$date &
                               valid_dates %in% sim_data$date]

  # Filter data to include only common dates
  data <- dplyr::filter(data, date %in% valid_dates)

  # Compute the Flow Duration Curve (FDC)
  fdc_data <- dplyr::group_by(data, source, period) %>%
    dplyr::reframe(
      Flow = sort(value, decreasing = TRUE),
      ExceedanceProbability = seq_along(value) / (length(value) + 1)
    )

  # Define Y-axis range
  y_min <- min(fdc_data$Flow, na.rm = TRUE)
  y_max <- max(fdc_data$Flow, na.rm = TRUE)

  # Define reference points on the Y-axis
  y_breaks <- c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
  y_breaks <- y_breaks[y_breaks >= y_min & y_breaks <= y_max]

  # Add limits close to the data range
  lower_limit <- 10^(floor(log10(y_min)))
  upper_limit <- 10^(ceiling(log10(y_max)))
  y_breaks <- unique(c(lower_limit, y_breaks, upper_limit))
  y_labels <- as.character(y_breaks)

  # Build the plot
  p <- ggplot2::ggplot(fdc_data, ggplot2::aes(
    x = ExceedanceProbability,
    y = Flow, color = source)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_y_log10(
      name = y_label,
      breaks = y_breaks,
      labels = y_labels
    ) +
    ggplot2::scale_x_continuous(
      name = x_label,
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0%", "25%", "50%", "75%", "100%")
    ) +
    ggplot2::scale_color_manual(values = c("obs" = "#1f78b4", "sim" = "#e31a1c")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 7.5),
      axis.title = ggplot2::element_text(size = 7.5),
      axis.text = ggplot2::element_text(size = 7.5),
      plot.title = ggplot2::element_text(size = 7, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(~period, scales = "fixed")

  # Adjust plot limits
  p <- p + ggplot2::expand_limits(y = c(lower_limit, upper_limit))

  # Add reference lines if 'exceedance_thresholds' is not NULL
  if (!is.null(exceedance_thresholds) && length(exceedance_thresholds) > 0) {
    p <- p + ggplot2::geom_vline(
      xintercept = exceedance_thresholds,
      linetype = "dashed", color = "gray", linewidth = 0.25
    )
  }

  # Configure legend
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.92, 0.92),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.text = ggplot2::element_text(size = 7.5),
      legend.title = ggplot2::element_blank()
    ) +
      ggplot2::guides(color = ggplot2::guide_legend(
        keyheight = grid::unit(0.5, "lines"),
        keywidth = grid::unit(1, "lines"),
        title.position = "top",
        label.position = "right"
      ))
  }

  return(p)
}

