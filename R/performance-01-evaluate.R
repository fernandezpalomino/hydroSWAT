# -------------------------------------------------------------------------
# evaluate_swat
# -------------------------------------------------------------------------
#' @title Evaluate SWAT model performance
#'
#' @description
#' Evaluates SWAT model performance by comparing simulated and observed data
#' using user-selected or all available goodness-of-fit (GOF) metrics at daily,
#' monthly, and annual time steps.
#'
#' @param rch_output data frame. SWAT model output for one or more reaches,
#' typically generated with \link{output_rch}. Must include columns RCH
#' (reach ID), MON (dates), and the simulated variable of interest
#' (e.g., FLOW_OUTcms).
#' @param observed_data data frame. Observed data for one or more reaches,
#' including columns Date (observation date), value (observed values),
#' and target_id (reach ID).
#' @param target_id numeric. Reach ID to evaluate.
#' @param variable character. Name of the variable to evaluate
#' (e.g., "FLOW_OUTcms"). Use \link{get_swat_vars}("rch") to list available
#' variables.
#' @param metrics character vector. Goodness-of-fit metrics to compute
#' (e.g., c("NSE","KGE","MAE","PBIAS")).
#' @param start_dates character vector. Start dates for evaluation periods in
#' "YYYY-MM-DD" format.
#' @param end_dates character vector. End dates for evaluation periods in
#' "YYYY-MM-DD" format. Must match length of start_dates.
#' @param all_metrics logical. Whether to calculate all available metrics.
#' Defaults to FALSE.
#'
#' @return A list of data frames:
#' \itemize{
#'   \item evaluation_metric: Evaluation metrics for daily, monthly, and annual
#'   time steps. Daily metrics are included only if daily data are available.
#'   \item daily_data: Daily observed and simulated values (if available).
#'   \item monthly_data: Monthly observed and simulated values with the same
#'   structure as daily_data.
#'   \item annual_data: Annual observed and simulated values with the same
#'   structure as daily_data. These three data frames (daily_data, monthly_data,
#'   annual_data) are designed for use with \link{plot_timeseries}.
#'   \item mean_annual_cycle: Mean annual cycle of observed and simulated values.
#'   Columns: source, period, month, value. This object is used by
#'   \link{plot_mean_annual_cycle}.
#'   \item daily_fdc_data: Daily data for flow duration curves (FDC). Columns:
#'   source, period, date, value. This object is used by \link{plot_fdc}.
#' }
#'
#' @family Performance evaluation
#' @importFrom dplyr filter select rename mutate bind_rows everything
#' @importFrom lubridate ymd
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
#' head(evaluation_results$evaluation_metric)

evaluate_swat <- function(rch_output, observed_data, target_id, variable,
                          metrics, start_dates, end_dates, all_metrics = FALSE) {

  # Input validation
  if (!is.data.frame(rch_output)) {
    stop("`rch_output` must be a data frame.")
  }

  # Validate the variable first
  if (!is.character(variable) || length(variable) != 1) {
    stop("`variable` must be a single character string.")
  }

  available_vars <- get_swat_vars("rch")
  if (!variable %in% available_vars) {
    stop(paste("`variable` must be one of the following available variables:",
               paste(available_vars, collapse = ", ")))
  }

  # Validate columns in `rch_output`
  required_columns_rch_output <- c("RCH", "MON", variable)
  missing_columns_rch_output <- setdiff(required_columns_rch_output,
                                        colnames(rch_output))
  if (length(missing_columns_rch_output) > 0) {
    stop(paste("`rch_output` is missing the following required columns:",
               paste(missing_columns_rch_output, collapse = ", ")))
  }

  if (!is.data.frame(observed_data)) {
    stop("`observed_data` must be a data frame.")
  }

  # Validate columns in `observed_data`
  required_columns_observed_data <- c("Date", "value", "target_id")
  missing_columns_observed_data <- setdiff(required_columns_observed_data,
                                           colnames(observed_data))
  if (length(missing_columns_observed_data) > 0) {
    stop(paste("`observed_data` is missing the following required columns:",
               paste(missing_columns_observed_data, collapse = ", ")))
  }

  if (!is.numeric(target_id) || length(target_id) != 1) {
    stop("`target_id` must be a numeric value.")
  }
  if (!is.character(metrics) || length(metrics) == 0) {
    stop("`metrics` must be a character vector with at least one element.")
  }

  # Get valid metrics from the get_gof_metrics function
  valid_metrics <- get_gof_metrics()

  # Validate provided metrics
  if (!all(metrics %in% valid_metrics)) {
    invalid_metrics <- metrics[!metrics %in% valid_metrics]
    stop(paste("`metrics` contains invalid metric(s):",
               paste(invalid_metrics, collapse = ", "),
               ". Valid metrics are:",
               paste(valid_metrics, collapse = ", ")))
  }

  if (!is.character(start_dates) || length(start_dates) == 0 ||
      any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_dates))) {
    stop("`start_dates` must be a character vector of valid dates in 'YYYY-MM-DD' format.")
  }
  if (!is.character(end_dates) || length(end_dates) == 0 ||
      any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", end_dates))) {
    stop("`end_dates` must be a character vector of valid dates in 'YYYY-MM-DD' format.")
  }
  if (length(start_dates) != length(end_dates)) {
    stop("`start_dates` and `end_dates` must have the same length.")
  }
  if (!is.logical(all_metrics) || length(all_metrics) != 1) {
    stop("`all_metrics` must be a single logical value.")
  }

  # Filter and select relevant columns
  rch_output <- rch_output %>%
    filter(RCH == !!target_id) %>%
    select(MON, RCH, !!variable)

  observed_data <- observed_data %>%
    filter(target_id == !!target_id) %>%
    select(Date, target_id, value) %>%
    dplyr::rename(MON = Date, RCH = target_id, !!variable := value)

  # Summarize SWAT and observed data
  summarized_swat <- summarize_swat_output(rch_output)
  summarized_observed <- summarize_swat_output(observed_data)

  # Check if daily data exists
  has_daily_data <- !is.null(summarized_swat$daily) &&
    !is.null(summarized_observed$daily)

  evaluation_metric <- NULL
  daily_fdc_data <- NULL

  if (has_daily_data) {
    # Compute daily, monthly, and annual metrics
    eval_results_daily <- compute_evaluation_metric(
      summarized_swat$daily, summarized_observed$daily, target_id,
      variable, metrics, start_dates, end_dates, all_metrics
    ) %>% mutate(time_step = "daily")

    eval_results_monthly <- compute_evaluation_metric(
      summarized_swat$monthly, summarized_observed$monthly, target_id,
      variable, metrics, start_dates, end_dates, all_metrics
    ) %>% mutate(time_step = "monthly")

    eval_results_annual <- compute_evaluation_metric(
      summarized_swat$annual, summarized_observed$annual, target_id,
      variable, metrics, start_dates, end_dates, all_metrics
    ) %>% mutate(time_step = "annual")

    evaluation_metric <- bind_rows(
      eval_results_daily, eval_results_monthly, eval_results_annual
    )

    # Compute FDC only if daily data exists
    daily_fdc_data <- split_fdc_data(
      summarized_observed$daily, summarized_swat$daily, start_dates, end_dates,
      variable
    )
  } else {
    # If no daily data, compute monthly and annual metrics
    eval_results_monthly <- compute_evaluation_metric(
      summarized_swat$monthly, summarized_observed$monthly, target_id,
      variable, metrics, start_dates, end_dates, all_metrics
    ) %>% mutate(time_step = "monthly")

    eval_results_annual <- compute_evaluation_metric(
      summarized_swat$annual, summarized_observed$annual, target_id,
      variable, metrics, start_dates, end_dates, all_metrics
    ) %>% mutate(time_step = "annual")

    evaluation_metric <- bind_rows(eval_results_monthly, eval_results_annual)
  }

  # Compute mean annual cycle
  mean_annual_cycle <- compute_climatology(
    summarized_observed$monthly, summarized_swat$monthly, start_dates, end_dates,
    variable
  )

  # Generate tidy daily data if available
  daily_data <- NULL
  if (has_daily_data) {
    daily_data <- bind_rows(
      summarized_observed$daily %>% mutate(source = "obs"),
      summarized_swat$daily %>% mutate(source = "sim")
    ) %>%
      select(source, MON, !!sym(variable)) %>%
      rename(date = MON, value = !!sym(variable))
  }

  # Generate tidy monthly data
  monthly_data <- bind_rows(
    summarized_observed$monthly %>% mutate(source = "obs"),
    summarized_swat$monthly %>% mutate(source = "sim")
  ) %>%
    select(source, MON, !!sym(variable)) %>%
    rename(date = MON, value = !!sym(variable))

  # Generate tidy annual data
  annual_data <- bind_rows(
    summarized_observed$annual %>% mutate(source = "obs"),
    summarized_swat$annual %>% mutate(source = "sim")
  ) %>%
    select(source, MON, !!sym(variable)) %>%
    rename(date = MON, value = !!sym(variable))

  return(list(
    evaluation_metric = evaluation_metric %>%
      select(variable, target_id, time_step, everything()),
    daily_data = daily_data,
    monthly_data = monthly_data,
    annual_data = annual_data,
    mean_annual_cycle = mean_annual_cycle,
    daily_fdc_data = daily_fdc_data
  ))
}




# -------------------------------------------------------------------------
# compute_evaluation_metric
# -------------------------------------------------------------------------

compute_evaluation_metric <- function(simulated_data, observed_data, target_id,
                                      variable, metrics, start_dates,
                                      end_dates, all_metrics = FALSE) {

  # Validate that start_dates and end_dates have the same length
  if (length(start_dates) != length(end_dates)) {
    stop("start_dates and end_dates must have the same number of elements")
  }

  # Convert dates to Date format
  start_dates <- as.Date(start_dates)
  end_dates <- as.Date(end_dates)

  # Filter simulated data by target_id and select the variable dynamically
  simulated_results <- simulated_data %>%
    dplyr::filter(RCH == !!target_id) %>%
    dplyr::select(MON, sim = !!sym(variable))

  # Filter observed data by target_id and select the variable dynamically
  observed_results <- observed_data %>%
    dplyr::filter(RCH == !!target_id) %>%
    dplyr::select(MON, obs = !!sym(variable))

  # Apply evaluation for each period
  validation_metrics <- dplyr::bind_rows(
    lapply(seq_along(start_dates), function(i) {
      start_date <- start_dates[i]
      end_date <- end_dates[i]

      # Convert MON to Date format if it is numeric (year-based)
      if (is.numeric(simulated_results$MON) & is.numeric(observed_results$MON)) {
        simulated_results <- simulated_results %>%
          dplyr::mutate(MON = lubridate::make_date(year = MON, month = 1, day = 1))

        observed_results <- observed_results %>%
          dplyr::mutate(MON = lubridate::make_date(year = MON, month = 1, day = 1))
      }

      # Filter data for the given date range
      period_simulated <- simulated_results %>%
        dplyr::filter(MON >= start_date & MON <= end_date)
      period_observed <- observed_results %>%
        dplyr::filter(MON >= start_date & MON <= end_date)

      # Merge data by date
      merged_data <- dplyr::left_join(period_simulated, period_observed, by = "MON")

      # Compute metrics
      if (all_metrics) {
        metric_results <- gof_metrics(merged_data$obs, merged_data$sim,
                                      all_metrics = TRUE)
      } else {
        metric_results <- dplyr::bind_rows(lapply(metrics, function(m) {
          tibble::tibble("gof" = m, "value" = gof_metrics(merged_data$obs, #modifyed
                                                          merged_data$sim, m))
        }))
      }

      # Transform to wide format using pivot_wider()
      metric_values <- metric_results %>%
        tidyr::pivot_wider(names_from = gof, values_from = value)

      # Create a row with metadata and metrics
      tibble::tibble(variable = variable,
                     target_id = target_id,
                     start_date = start_date,
                     end_date = end_date) %>%
        dplyr::bind_cols(metric_values)
    })
  )

  return(validation_metrics)
}

# -------------------------------------------------------------------------
# compute_climatology
# -------------------------------------------------------------------------
compute_climatology <- function(observed_monthly, simulated_monthly,
                                start_dates, end_dates, variable) {
  climatology_results <- dplyr::bind_rows(lapply(seq_along(start_dates), function(i) {
    start_date <- as.Date(start_dates[i])
    end_date <- as.Date(end_dates[i])

    # Filter observed and simulated data for the period
    period_obs <- observed_monthly %>%
      dplyr::filter(MON >= start_date & MON <= end_date)

    period_sim <- simulated_monthly %>%
      dplyr::filter(MON >= start_date & MON <= end_date)

    # Identify dates with data in period_obs (excluding NA and NaN in the variable)
    dates_with_data <- period_obs %>%
      dplyr::filter(!is.na(!!rlang::sym(variable)) & !is.nan(!!rlang::sym(variable))) %>%
      dplyr::pull(MON)

    # Filter period_obs and period_sim using dates with data in period_obs
    period_obs <- period_obs %>%
      dplyr::filter(MON %in% dates_with_data)

    period_sim <- period_sim %>%
      dplyr::filter(MON %in% dates_with_data)

    # Compute climatology for observed data
    climatology_obs <- period_obs %>%
      dplyr::group_by(month = lubridate::month(MON)) %>%
      dplyr::summarize(value = mean(!!rlang::sym(variable), na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::mutate(period = paste(start_date, end_date, sep = " to "),
                    source = "obs")

    # Compute climatology for simulated data
    climatology_sim <- period_sim %>%
      dplyr::group_by(month = lubridate::month(MON)) %>%
      dplyr::summarize(value = mean(!!rlang::sym(variable), na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::mutate(period = paste(start_date, end_date, sep = " to "),
                    source = "sim")

    # Combine observed and simulated climatology data
    climatology_data <- dplyr::bind_rows(climatology_obs, climatology_sim) %>%
      dplyr::select(source, period, month, value)

    climatology_data
  }))

  return(climatology_results)
}


# -------------------------------------------------------------------------
# split_fdc_data
# -------------------------------------------------------------------------
split_fdc_data <- function(observed_daily, simulated_daily,
                           start_dates, end_dates, variable) {

  # Identify valid observed dates (non-NA and non-NaN values )
  valid_dates_obs <- observed_daily %>%
    dplyr::filter(!is.na(.data[[variable]]) & !is.nan(.data[[variable]])) %>%
    dplyr::pull(MON)

  # Filter the observed data based on valid dates
  observed_daily_valid <- observed_daily %>%
    dplyr::filter(MON %in% valid_dates_obs)

  # Process the observed data
  fdc_obs <- dplyr::bind_rows(lapply(seq_along(start_dates), function(i) {
    start_date <- as.Date(start_dates[i])
    end_date <- as.Date(end_dates[i])

    # Filter data within the specified period
    period_data <- observed_daily_valid %>%
      dplyr::filter(MON >= start_date & MON <= end_date)

    tibble::tibble(
      source = "obs",  # Label as observed data
      period = paste(start_date, end_date, sep = " to "),
      date = period_data$MON,
      value = period_data[[variable]]  # Dynamically use the selected variable
    )
  }))

  # Filter the simulated data based on valid observed dates
  simulated_daily_valid <- simulated_daily %>%
    dplyr::filter(MON %in% valid_dates_obs)

  # Process the simulated data
  fdc_sim <- dplyr::bind_rows(lapply(seq_along(start_dates), function(i) {
    start_date <- as.Date(start_dates[i])
    end_date <- as.Date(end_dates[i])

    # Filter data within the specified period
    period_data <- simulated_daily_valid %>%
      dplyr::filter(MON >= start_date & MON <= end_date)

    tibble::tibble(
      source = "sim",  # Label as simulated data
      period = paste(start_date, end_date, sep = " to "),
      date = period_data$MON,
      value = period_data[[variable]]  # Dynamically use the selected variable
    )
  }))

  # Combine both observed and simulated results into a single data frame
  fdc_results <- dplyr::bind_rows(fdc_obs, fdc_sim)

  return(fdc_results)
}
