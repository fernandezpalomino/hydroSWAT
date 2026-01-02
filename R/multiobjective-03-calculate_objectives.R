# -------------------------------------------------------------------------
# calculate_objectives
# -------------------------------------------------------------------------
#' @title Compute calibration objectives for the SWAT model
#'
#' @description
#' Computes objective metrics for multiobjective SWAT model calibration.
#' Users can define multiple objectives, each linked to spatial units such
#' as river reaches (\code{"rch"}), subbasins (\code{"sub"}), or HRUs
#' (\code{"hru"}). Each objective specifies target location, variable,
#' performance metric, and time step. This function is called by
#' \code{calibrate_swat()} during optimization.
#'
#' @param parameter_values numeric. Vector of calibration parameter values.
#' @param fn_args list. Named list with inputs/configuration:
#' \itemize{
#'   \item \code{objectives}: Calibration objectives.
#'   \item \code{observed_data}: Observed data for comparison.
#'   \item \code{output_config}: How to extract SWAT outputs.
#'   \item \code{parameter_info}: Parameter specifications.
#'   \item \code{subbasins}: Subbasin IDs to modify (\code{NULL} all).
#'   \item \code{swat_exe_path}: Path to the SWAT executable.
#' }
#'
#' @details
#' \strong{objectives}: list of objectives. Each element contains:
#' \itemize{
#'   \item \code{element}: \code{"rch"}, \code{"sub"}, or \code{"hru"}.
#'   \item \code{variable}: e.g., \code{"FLOW_OUTcms"}.
#'   \item \code{target_id}: IDs to evaluate (type depends on element).
#'   \item \code{metric}: performance metric (see \link{gof_metrics}).
#'   \item \code{calib_time_step}: \code{"daily"} or \code{"monthly"}.
#'   \item \code{calib_start_date}: Start date of the calibration period.
#'   \item \code{calib_end_date}: End date of the calibration period.
#' }
#'
#' Example objectives (daily, reach 3, 2011-2013):
#' \preformatted{
#' objectives <- list(
#'   list(
#'     element          = "rch",
#'     variable         = "FLOW_OUTcms",
#'     target_id        = 3,
#'     metric           = "NSE",
#'     calib_time_step  = "daily",
#'     calib_start_date = "2011-01-01",
#'     calib_end_date   = "2013-12-31"
#'   ),
#'   list(
#'     element          = "rch",
#'     variable         = "FLOW_OUTcms",
#'     target_id        = 3,
#'     metric           = "log_NSE",
#'     calib_time_step  = "daily",
#'     calib_start_date = "2011-01-01",
#'     calib_end_date   = "2013-12-31"
#'   )
#' )
#' }
#'
#' \strong{observed_data}: nested list by time step (\code{"daily"} or
#' \code{"monthly"}), spatial unit, and variable. Each terminal element is a
#' tibble with columns \code{Date}, \code{value}, \code{target_id}.
#'
#' Example structure (daily, reach 3):
#' \preformatted{
#' observed_data <- list(
#'   daily = list(
#'     rch = list(
#'       "FLOW_OUTcms" = tibble::tibble(
#'         Date      = seq.Date(as.Date("2010-01-01"),
#'                              as.Date("2013-12-31"), by = "day"),
#'         value     = runif(1461, 1, 50),
#'         target_id = 3
#'       )
#'     )
#'   )
#' )
#' }
#'
#' \strong{output_config}: how to read SWAT outputs for each element. These
#' arguments are passed to \link{output_rch}, \link{output_sub}, or
#' \link{output_hru}, as appropriate.
#' \itemize{
#'   \item \code{file}: e.g., \code{"output.rch"}.
#'   \item \code{variable}: variables to extract.
#'   \item \code{target_id}: IDs to keep (\code{NULL} all).
#'   \item \code{time_step}: \code{"daily"} or \code{"monthly"}.
#'   \item \code{output_start_date}: date when SWAT begins writing outputs
#'         (typically after warm-up).
#' }
#'
#' Example (daily reach output; with a 1-year warm-up, outputs begin on
#' 2011-01-01):
#' \preformatted{
#' output_config <- list(
#'   rch = list(
#'     file              = "output.rch",
#'     variable          = c("FLOW_OUTcms"),
#'     target_id         = c(3),
#'     time_step         = "daily",
#'     output_start_date = "2011-01-01"
#'   )
#' )
#' }
#'
#' \strong{parameter_info}: data frame/tibble describing parameter changes,
#' passed to \link{change_multiple_parameters}. Columns:
#' \itemize{
#'   \item \code{component}: e.g., \code{".gw"}, \code{".mgt"}, \code{".hru"}.
#'   \item \code{parameter}: parameter name.
#'   \item \code{value}: value/adjustment factor (filled internally).
#'   \item \code{method}: \code{"v"} (value) or \code{"r"} (relative).
#'   \item \code{version} (opt.): \code{"SWAT"} or \code{"SWAT_T"}.
#'   \item \code{plant_type} (opt.): plant type for \code{plant.dat}.
#' }
#' See \link{change_multiple_parameters} and \link{change_parameter} for
#' details; list of available parameters: \link{get_swat_parameters}.
#'
#' \strong{swat_exe_path}: path to the SWAT executable.
#'
#' \strong{subbasins} (optional): vector of subbasin IDs (\code{NULL} all).
#'
#' @return
#' Numeric vector of objective values (one per objective in \code{objectives}).
#'
#' @family Multiobjective calibration
#' @seealso \link{run_swat_exe}
#' @note When used via \code{calibrate_swat()} with
#' \code{fn = calculate_objectives} (the package default),
#' \code{fn_args} is validated upstream to avoid per-iteration overhead.
#' If you pass a custom \code{fn}, \code{calibrate_swat()} does not validate
#' its inputs; your function must validate \code{fn_args} (or you must
#' validate it before calling \code{calibrate_swat()}).
#'
#' @examples
#' \donttest{
#' # Windows-only example (runs SWAT once to compute objectives).
#' # On non-Windows, skip or provide your own compiled swat.exe.
#' if (.Platform$OS.type == "windows") {
#'
#'   tmpdir <- tempdir()
#'   swat_project <- get_swat_example(tmpdir)
#'
#'   create_calibration_project(
#'     swat_TxtInOut   = swat_project,
#'     destination_dir = tmpdir,
#'     project_name    = "calc_obj_example",
#'     set_working_dir = TRUE
#'   )
#'
#'   # SWAT executable (if download fails, set your local path below)
#'   swat_exe <- tryCatch(
#'     download_swat_exe(dest_dir = tmpdir, type = "release"),
#'     error = function(e) {
#'       message("download_swat_exe() failed: ", conditionMessage(e))
#'       "path/to/swat.exe"  # <-- set your local executable
#'     }
#'   )
#'
#'   # Simulation setup (warm-up = 1 year)
#'   setup_swat_out <- setup_swat(
#'     sim_start_date = "2010-01-01",
#'     sim_end_date   = "2013-12-31",
#'     time_step      = "daily",
#'     NYSKIP         = 1,
#'     rch_vars       = c("FLOW_OUTcms")
#'   )
#'
#'   # Objectives (NSE and log_NSE at reach 3; 2011-01-01 to 2013-12-31)
#'   objectives <- list(
#'     list(
#'       element          = "rch",
#'       variable         = "FLOW_OUTcms",
#'       target_id        = 3,
#'       metric           = "NSE",
#'       calib_time_step  = "daily",
#'       calib_start_date = "2011-01-01",
#'       calib_end_date   = "2013-12-31"
#'     ),
#'     list(
#'       element          = "rch",
#'       variable         = "FLOW_OUTcms",
#'       target_id        = 3,
#'       metric           = "log_NSE",
#'       calib_time_step  = "daily",
#'       calib_start_date = "2011-01-01",
#'       calib_end_date   = "2013-12-31"
#'     )
#'   )
#'
#'   # Observed flows (qobserved) provided with the package
#'   observed_data <- list(
#'     daily = list(
#'       rch = list(
#'         "FLOW_OUTcms" = tibble::tibble(
#'           Date      = qobserved$Date,
#'           value     = qobserved$Flow,
#'           target_id = 3
#'         )
#'       )
#'     )
#'   )
#'
#'   # What to read from SWAT outputs
#'   output_config <- list(
#'     rch = list(
#'       file              = "output.rch",
#'       variable          = c("FLOW_OUTcms"),
#'       target_id         = c(3),
#'       time_step         = "daily",
#'       output_start_date = setup_swat_out$output_start_date
#'     )
#'   )
#'
#'   # Parameters and bounds (values filled when evaluating objectives)
#'   parameter_info <- tibble::tibble(
#'     component  = c(".gw", ".gw", ".gw", ".gw", ".hru", ".hru", ".mgt"),
#'     parameter  = c("GW_DELAY", "ALPHA_BF", "GWQMN", "RCHRG_DP",
#'                    "SURLAG", "ESCO", "CN2"),
#'     value      = NA,
#'     method     = c("v", "v", "v", "v", "v", "v", "r"),
#'     version    = rep("SWAT", 7),
#'     plant_type = NA,
#'     min        = c(10, 0.50, 700, 0.05, 1.0, 0.90, -0.05),
#'     max        = c(50, 1.00, 750, 0.50, 2.0, 1.00,  0.05)
#'   )
#'
#'   # Assemble arguments for objective computation
#'   fn_args <- list(
#'     parameter_info = parameter_info,
#'     observed_data  = observed_data,
#'     output_config  = output_config,
#'     objectives     = objectives,
#'     subbasins      = NULL,
#'     swat_exe_path  = swat_exe
#'   )
#'
#'   # Single evaluation for a candidate parameter vector
#'   obj_vals <- calculate_objectives(
#'     parameter_values = c(30, 0.8, 725, 0.2, 1.3, 0.95, 0.00),
#'     fn_args          = fn_args
#'   )
#'   print(round(obj_vals, 3))
#' } else {
#'   message("Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.")
#' }
#' }
#'
#' @export


calculate_objectives <- function(parameter_values, fn_args) {

  # Input data
  objectives <- fn_args$objectives
  observed_data <- fn_args$observed_data
  output_config <- fn_args$output_config
  parameter_info <- fn_args$parameter_info
  subbasins <- fn_args$subbasins
  swat_exe_path  <- fn_args$swat_exe_path


  # Validate parameter lengths
  if (length(parameter_values) != nrow(parameter_info))
    stop("The length of 'parameter_values' must match the number of rows in ",
         "'parameter_info'.")

  # Change parameters with calibration values
  parameter_info$value <- parameter_values
  change_output <- change_multiple_parameters(parameter_info, subbasins)

  # Run SWAT model
  swat_exit_code <- run_swat_exe(swat_exe_path)

  if (swat_exit_code != 0) {
    stop("SWAT execution failed. Exit code: ", swat_exit_code,
         ". Check SWAT configuration and input files.")
  }


  # Retrieve simulated results
  simulated_results <- list()

  # Extract river reach data (output.rch)
  if (!is.null(output_config$rch)) {
    simulated_results$rch <- extract_output(
      output_config$rch, output_rch, "output_rch_args"
    )
  }

  # Extract subbasin data (output.sub)
  if (!is.null(output_config$sub)) {
    vars <- output_config$sub$variable
    if(!"AREAkm2" %in% vars){
      output_config$sub$variable <- c("AREAkm2", vars)
    }

    simulated_results$sub <- extract_output(
      output_config$sub, output_sub, "output_sub_args"
    )
  }

  # Extract HRU data (output.hru)
  if (!is.null(output_config$hru)) {
    simulated_results$hru <- extract_output(
      output_config$hru, output_hru, "output_hru_args"
    )
  }

  # Compute objective metrics
  obj_number <- length(objectives)
  obj_values <- numeric(obj_number)
  for (i in seq_len(obj_number)) {
    obj_setting <- objectives[[i]]
    obj_values[i] <- compute_gof(
      obj_setting, observed_data, simulated_results, output_config
    )
  }

  # Restore original files from "Backup"
  if (!is.null(change_output)) {
    restore_swat_input_files(change_output)
  }

  return(obj_values)
}




# -------------------------------------------------------------------------
# compute_gof
# -------------------------------------------------------------------------

compute_gof <- function(obj_setting, observed_data, simulated_results, output_config) {

  calib_time_step <- obj_setting$calib_time_step
  if(!calib_time_step %in% c("daily", "monthly")) {
    stop('Calibration time step must be "daily" or "monthly".')
  }

  # Process simulated data and compute metrics for each element
  if (obj_setting$element == "rch") {

    obs_data <- observed_data[[calib_time_step]]$rch[[obj_setting$variable]] %>%
      dplyr::filter(target_id == obj_setting$target_id) %>%
      dplyr::rename(obs = value) %>%
      dplyr::select(Date, obs)

    sim_data <- simulated_results$rch %>%
      dplyr::filter(RCH == obj_setting$target_id) %>%
      dplyr::select(RCH, MON, !!rlang::sym(obj_setting$variable))

    if (output_config$rch$time_step == "daily" &&
        obj_setting$calib_time_step == "monthly") {
      sim_data <- aggregate_swat_output(
        swat_output = sim_data, agg_level = "month"
      )
    }

    sim_data <- sim_data %>%
      dplyr::rename(Date = MON, sim = !!rlang::sym(obj_setting$variable)) %>%
      dplyr::select(Date, sim)

    # Data for calibration period
    sim_data <- sim_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))
    obs_data <- obs_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))


    sim_obs_data <- dplyr::left_join(sim_data, obs_data, by = "Date")

    metric_value <- gof_metrics(
      obs = sim_obs_data$obs,
      sim = sim_obs_data$sim,
      metric = obj_setting$metric,
      minimize = TRUE
    )

    if (is.na(metric_value)) {
      stop("Objective function returned NA for an reach element. ",
           "Please verify the input data.")
    }

  } else if (obj_setting$element == "sub") {


    obs_data <- observed_data[[calib_time_step]]$sub[[obj_setting$variable]] %>%
      dplyr::rename(obs = value) %>%
      dplyr::select(Date, target_id, obs)

    sim_data <- simulated_results$sub %>%
      dplyr::select(SUB, MON, AREAkm2, !!rlang::sym(obj_setting$variable))

    if (!is.null(obj_setting$target_id)) {
      subbasins <- unique(obj_setting$target_id)

      obs_data <- obs_data %>%
        dplyr::filter(target_id %in% subbasins)

      sim_data <- sim_data %>%
        dplyr::filter(SUB %in% subbasins)
    } else {
      subbasins <- unique(sim_data$SUB)
    }

    subbasin_area <- tibble(SUB = subbasins) %>%
      dplyr::left_join(
        dplyr::select(sim_data, SUB, AREAkm2) %>% unique(), by = "SUB"
      ) %>%
      dplyr::rename(target_id = SUB)

    if(nrow(subbasin_area) != length(subbasins)) {
      stop("Reading AREAkm2 column from output.sub")
    }

    sim_data <- sim_data %>%
      dplyr::select(SUB, MON, !!rlang::sym(obj_setting$variable))

    if (output_config$sub$time_step == "daily" &&
        obj_setting$calib_time_step == "monthly") {
      sim_data <- aggregate_swat_output(
        swat_output = sim_data, agg_level = "month"
      )
    }

    sim_data <- sim_data %>%
      dplyr::rename(Date = MON, target_id = SUB,
                    sim = !!rlang::sym(obj_setting$variable)) %>%
      dplyr::select(Date, target_id, sim)

    # Verify target_id match between observed and simulated data
    missing_subbasins <- setdiff(
      as.numeric(unique(sim_data$target_id)),
      as.numeric(unique(obs_data$target_id))
    )

    if (length(missing_subbasins) > 0) {
      stop(
        "The following subbasins are present in the simulated data but not in ",
        "the observed data: ", paste(missing_subbasins, collapse = ", "),
        ". Please verify the observed data."
      )
    }

    # Data for calibration period
    sim_data <- sim_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))
    obs_data <- obs_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))

    sim_obs_data <- dplyr::left_join(
      sim_data, obs_data, by = c("Date", "target_id")
    )

    subbasin_metrics <- sim_obs_data %>%
      dplyr::group_by(target_id) %>%
      dplyr::summarise(
        gof = gof_metrics(
          obs = obs,
          sim = sim,
          metric = obj_setting$metric,
          minimize = TRUE
        ),
        .groups = "drop"
      )

    if (any(is.na(subbasin_metrics$gof))) {
      problematic_subbasins <- subbasin_metrics %>%
        dplyr::filter(is.na(gof)) %>%
        dplyr::pull(target_id)

      stop(
        "NA values found in subbasin metrics for the following subbasins: ",
        paste(problematic_subbasins, collapse = ", "),
        ". Verify input data."
      )
    }

    subbasin_metrics <- subbasin_metrics %>%
      dplyr::left_join(subbasin_area, by = "target_id")

    total_area <- sum(subbasin_metrics$AREAkm2, na.rm = TRUE)
    metric_value <- sum(subbasin_metrics$gof * subbasin_metrics$AREAkm2) /
      total_area

  } else if (obj_setting$element == "hru") {

    obs_data <- observed_data[[calib_time_step]]$hru[[obj_setting$variable]] %>%
      dplyr::filter(target_id == obj_setting$target_id) %>%
      dplyr::rename(obs = value) %>%
      dplyr::select(Date, obs)

    sim_data <- simulated_results$hru %>%
      dplyr::filter(GIS == obj_setting$target_id)

    if (output_config$hru$time_step == "daily" &&
        obj_setting$calib_time_step == "monthly") {
      sim_data <- aggregate_swat_output(
        swat_output = sim_data, agg_level = "month"
      )
    }

    sim_data <- sim_data %>%
      dplyr::rename(Date = MON, sim = !!rlang::sym(obj_setting$variable)) %>%
      dplyr::select(Date, sim)

    # Data for calibration period
    sim_data <- sim_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))
    obs_data <- obs_data %>%
      dplyr::filter(Date >= as.Date(obj_setting$calib_start_date) &
                      Date <= as.Date(obj_setting$calib_end_date))

    sim_obs_data <- dplyr::left_join(sim_data, obs_data, by = "Date")

    metric_value <- gof_metrics(
      obs = sim_obs_data$obs,
      sim = sim_obs_data$sim,
      metric = obj_setting$metric,
      minimize = TRUE
    )

    if (is.na(metric_value)) {
      stop("Objective function returned NA for an HRU element. ",
           "Please verify the input data.")
    }

  } else {
    stop("Unsupported data element type: ", obj_setting$element)
  }



  return(metric_value)
}
