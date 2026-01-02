# =========================================================================
# Multiobjective calibration — validators (internal)
# -------------------------------------------------------------------------
# Purpose:
#   Input validation helpers for the SWAT multiobjective workflow.
#   These functions are called by calibrate_swat() when using
#   calculate_objectives(), to fail fast with clear messages and to
#   keep per-iteration overhead low.
#
# Scope:
#   - validate_fn_args()
#   - validate_objectives()
#   - validate_observed_data()
#   - validate_output_config()
#   - validate_parameter_info()
#   - validate_cross_configurations()
#
# =========================================================================

# -------------------------------------------------------------------------
# validate_fn_args
# -------------------------------------------------------------------------
# Validate fn_args for calculate_objectives function
validate_fn_args <- function(fn_args) {
  # Ensure 'fn_args' is a named list
  if (!is.list(fn_args)) {
    stop("'fn_args' must be a named list.")
  }

  # Define required calibration data inputs in 'fn_args'
  required_inputs <- c("objectives", "observed_data", "output_config",
                       "parameter_info", "subbasins", "swat_exe_path")

  # Check for missing calibration data inputs
  missing_inputs <- setdiff(required_inputs, names(fn_args))
  if (length(missing_inputs) > 0) {
    stop("Missing required calibration data inputs in 'fn_args': ",
         paste(missing_inputs, collapse = ", "))
  }

  # Validate and correct components
  validate_objectives(fn_args$objectives)
  validate_observed_data(fn_args$observed_data)
  validate_output_config(fn_args$output_config)

  # Ensure 'parameter_info' is valid
  fn_args$parameter_info <- validate_parameter_info(fn_args$parameter_info)

  # Ensure 'subbasins' is either numeric or NULL
  if (!is.null(fn_args$subbasins) && !is.numeric(fn_args$subbasins)) {
    stop("'subbasins' must be either numeric or NULL.")
  }

  # Ensure SWAT executable exists
  if (!file.exists(fn_args$swat_exe_path)) {
    stop("SWAT executable not found at path: ", fn_args$swat_exe_path)
  }

  # Cross validation
  validate_cross_configurations(fn_args$objectives,
                                fn_args$observed_data,
                                fn_args$output_config)

  return(fn_args)  # Return validated and corrected 'fn_args'
}

# -------------------------------------------------------------------------
# validate_objectives
# -------------------------------------------------------------------------
validate_objectives <- function(objectives) {
  if (!is.list(objectives) || length(objectives) == 0) {
    stop("'objectives' must be a non-empty list.")
  }

  # Define valid values for 'element', time steps, and metrics
  valid_elements <- c("rch", "sub", "hru")
  valid_time_steps <- c("daily", "monthly")
  valid_metrics <- get_gof_metrics()

  # Required fields for each objective
  required_fields <- c(
    "element", "variable", "target_id", "metric", "calib_time_step",
    "calib_start_date", "calib_end_date"
  )

  # Iterate over each objective
  for (i in seq_along(objectives)) {
    obj <- objectives[[i]]

    # Check that each objective is a list
    if (!is.list(obj)) {
      stop("Each objective must be a list. Error in objective: ", i)
    }

    # Check for missing fields
    missing_fields <- setdiff(required_fields, names(obj))
    if (length(missing_fields) > 0) {
      stop(
        "Objective ", i, " is missing required fields: ",
        paste(missing_fields, collapse = ", ")
      )
    }

    # Check for extra fields
    extra_fields <- setdiff(names(obj), required_fields)
    if (length(extra_fields) > 0) {
      stop(
        "Objective ", i, " contains unrecognized fields: ",
        paste(extra_fields, collapse = ", ")
      )
    }

    # Validate 'element' field
    if (!is.character(obj$element) || length(obj$element) != 1) {
      stop(
        "'element' must be a single character string in objective ", i,
        ". Allowed elements: ", paste(valid_elements, collapse = ", ")
      )
    }

    if (!obj$element %in% valid_elements) {
      stop(
        "Invalid 'element' in objective ", i, ": ", obj$element,
        ". Allowed elements: ", paste(valid_elements, collapse = ", ")
      )
    }

    # Validate 'variable' field
    if (!is.character(obj$variable) || length(obj$variable) != 1 ||
        nchar(obj$variable) == 0) {
      stop("'variable' must be a single non-empty string in objective ", i)
    }

    # Validate that 'variable' is in the valid list for the selected 'element'
    element_vars <- get_swat_vars(obj$element)
    if (!obj$variable %in% element_vars) {
      stop(
        "Invalid 'variable' in objective ", i, ": ", obj$variable,
        ". \nAllowed variables for '", obj$element, "': ",
        paste(element_vars, collapse = ", ")
      )
    }

    # Validate 'target_id' based on 'element'
    if (obj$element == "rch" && (!is.numeric(obj$target_id) ||
                                 length(obj$target_id) != 1)) {
      stop("'target_id' must be a single numeric value for 'rch' in objective ",
           i)
    }

    if (obj$element == "sub" && (!is.numeric(obj$target_id) &&
                                 !is.null(obj$target_id))) {
      stop("'target_id' must be numeric or NULL for 'sub' in objective ", i)
    }

    if (obj$element == "hru" && (!is.character(obj$target_id) ||
                                 nchar(obj$target_id) == 0)) {
      stop("'target_id' must be a non-empty character for 'hru' in objective ",
           i)
    }

    # Validate 'metric' field
    if (!is.character(obj$metric) || length(obj$metric) != 1 ||
        nchar(obj$metric) == 0 || !obj$metric %in% valid_metrics) {
      stop(paste0("Invalid 'metric' in objective ", i,
                  ".\nSelect one of: ", paste(valid_metrics, collapse = ", ")))
    }

    # Validate 'calib_time_step'
    if (!obj$calib_time_step %in% valid_time_steps) {
      stop(
        "Invalid 'calib_time_step' in objective ", i, ": ", obj$calib_time_step,
        ". Allowed values: ", paste(valid_time_steps, collapse = ", ")
      )
    }

    # Validate 'calib_start_date' and 'calib_end_date'
    if (!is.character(obj$calib_start_date) ||
        length(obj$calib_start_date) != 1) {
      stop("'calib_start_date' must be a single string in objective ", i)
    }

    if (!is.character(obj$calib_end_date) ||
        length(obj$calib_end_date) != 1) {
      stop("'calib_end_date' must be a single string in objective ", i)
    }

    start_date <- suppressWarnings(as.Date(obj$calib_start_date,
                                           format="%Y-%m-%d"))
    end_date <- suppressWarnings(as.Date(obj$calib_end_date,
                                         format="%Y-%m-%d"))

    if (is.na(start_date)) {
      stop("Invalid 'calib_start_date' in objective ", i, ": ",
           obj$calib_start_date)
    }

    if (is.na(end_date)) {
      stop("Invalid 'calib_end_date' in objective ", i, ": ",
           obj$calib_end_date)
    }

    if (start_date > end_date) {
      stop(
        "'calib_start_date' cannot be after 'calib_end_date' in objective ", i
      )
    }
  }

  # Check for duplicate objectives
  objectives_df <- do.call(rbind, lapply(objectives, function(obj) {
    data.frame(lapply(obj, function(x) paste(x, collapse=",")),
               stringsAsFactors = FALSE)
  }))

  duplicates <- duplicated(objectives_df) | duplicated(objectives_df,
                                                       fromLast = TRUE)
  if (any(duplicates)) {
    dup_indices <- which(duplicates)
    stop(
      "The following objectives have the same configuration: Objective ",
      paste(dup_indices, collapse = " and "),
      ". Ensure each objective is unique."
    )
  }
}


# -------------------------------------------------------------------------
# validate_observed_data
# -------------------------------------------------------------------------
validate_observed_data <- function(observed_data) {
  valid_time_steps <- c("daily", "monthly")
  valid_elements <- c("rch", "sub", "hru")

  # Check if only valid time steps ('daily' or 'monthly') exist
  invalid_time_steps <- setdiff(names(observed_data), valid_time_steps)
  if (length(invalid_time_steps) > 0) {
    stop("Invalid time step(s) found in 'observed_data': ",
         paste(invalid_time_steps, collapse = ", "),
         ". Only 'daily' and 'monthly' are allowed.")
  }

  # Validate each time step
  for (time_step in valid_time_steps) {
    if (!is.null(observed_data[[time_step]])) {

      # Ensure only valid elements ('rch', 'sub', 'hru') exist
      time_step_elements <- names(observed_data[[time_step]])
      invalid_elements <- setdiff(time_step_elements, valid_elements)
      if (length(invalid_elements) > 0) {
        stop("Invalid element(s) found in '", time_step, "' time step: ",
             paste(invalid_elements, collapse = ", "),
             ". Only 'rch', 'sub', and 'hru' are allowed.")
      }

      if (length(intersect(time_step_elements, valid_elements)) == 0) {
        stop("Each time step ('daily' or 'monthly') must contain at least one ",
             "of the following elements: 'rch', 'sub', or 'hru'.")
      }

      # Validate each element within the time step
      for (element in time_step_elements) {
        if (element %in% valid_elements) {
          for (variable in names(observed_data[[time_step]][[element]])) {
            obs <- observed_data[[time_step]][[element]][[variable]]

            # Validate structure (should be a tibble with required columns)
            if (!inherits(obs, "tbl") ||
                !all(c("Date", "value", "target_id") %in% colnames(obs))) {
              stop("Each entry in 'observed_data' must be a tibble containing ",
                   "the columns 'Date', 'value', and 'target_id'.")
            }

            # Validate 'Date' column
            if (!all(lubridate::is.Date(as.Date(obs$Date)))) {
              stop("All dates in 'observed_data' must be valid dates.")
            }

            # Validate 'value' column (must be numeric)
            if (!is.numeric(obs$value)) {
              stop("The 'value' column in 'observed_data' must be numeric.")
            }

            # Validate 'target_id' column based on element type
            if (element %in% c("rch", "sub")) {
              if (!is.numeric(obs$target_id)) {
                stop("For elements 'rch' or 'sub', 'target_id' must be numeric.")
              }
            } else if (element == "hru") {
              if (!is.character(obs$target_id)) {
                stop("For element 'hru', 'target_id' must be of character type.")
              }
            }
          }
        }
      }
    }
  }
}

# -------------------------------------------------------------------------
# validate_output_config
# -------------------------------------------------------------------------
validate_output_config <- function(output_config) {
  valid_elements <- c("rch", "sub", "hru")
  required_fields <- c("file", "variable", "target_id", "time_step",
                       "output_start_date")
  valid_time_steps <- c("daily", "monthly")

  # Check that only valid elements exist
  invalid_elements <- setdiff(names(output_config), valid_elements)
  if (length(invalid_elements) > 0) {
    stop("Invalid element(s) found in 'output_config': ",
         paste(invalid_elements, collapse = ", "),
         ". Only 'rch', 'sub', and 'hru' are allowed.")
  }

  # Validate each element in output_config
  for (element in names(output_config)) {
    config <- output_config[[element]]

    # Check that all required fields are present
    missing_fields <- setdiff(required_fields, names(config))
    if (length(missing_fields) > 0) {
      stop("Missing required field(s) in '", element, "': ",
           paste(missing_fields, collapse = ", "), ".")
    }

    # Validate 'file' (must be a non-empty string)
    if (!is.character(config$file) || length(config$file) != 1 ||
        nchar(config$file) == 0) {
      stop("The 'file' field in '", element, "' must be a non-empty string.")
    }

    # Validate 'variable' (must be a non-empty character vector)
    if (!is.character(config$variable) || length(config$variable) == 0) {
      stop("The 'variable' field in '", element,
           "' must be a non-empty character vector.")
    }

    # Validate 'target_id' based on the element type
    if (element %in% c("rch", "sub")) {
      if (!is.null(config$target_id) && !is.numeric(config$target_id)) {
        stop("For 'rch' or 'sub', 'target_id' must be numeric or NULL.")
      }
    } else if (element == "hru") {
      if (!is.null(config$target_id) && !is.character(config$target_id)) {
        stop("For 'hru', 'target_id' must be character or NULL.")
      }
    }

    # Validate 'time_step' (must be 'daily' or 'monthly')
    if (!config$time_step %in% valid_time_steps) {
      stop("Invalid 'time_step' in '", element, "': ", config$time_step,
           ". Must be 'daily' or 'monthly'.")
    }

    # Validate 'output_start_date' (must be a valid date in YYYY-MM-DD format)
    date_check <- suppressWarnings(as.Date(config$output_start_date,
                                           format = "%Y-%m-%d"))
    if (is.na(date_check)) {
      stop("Invalid 'output_start_date' in '", element,
           "'. Must be a valid date (YYYY-MM-DD).")
    }
  }
}

# -------------------------------------------------------------------------
# validate_objectives
# -------------------------------------------------------------------------

validate_parameter_info <- function(parameter_info) {
  # Required and optional columns
  required_columns <- c("component", "parameter", "value", "method")
  optional_columns <- c("version", "plant_type", "min", "max")

  # Valid methods and versions
  valid_methods <- c("v", "r")
  valid_versions <- c("SWAT", "SWAT_T")

  # Check for missing required columns
  missing_columns <- setdiff(required_columns, colnames(parameter_info))
  if (length(missing_columns) > 0) {
    stop("Missing required column(s): ",
         paste(missing_columns, collapse = ", "))
  }

  # Check for unexpected columns
  unexpected_columns <- setdiff(colnames(parameter_info),
                                c(required_columns, optional_columns))
  if (length(unexpected_columns) > 0) {
    message("Ignoring extra columns in parameter_info: ",
            paste(unexpected_columns, collapse = ", ")
    )
  }

  # Validate 'component' values are non-empty strings
  if (!is.character(parameter_info$component) ||
      any(nchar(parameter_info$component) == 0)) {
    stop("All 'component' values must be non-empty character strings.")
  }

  # Validate 'parameter' values are non-empty strings
  if (!is.character(parameter_info$parameter) ||
      any(nchar(parameter_info$parameter) == 0)) {
    stop("All 'parameter' values must be non-empty character strings.")
  }

  # Validate 'value' column: numeric or all NA
  if (!all(is.na(parameter_info$value)) && !is.numeric(parameter_info$value)) {
    stop("The 'value' column must contain numeric values or all NA.")
  }

  # Validate 'method' values are valid
  if (!all(parameter_info$method %in% valid_methods)) {
    stop("Invalid 'method' values found. Must be one of: ",
         paste(valid_methods, collapse = ", "))
  }

  # Check and handle 'version' column
  if (!"version" %in% colnames(parameter_info)) {
    parameter_info$version <- "SWAT"  # Default value
  } else if (all(is.na(parameter_info$version))) {
    parameter_info$version <- rep("SWAT", nrow(parameter_info))
  } else if (!all(parameter_info$version %in% valid_versions, na.rm = TRUE)) {
    stop("Invalid 'version' values found. Must be 'SWAT' or 'SWAT_T'.")
  }

  # Ensure 'plant_type' column exists (with NAs if missing)
  if (!"plant_type" %in% colnames(parameter_info)) {
    if (any(parameter_info$component == "plant.dat")) {
      stop("When 'component' is 'plant.dat', the 'plant_type' column must exist.")
    } else {
      parameter_info$plant_type <- NA
    }
  }

  if (any(parameter_info$component == "plant.dat" &
          is.na(parameter_info$plant_type))) {
    stop("When 'component' is 'plant.dat', 'plant_type' must be a ",
         "non-NA character value.")
  }

  return(parameter_info)
}

# -------------------------------------------------------------------------
# validate_cross_configurations
# -------------------------------------------------------------------------
validate_cross_configurations <- function(objectives, observed_data,
                                          output_config) {
  for (i in seq_along(objectives)) {
    obj <- objectives[[i]]
    obj_config <- obj[setdiff(names(obj), "metric")]

    # Extract parameters from the objective configuration
    time_step <- obj$calib_time_step
    element <- obj$element
    variable <- obj$variable
    target_id <- obj$target_id
    start_date <- as.Date(obj$calib_start_date)
    end_date <- as.Date(obj$calib_end_date)

    # Validate 'time_step' presence in observed_data
    if (!time_step %in% names(observed_data)) {
      stop(paste0("For objective ", i, ", observed_data must contain ",
                  time_step, " data."))
    }

    # Validate 'element' presence in observed_data for the given time_step
    if (!element %in% names(observed_data[[time_step]])) {
      stop(paste0("For objective ", i, ", element ", element,
                  " is missing in observed_data for time step ", time_step, "."))
    }

    # Validate 'variable' presence in observed_data for the given element
    # and time_step
    if (!variable %in% names(observed_data[[time_step]][[element]])) {
      stop(paste0("For objective ", i, ", variable ", variable,
                  " is missing in observed_data for element ", element,
                  " and time step ", time_step, "."))
    }

    obs_data <- observed_data[[time_step]][[element]][[variable]]
    observed_target_ids <- unique(obs_data$target_id)

    # Validate 'target_id' presence in observed_data
    if (element %in% c("rch", "hru")) {
      # 'target_id' must be a single value and must exist in observed data
      if (!is.null(target_id) && !target_id %in% observed_target_ids) {
        stop(paste0("For objective ", i, ", target id ", target_id,
                    " is missing in observed_data for element ", element, "."))
      }
    } else if (element == "sub") {
      # 'target_id' can be NULL (all subbasins) or a numeric vector
      if (is.null(target_id)) {
        warning(paste0("For objective ", i, ", target id is NULL for element sub. ",
                       "All subbasins will be analyzed. Ensure observed_data ",
                       "contains all necessary subbasin data."))
      } else {
        # Identify missing target_ids
        missing_ids <- target_id[!target_id %in% observed_target_ids]

        if (length(missing_ids) > 0) {
          stop(paste0("For objective ", i, ", the following target ids are missing ",
                      "in observed_data for element sub: ", toString(missing_ids), "."))
        }
      }
    }

    # Ensure the calibration period contains at least one observed data point
    obs_dates_by_target <- split(obs_data, obs_data$target_id)

    # For 'sub', if target_id is NULL, consider all available target_ids
    if (element == "sub" && is.null(target_id)) {
      target_ids_to_check <- unique(obs_data$target_id)
    } else {
      target_ids_to_check <- target_id
    }

    # Track target_ids with no observed data in the calibration period
    no_data_ids <- c()

    for (tid in target_ids_to_check) {
      tid_str <- as.character(tid)  # Convert to character for comparison

      if (tid_str %in% names(obs_dates_by_target)) {
        obs_dates <- as.Date(obs_dates_by_target[[tid_str]]$Date)

        # Check if there's at least one observed date within the calibration
        # range
        if (!any(obs_dates >= start_date & obs_dates <= end_date)) {
          no_data_ids <- c(no_data_ids, tid)
        }
      } else {
        no_data_ids <- c(no_data_ids, tid)
      }
    }

    # Stop execution if there are target_ids with no observed data in the
    # calibration period
    if (length(no_data_ids) > 0) {
      stop(paste0("For objective ", i, ", no observed data is available in the ",
                  "calibration period for target ids: ", toString(no_data_ids), "."))
    }

    # Validate 'element' presence in output_config
    if (!element %in% names(output_config)) {
      stop(paste0("For objective ", i, ", element ", element, " is missing in ",
                  "output_config."))
    }

    output_element <- output_config[[element]]

    # Validate 'variable' presence in output_config for the given element
    if (!variable %in% output_element$variable) {
      stop(paste0("For objective ", i, ", variable ", variable,
                  " is missing in output_config for element ", element, "."))
    }

    output_target_ids <- output_element$target_id

    # Validate 'target_id' presence in output_config for different elements
    if (element == "rch" || element == "hru") {
      # Check if output_target_ids is NULL, then verify target_id presence
      if (!is.null(output_target_ids) && !target_id %in% output_target_ids) {
        stop(paste0("For objective ", i, ", target id ", target_id,
                    " is missing in output_config$target_id for element ",
                    element, "."))
      }
    } else if (element == "sub") {
      # For 'sub', if target_id is numeric, check if output_target_ids is NULL
      # or contains the target_id
      if (is.numeric(target_id)) {
        if (!is.null(output_target_ids)) {
          missing_target_ids <- target_id[!target_id %in% output_target_ids]
          if (length(missing_target_ids) > 0) {
            stop(paste0("For objective ", i, ", the following target_id(s) are ",
                        "missing in output_config$target_id for element sub: ",
                        paste(missing_target_ids, collapse = ", "), "."))
          }
        }
      } else if (is.null(target_id)) {
        # If target_id is NULL, output_target_ids should also be NULL
        if (!is.null(output_target_ids)) {
          stop(paste0("For objective ", i, ", target_id is NULL, so output_config$",
                      "target_id should also be NULL for element sub."))
        }
      }
    }
  }
}

