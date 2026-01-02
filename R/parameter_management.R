# =============================================================================
# Family: Parameter management
# SWAT parameter querying and editing (get_swat_parameters, change_parameter,
# change_multiple_parameters, split_swat_parameters)
# =============================================================================

# -------------------------------------------------------------------------
# change_parameter
# -------------------------------------------------------------------------
#' @title Change a single SWAT model parameter
#'
#' @description
#' Modifies the value of a single SWAT model parameter in one or more input
#' files. Typically used for calibration or sensitivity analysis at the subbasin
#' or global level.
#' This function is used by \link{change_multiple_parameters} and supports
#' \link{calibrate_swat} for multi-objective calibration workflows.
#'
#' @param subbasins numeric. Vector of subbasin IDs where the parameter change
#'   should be applied. If `NULL` (default), changes are applied globally.
#' @param component character. SWAT input component (e.g., `".gw"`, `".mgt"`,
#'   `"plant.dat"`, `"basins.bsn"`) for the corresponding parameter to modify.
#'   See \link{get_swat_parameters}.
#' @param parameter character. Name of the SWAT parameter to modify within the
#'   defined `component`.
#' @param value numeric. New value or adjustment factor for the parameter.
#' @param method character. Method for applying the change:
#'   \itemize{
#'     \item `"v"`: Replace the value.
#'     \item `"r"`: Relative change (e.g., `0.1` increases the current value by 10%).
#'   }
#' @param version character. SWAT version (`"SWAT"` or `"SWAT_T"`). Default is `"SWAT"`.
#' @param plant_type character. Type of plant to modify. Required when
#'   `component = "plant.dat"`.
#'
#' @details
#' For \code{component = ".wgn"} and \code{".pcp"}, only \code{method = "r"} is supported.
#' For \code{component = "plant.dat"}, \code{plant_type} is required.
#' The function expects to run inside the SWAT project's \code{TxtInOut} folder.
#'
#' @return
#' A list (class `"changed_parameter"`) summarizing the applied changes,
#' including all input arguments and the modified files.
#'
#' @family Parameter management
#'
#' @seealso
#' \link{calibrate_swat}
#'
#' @examples
#' \donttest{
#' tmpdir <- tempdir()
#'
#' # Get example SWAT project
#' get_swat_example(tmpdir)
#' setwd(file.path(tmpdir, "TxtInOut"))
#'
#' # Modify GW_DELAY in .gw globally
#' change_parameter(
#'   component = ".gw",
#'   parameter = "GW_DELAY",
#'   value = 30,
#'   method = "v"
#' )
#'
#' # Apply a relative change to CN2 in .mgt for subbasins 1 and 2
#' change_parameter(
#'   subbasins = c(1, 2),
#'   component = ".mgt",
#'   parameter = "CN2",
#'   value = 0.1,
#'   method = "r"
#' )
#' }
#'
#' @export
#' @importFrom readr read_lines write_lines
#' @importFrom dplyr select mutate bind_cols
#' @importFrom stringr str_which str_extract_all

change_parameter <- function(subbasins = NULL, component, parameter, value, method,
                             version = "SWAT",  plant_type = NULL) {

  # Retrieve SWAT parameters for the selected version
  swat_params <- get_swat_parameters(version = version)

  # Validate the component argument
  if (missing(component)) stop("'component' must be specified.")
  valid_components <- unique(swat_params$component)
  if (!component %in% valid_components) {
    stop(paste("'component' must be one of",
               paste(valid_components, collapse = ", ")))
  }

  # Validate parameter and method arguments
  valid_parameters <- swat_params$parameter[swat_params$component == component]
  if (!parameter %in% valid_parameters) {
    stop(paste("'parameter' for component '",
               component, "' must be one of ",
               paste(valid_parameters, collapse = ", ")))
  }

  if (missing(method)) stop("'method' must be specified as 'r' or 'v'.")
  if (!method %in% c("r", "v")) stop("'method' must be either 'r' (relative) or 'v' (replace).")

  if (!is.numeric(value)) stop("'value' must be numeric.")
  if (method == "r" && (value < -1 || value > 1)) {
    stop("'value' must be between -1 and 1 for method 'r'.")
  }

  # Specific checks for plant_type requirement
  if (component == "plant.dat" && is.null(plant_type)) {
    stop("'plant_type' is required for 'plant.dat' component.")
  }


  # Validate parameter in swat_params
  param_row <- swat_params[swat_params$component == component &
                             swat_params$parameter == parameter, ]
  if (nrow(param_row) == 0) {
    stop(paste0("The parameter '", parameter,
                "' was not found in the component '", component, "'."))
  }

  # Get target files
  if(component == "basins.bsn"){
    files <- "basins.bsn"
  } else if(component == "plant.dat"){
    files <- "plant.dat"
  }else if(component == ".pcp"){
    files <- list.files(pattern = "^pcp[0-9]*\\.pcp$")
  } else{
    files <- dir(pattern = component)[grepl("^[0-9]{5}", dir(pattern = component))]
    subbasins_id <- as.numeric(substr(files, 1, 5))

    # Check if subbasins is provided and filter files accordingly
    if (!is.null(subbasins)) {
      if (!is.numeric(subbasins) || !all(subbasins %in% subbasins_id)) {
        stop("'subbasins' must be a numeric vector containing valid subbasin IDs.")
      }
      files <- files[subbasins_id %in% subbasins]
    }
  }

  if (length(files) == 0) {
    stop("No SWAT input files were found in the current working directory. Ensure that the required SWAT files are present.")
  }

  # Change parameter values
  if (component %in% c(".rte", "basins.bsn", ".sub", ".swq", ".pnd", ".hru",
                       ".mgt", ".gw", ".sdr", ".sep")) {
    # Read the entire content of all the files into memory
    all_texts <- lapply(files, readr::read_lines)

    # Modify the content in memory
    for (i in seq_along(files)) {
      text <- all_texts[[i]]

      # Check if the specified line exists in the file
      if (param_row$line > length(text)) {
        stop("Line ", param_row$line, " does not exist in file '", files[i], "'.")
      }

      # Get the current value from the specified line
      current_value <- as.numeric(
        substr(text[param_row$line],
               start = param_row$start,
               stop = param_row$stop)
      )

      # Calculate the new value based on the method
      if (method == "r") {
        new_value <- current_value * (1 + value)
      } else {
        new_value <- value
      }

      # Update the line in the text with the new value
      text[param_row$line] <- paste0(
        sprintf(param_row$format, new_value),
        sprintf("%4s", ""),  # Add spaces if necessary
        param_row$description
      )

      # Store the modified text back in the list of all texts
      all_texts[[i]] <- text
    }

    # Write all modified files back at once
    for (i in seq_along(files)) {
      readr::write_lines(all_texts[[i]], files[i])
    }

  } else if (component %in% c(".sol", ".chm")) {

    # Read all the content of the files into memory
    all_texts <- lapply(files, readr::read_lines)

    # Loop through each file for this component
    for (i in seq_along(files)) {
      text <- all_texts[[i]]

      # Ensure the target line exists in the file
      if (param_row$line > length(text)) {
        stop("The specified line does not exist in the file: ", files[i])
      }

      # Get the current value from the specified line
      current_value <- as.numeric(trimws(str_extract_all(
        strsplit(text[param_row$line], ":")[[1]][2], ".{1,12}")[[1]]
      ))

      # Calculate new_value based on the method
      if (method == "r") {
        new_value <- current_value * (1 + value)
      } else {
        new_value <- rep(value, length(current_value))
      }

      # Update the line in text with the new value
      text[param_row$line] <- paste0(
        param_row$description,
        paste(sprintf(param_row$format, new_value), collapse = "")
      )

      # Store the modified text back in the list of all texts
      all_texts[[i]] <- text
    }

    # Write all modified files back at once
    for (i in seq_along(files)) {
      readr::write_lines(all_texts[[i]], files[i])
    }

  } else if (component == "plant.dat") {

    text <- readr::read_lines(files)

    pos_crop <- stringr::str_which(text, plant_type)

    if (length(pos_crop) == 0) {
      stop(paste0("Plant type '", plant_type, "' not found in 'plant.dat' file."))
    }

    if (length(pos_crop) > 1) {
      stop(paste0("Multiple entries for plant type '", plant_type,
                  "' found in 'plant.dat'. Please ensure only one entry exists."))
    }

    # Get the current value from the specified line
    pos_param <- pos_crop + param_row$line

    plant_param_subset <- swat_params[swat_params$component == component &
                                        swat_params$line == param_row$line, ]

    param_vector <- as.numeric(unlist(strsplit(trimws(text[pos_param]), "\\s+")))

    param_target_loc <- which(plant_param_subset$parameter %in% parameter)

    current_value <- param_vector[param_target_loc]

    # Calculate new_value based on the method
    if (method == "r") {
      new_value <- current_value * (1 + value)
    } else {
      new_value <- rep(value, length(current_value))
    }

    # Update parameter value
    param_vector[param_target_loc] <- new_value

    # Format and update the line in text
    text[pos_param] <- paste(sprintf(plant_param_subset$format, param_vector),
                             collapse = "")

    # Write the modified line back to the file
    readr::write_lines(text, files)

  } else if (component == ".wgn") {
    if (method != "r"){
      stop("method must be 'r' for '.wgn' ")
    }

    # Read all the content of the files into memory
    all_texts <- lapply(files, readr::read_lines)

    # Loop through each file for this component
    for (i in seq_along(files)) {
      text <- all_texts[[i]]

      # Ensure the target line exists in the file
      if (param_row$line > length(text)) {
        stop("The specified line does not exist in the file: ", files[i])
      }

      # Get the current value from the specified line
      current_value <- as.numeric(
        unlist(strsplit(trimws(text[param_row$line]), "\\s+"))
      )

      # Calculate new_value based on the method
      new_value <- current_value * (1 + value)

      # Update the line in text with the new value
      text[param_row$line] <- paste(sprintf("%6.2f", new_value),
                                    sep="", collapse="")

      # Store the modified text back in the list of all texts
      all_texts[[i]] <- text
    }

    # Write all modified files back at once
    for (i in seq_along(files)) {
      readr::write_lines(all_texts[[i]], files[i])
    }

  } else if (component == ".pcp") {

    #Get IRGAGE used for each subbasin
    file_sub <- dir(pattern = ".sub")[grepl("^[0-9]{5}", dir(pattern = ".sub"))]
    subbasins_id <- as.numeric(substr(file_sub, 1 , 5))

    #Filter files for target subbasins
    if (!is.null(subbasins)) {
      if (!is.numeric(subbasins) || !all(subbasins %in% subbasins_id)) {
        stop("'subbasins' must be a numeric vector containing valid subbasin IDs.")
      }
      file_sub <- file_sub[subbasins_id %in% subbasins] # Select files for target subbasins
    }

    gauge_ID_swat <- lapply(file_sub, function(x){
      sub <- readLines(x)
      IRGAGE <- as.numeric(substr(sub[7], 1 , 16))

    }) %>% unlist()

    #read swat pcp data
    read_pcp_out <- read_pcp(files)

    pcp_data <- read_pcp_out$pcp %>%
      dplyr::select(-year, -yday)


    pcp_date <- read_pcp_out$pcp %>%
      dplyr::select(year, yday) %>%
      dplyr::mutate(Date = as.Date(yday, origin = paste0(year-1,"-12-31"))) %>%
      dplyr::select(Date)

    if (method == "r"){
      pcp_data[, gauge_ID_swat] <- pcp_data[, gauge_ID_swat] * (1 + value)

    } else {
      stop("method must be 'r' for '.pcp' ")
    }

    colnames(pcp_data) <- as.character(read_pcp_out$gauges$ID_swat)
    pcp_data <- dplyr::bind_cols(pcp_date, pcp_data) %>%
      as.data.frame()

    #write pcp files
    pcp2swat(pcp_data, files)

  }

  message("Parameter change applied successfully.")

  outputs <- list(
    subbasins = subbasins,
    component = component,
    parameter = parameter,
    value = value,
    method = method,
    files = files
  )

  class(outputs) <- append(class(outputs), "changed_parameter")

  return(outputs)
}


# -------------------------------------------------------------------------
# change_multiple_parameters
# -------------------------------------------------------------------------
#' @title Change multiple SWAT model parameters
#'
#' @description
#' Applies several SWAT parameter updates by iterating over rows of
#' `parameter_info` and internally calling \link{change_parameter}. Commonly
#' used in calibration or sensitivity workflows, and integrates with
#' \link{run_swat} and \link{calibrate_swat}.
#'
#' @param parameter_info data.frame or tibble. Each row defines one parameter
#'   update and must include: `component`, `parameter`, `value`, `method`.
#'   Optional columns: `version` (defaults to "SWAT") and `plant_type`
#'   (required when `component = "plant.dat"`).
#' @param subbasins numeric. Vector of subbasin IDs where the changes should be
#'   applied. If `NULL` (default), changes are applied globally.
#'
#' @return A list with one element per row of `parameter_info`. Each element is
#'   the value returned by \link{change_parameter}.
#'
#' @family Parameter management
#' @seealso \link{run_swat}, \link{calibrate_swat}
#'
#' @examples
#' \donttest{
#' library(tibble)
#'
#' tmpdir <- tempdir()
#' get_swat_example(tmpdir)
#' setwd(file.path(tmpdir, "TxtInOut"))
#'
#' # Define multiple parameter changes
#' parameter_info <- tibble(
#'   component = c(".gw", ".mgt"),
#'   parameter = c("GW_DELAY", "CN2"),
#'   value     = c(30, 0.10),
#'   method    = c("v", "r")
#' )
#'
#' # Apply changes to subbasins 1 and 2
#' results <- change_multiple_parameters(
#'   parameter_info, subbasins = c(1, 2)
#' )
#'
#' # Apply changes globally (all subbasins)
#' results_global <- change_multiple_parameters(parameter_info)
#' }
#'
#' @export

change_multiple_parameters <- function(parameter_info, subbasins = NULL) {
  # Check if 'parameter_info' is a data frame or tibble
  if (!inherits(parameter_info, c("data.frame", "tbl_df"))) {
    stop("'parameter_info' must be a data frame or tibble.")
  }

  # Ensure 'parameter_info' contains required columns
  required_columns <- c("component", "parameter", "value", "method")
  missing_columns <- setdiff(required_columns, names(parameter_info))
  if (length(missing_columns) > 0) {
    stop("Missing required columns in 'parameter_info': ",
         paste(missing_columns, collapse = ", "))
  }

  # Add default values for optional columns
  if (!"version" %in% names(parameter_info)) parameter_info$version <- "SWAT"
  if (!"plant_type" %in% names(parameter_info)) parameter_info$plant_type <- NULL

  # Validate 'subbasins' if provided
  if (!is.null(subbasins)) {
    if (!is.numeric(subbasins)) {
      stop("'subbasins' must be a numeric vector of subbasin IDs.")
    }
  }

  # Apply parameter changes
  results <- lapply(seq_len(nrow(parameter_info)), function(i) {
    change_parameter(
      subbasins = subbasins,
      component = parameter_info$component[i],
      parameter = parameter_info$parameter[i],
      value = parameter_info$value[i],
      method = parameter_info$method[i],
      version = parameter_info$version[i],
      plant_type = parameter_info$plant_type[i]
    )
  })

  return(results)
}


# -------------------------------------------------------------------------
# get_swat_parameters
# -------------------------------------------------------------------------

#' @title Retrieve SWAT model parameters
#'
#' @description
#' Returns the set of SWAT parameters available for calibration or sensitivity
#' analysis for a given SWAT version. The table helps define valid
#' `component`/`parameter` pairs for \link{change_parameter} and
#' \link{change_multiple_parameters}.
#'
#' @param version character. SWAT version ("SWAT" or "SWAT_T"). Default:
#'   "SWAT".
#'
#' @return
#' A tibble with:
#' \itemize{
#'   \item `component`: SWAT input component (e.g., ".gw", ".mgt").
#'   \item `parameter`: Parameter name.
#'   \item `line`: Line index in the file.
#'   \item `start`: Start position in the line.
#'   \item `stop`: End position in the line.
#'   \item `format`: Print format used when writing.
#'   \item `description`: Short description.
#' }
#'
#' @family Parameter management
#'
#' @examples
#' # Parameters for default SWAT
#' p <- get_swat_parameters()
#'
#' # Parameters for SWAT_T
#' pt <- get_swat_parameters(version = "SWAT_T")
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @export

get_swat_parameters <- function(version = "SWAT") {

  # Ensure the version is valid
  version <- match.arg(version, c("SWAT", "SWAT_T"))

  #Valid parameters
  swat_parameters <- list()


  swat_parameters[["plant.dat"]] <-
    tibble::tibble(
      parameter = c(
        #"ICNUM", "CPNM", "IDC"
        # First group
        "BIO_E", "HVSTI", "BLAI", "FRGRW1", "LAIMX1", "FRGRW2", "LAIMX2", "DLAI", "CHTMX", "RDMX",
        # Second group
        "T_OPT", "T_BASE", "CNYLD", "CPYLD", "PLTNFR_1", "PLTNFR_2", "PLTNFR_3", "PLTPFR_1", "PLTPFR_2", "PLTPFR_3",
        # Third group
        "WSYF", "USLE_C", "GSI", "VPDFR", "FRGMAX", "WAVP", "CO2HI", "BIOEHI", "RSDCO_PL", "ALAI_MIN",
        # Fourth group
        "BIO_LEAF", "MAT_YRS", "BMX_TREES", "EXT_COEF", "BMDIEOFF"
      ),
      line = c(
        # Line numbers for the first group
        rep(1, 10),
        # Line numbers for the second group
        rep(2, 10),
        # Line numbers for the third group
        rep(3, 10),
        # Line numbers for the fourth group
        rep(4, 5)
      ),
      start = rep(NA, 35),
      stop = rep(NA, 35),
      #format = rep(NA, 35),
      format = c(
        rep("%7.2f", 2), rep("%8.2f", 1), rep("%7.2f", 5), rep("%8.2f", 1), rep("%7.2f", 1),#line 1
        rep("%7.2f", 1), rep("%8.2f", 1), rep("%9.4f", 8),#line 2
        rep("%7.3f", 1), rep("%9.4f", 2), rep("%7.2f", 1), rep("%8.3f", 1), rep("%8.2f", 1), rep("%10.2f", 1), rep("%9.2f", 1), rep("%9.4f", 1), rep("%8.3f", 1),#line 3
        rep("%7.3f", 1), rep("%6.0f", 1), rep("%8.2f", 1), rep("%8.3f", 2)#line 4
      ),

      description = c(
        # Descriptions for the first group
        "Radiation-use efficiency (kg/ha)/(MJ/m\u00B2)",
        "Harvest index for optimal growing conditions",
        "Maximum potential leaf area index (m\u00B2/m\u00B2)",
        "Fraction of PHU corresponding to the first point on the optimal leaf area development curve",
        "Fraction of BLAI corresponding to the first point on the optimal leaf area development curve",
        "Fraction of PHU corresponding to the second point on the optimal leaf area development curve",
        "Fraction of BLAI corresponding to the second point on the optimal leaf area development curve",
        "Fraction of total PHU when leaf area begins to decline",
        "Maximum canopy height (m)",
        "Maximum root depth (m)",
        # Descriptions for the second group
        "Optimal temperature for plant growth (\u00B0C)",
        "Base temperature for plant growth (\u00B0C)",
        "Normal fraction of nitrogen in yield (kg N/ha yield)",
        "Normal fraction of phosphorus in yield (kg P/ha yield)",
        "Normal fraction of nitrogen uptake in plant biomass at emergence (kg N/kg biomass)",
        "Normal fraction of nitrogen uptake  in plant biomass at 50% maturity (kg N/kg biomass)",
        "Normal fraction of nitrogen uptake  in plant biomass at maturity (kg N/kg biomass)",
        "Normal fraction of phosphorus uptake in plant biomass at emergence (kg P/kg biomass)",
        "Normal fraction of phosphorus uptake in plant biomass at 50% maturity (kg P/kg biomass)",
        "Normal fraction of phosphorus uptake in plant biomass at maturity (kg P/kg biomass)",
        # Descriptions for the third group
        "Lower limit of harvest index",
        "Minimum value of USLE C factor for water erosion applicable to the land cover",
        "Maximum stomatal conductance at high solar radiation and low vapor pressure deficit (m*s-1)",
        "Vapor pressure deficit (kPa) corresponding to the second point on the stomatal conductance curve",
        "Fraction of maximum stomatal conductance corresponding to the second point on the stomatal conductance curve",
        "Rate of decline in radiation use efficiency per unit increase in vapor pressure deficit",
        "Elevated CO2 atmospheric concentration corresponding to the second point on the radiation use efficiency curve",
        "Biomass-energy ratio corresponding to the second point on the radiation use efficiency curve",
        "Plant residue descomposition coefficient",
        "Minimum leaf area index for plant during dormant period (m\u00B2/m\u00B2)",
        # Descriptions for the fourth group
        "Fraction of tree biomass accumulated each year that is converted to residue during dormancy",
        "Number of years required for tree species to reach full development (years)",
        "Maximum biomass for a forest (metric tons/ha)",
        "Light extinction coefficient",
        "Biomass die-off fraction"
      )
    )


  swat_parameters[[".rte"]] <- tibble::tibble(
    parameter = c("CHW2", "CHD", "CH_S2", "CH_L2", "CH_N2", "CH_K2",
                  "CH_COV1", "CH_COV2", "CH_WDR", "ALPHA_BNK",
                  "ICANAL", "CH_ONCO", "CH_OPCO", "CH_SIDE",
                  "CH_BNK_BD", "CH_BED_BD", "CH_BNK_KD", "CH_BED_KD",
                  "CH_BNK_D50", "CH_BED_D50", "CH_BNK_TC", "CH_BED_TC",
                  "PRF", "SPCON", "SPEXP", "FP_N",
                  "FP_W_F", "SALT_DEL"),
    line = c(2, 3, 4, 5, 6, 7,
             8, 9, 10, 11,
             12, 13, 14, 15,
             16, 17, 18, 19,
             20, 21, 22, 23,
             26, 27, 28,
             29, 30, 32),
    start = rep(1, 28),
    stop = rep(14, 28),
    format = c(rep("%14.3f", 2), "%14.5f", rep("%14.3f", 7), rep("%14.2f", 13),
               "%14.4f", "%14.2f", rep("%14.3f", 3)),
    description = c("| CHW2 : Main channel width [m]",
                    "| CHD : Main channel depth [m]",
                    "| CH_S2 : Main channel slope [m/m]",
                    "| CH_L2 : Main channel length [km]",
                    "| CH_N2 : Manning's n value for main channel",
                    "| CH_K2 : Effective hydraulic conductivity [mm/hr]",
                    "| CH_COV1 : Channel erodibility factor",
                    "| CH_COV2 : Channel cover factor",
                    "| CH_WDR : Channel width:depth ratio [m/m]",
                    "| ALPHA_BNK : Baseflow alpha factor for bank storage [days]",
                    "| ICANAL : Code for irrigation canal",
                    "| CH_ONCO : Organic nitrogen concentration in the channel [ppm]",
                    "| CH_OPCO : Organic phosphorus concentration in the channel [ppm]",
                    "| CH_SIDE : Change in horizontal distance per unit vertical distance",
                    "| CH_BNK_BD : Bulk density of channel bank sediment (g/cc)",
                    "| CH_BED_BD : Bulk density of channel bed sediment (g/cc)",
                    "| CH_BNK_KD : Erodibility of channel bank sediment by jet test (cm3/N-s)",
                    "| CH_BED_KD : Erodibility of channel bed sediment by jet test (cm3/N-s)",
                    "| CH_BNK_D50 : D50 Median particle size diameter of channel bank sediment (\u00B5m)",
                    "| CH_BED_D50 : D50 Median particle size diameter of channel bed sediment (\u00B5m)",
                    "| CH_BNK_TC : Critical shear stress of channel bank (N/m2)",
                    "| CH_BED_TC : Critical shear stress of channel bed (N/m2)",
                    "| PRF : Peak rate adjustment factor for sediment routing in the main channel",
                    "| SPCON : Linear parameter for calculating the maximum amount of sediment that can be reentrained during channel sediment routing",
                    "| SPEXP : Exponent parameter for calculating sediment reentrained in channel sediment routing.",
                    "| FP_N : Manning's 'n' value for the flood plain",
                    "| FP_W_F : Flood plain average width / main channel average width",
                    "| SALT_DEL : Salt delivery ratio in reach")
  )


  swat_parameters[["basins.bsn"]] <- tibble::tibble(
    parameter = c("SFTMP", "SMTMP", "SMFMX", "SMFMN", "TIMP",
                  "SNOCOVMX", "SNO50COV", "ESCO", "EPCO", "EVLAI",
                  "FFCB", "SURLAG", "ADJ_PKR", "PRF_BSN", "SPCON",
                  "SPEXP", "RCN", "CMN", "N_UPDIS", "P_UPDIS",
                  "NPERCO", "PPERCO", "PHOSKD", "PSP", "RSDCO",
                  "PERCOP", "WDPQ", "WGPQ", "WDLPQ", "WGLPQ",
                  "WDPS", "WGPS", "WDLPS", "WGLPS", "BACTKDQ",
                  "THBACT", "WOF_P", "WOF_LP", "WDPF", "WGPF",
                  "WDLPF", "WGLPF", "MSK_CO1", "MSK_CO2", "MSK_X",
                  "TRNSRCH", "EVRCH", "CNCOEF", "CDN", "SDNCO",
                  "BACT_SWF", "BACTMX", "BACTMINLP", "BACTMINP",
                  "WDLPRCH", "WDPRCH", "WDLPRES", "WDPRES", "TB_ADJ",
                  "DEPIMP_BSN", "DDRAIN_BSN", "TDRAIN_BSN", "GDRAIN_BSN",
                  "CN_FROZ", "DORM_HR", "SMXCO", "FIXCO", "NFIXMX",
                  "ANION_EXCL_BSN", "CH_ONCO_BSN", "CH_OPCO_BSN",
                  "HLIFE_NGW_BSN", "RCN_SUB_BSN", "BC1_BSN", "BC2_BSN",
                  "BC3_BSN", "BC4_BSN", "DECR_MIN", "ICFAC", "RSD_COVCO",
                  "VCRIT", "RES_STLR_CO", "BFLO_DIST", "UHALPHA",
                  "EROS_SPL", "RILL_MULT", "EROS_EXPO", "SUBD_CHSED",
                  "C_FACTOR", "CH_D50", "SIG_G", "RE_BSN", "SDRAIN_BSN",
                  "DRAIN_CO_BSN", "PC_BSN", "LATKSATF_BSN", "IABSTR",
                  "R2ADJ_BSN", "SSTMAXD_BSN", "CO2_X2", "CO2_X",
                  "SFSEDMEAN", "SFSEDSTDEV"),
    line = c(4, 5, 6, 7, 8,
             9, 10, 13, 14, 15,
             16, 20, 21, 22, 23,
             24, 26, 27, 28, 29,
             30, 31, 32, 33, 34,
             36, 40, 41, 42, 43,
             44, 45, 46, 47, 48,
             49, 50, 51, 52, 53,
             54, 55, 59, 60, 61,
             65, 66, 69, 70, 71,
             72, 73, 74, 75, 76,
             77, 78, 79, 80, 81,
             82, 83, 84, 85, 86,
             87, 88, 89, 90, 91,
             92, 93, 94, 95, 96,
             97, 98, 99, 100, 101,
             102, 104, 105, 107,
             111, 112, 113, 114,
             115, 116, 117, 118,
             119, 120, 121, 122,
             126, 128, 129, 132,
             133, 134, 135),
    start = rep(1, 103),
    stop = rep(16, 103),
    format = c(rep("%16.3f", 14),
               rep("%16.4f", 1),
               rep("%16.3f", 2),

               rep("%16.5f", 1),
               rep("%16.3f", 45),

               rep("%16.6f", 1),
               rep("%16.3f", 25),
               rep("%16.2f", 1),
               rep("%16.3f", 1),
               rep("%16.2f", 3),
               rep("%16.3f", 1),
               rep("%16.2f", 2),
               rep("%16.0f", 2),
               rep("%16.3f", 2),
               rep("%16.0f", 2)
    ),
    description = c("| SFTMP : Snowfall temperature [\u00B0C]",
                    "| SMTMP : Snow melt base temperature [\u00B0C]",
                    "| SMFMX : Melt factor for snow on June 21 [mm H2O/\u00B0C-day]",
                    "| SMFMN : Melt factor for snow on December 21 [mm H2O/\u00B0C-day]",
                    "| TIMP : Snow pack temperature lag factor",
                    "| SNOCOVMX : Minimum snow water content that corresponds to 100% snow cover [mm]",
                    "| SNO50COV : Fraction of snow volume represented by SNOCOVMX that corresponds to 50% snow cover",
                    "| ESCO: Soil evaporation compensation factor",
                    "| EPCO: Plant water uptake compensation factor",
                    "| EVLAI : Leaf area index at which no evaporation occurs from water surface [m2/m2]",
                    "| FFCB : Initial soil water storage as a fraction of field capacity water content",
                    "| SURLAG : Surface runoff lag time [days]",
                    "| ADJ_PKR : Peak rate adjustment for sediment routing in tributaries",
                    "| PRF_BSN : Peak rate adjustment for sediment routing in main channel",
                    "| SPCON : Linear parameter for max sediment reentrained",
                    "| SPEXP : Exponent for sediment reentrained",
                    "| RCN : Nitrogen concentration in rainfall [mg N/l]",
                    "| CMN : Humus mineralization rate factor",
                    "| N_UPDIS : Nitrogen uptake distribution",
                    "| P_UPDIS : Phosphorus uptake distribution",
                    "| NPERCO : Nitrogen percolation coefficient",
                    "| PPERCO : Phosphorus percolation coefficient",
                    "| PHOSKD : Phosphorus soil partitioning coefficient",
                    "| PSP : Phosphorus sorption coefficient",
                    "| RSDCO : Residue decomposition coefficient",
                    "| PERCOP : Pesticide percolation coefficient",
                    "| WDPQ : Die-off factor for persistent bacteria [1/day]",
                    "| WGPQ : Growth factor for persistent bacteria [1/day]",
                    "| WDLPQ : Die-off factor for less persistent bacteria [1/day]",
                    "| WGLPQ : Growth factor for less persistent bacteria [1/day]",
                    "| WDPS : Die-off factor for bacteria adsorbed to soil particles",
                    "| WGPS : Growth factor for persistent bacteria adsorbed to soil",
                    "| WDLPS : Die-off for less persistent bacteria adsorbed",
                    "| WGLPS : Growth for less persistent bacteria adsorbed",
                    "| BACTKDQ : Bacteria partition coefficient",
                    "| THBACT : Temperature factor for bacteria die-off",
                    "| WOF_P: Wash-off fraction for persistent bacteria",
                    "| WOF_LP: Wash-off fraction for less persistent bacteria",
                    "| WDPF: Persistent bacteria die-off on foliage",
                    "| WGPF: Persistent bacteria growth on foliage",
                    "| WDLPF: Less persistent bacteria die-off foliage",
                    "| WGLPF: Less persistent bacteria growth foliage",
                    "| MSK_CO1 : Coefficient for normal flow",
                    "| MSK_CO2 : Coefficient for low flow",
                    "| MSK_X : Weighting factor for inflow-outflow water storage",
                    "| TRNSRCH: Reach transmission loss partitioning",
                    "| EVRCH : Reach evaporation adjustment",
                    "| CNCOEF : Plant ET curve number coefficient",
                    "| CDN : Denitrification exponential rate",
                    "| SDNCO : Denitrification threshold water content",
                    "| BACT_SWF : Manure application active CFU fraction",
                    "| BACTMX : Bacteria percolation coefficient",
                    "| BACTMINLP : Min bacteria loss for less persistent",
                    "| BACTMINP : Min bacteria loss for persistent",
                    "| WDLPRCH: Die-off less persistent bacteria in streams",
                    "| WDPRCH : Die-off for persistent bacteria in streams",
                    "| WDLPRES : Die-off for less persistent in water bodies",
                    "| WDPRES : Die-off for persistent in water bodies",
                    "| TB_ADJ : Subdaily unit hydrograph basetime adjustment",
                    "| DEPIMP_BSN : Depth to impervious layer",
                    "| DDRAIN_BSN : Depth to sub-surface drain",
                    "| TDRAIN_BSN : Time to drain soil to field capacity",
                    "| GDRAIN_BSN : Drain tile lag time",
                    "| CN_FROZ : Frozen soil infiltration/runoff adjustment",
                    "| DORM_HR : Time threshold for dormancy",
                    "| SMXCO : Max curve number S factor adjustment",
                    "| FIXCO : Nitrogen fixation coefficient",
                    "| NFIXMX : Max daily nitrogen fixation",
                    "| ANION_EXCL_BSN : Fraction of porosity excluding anions",
                    "| CH_ONCO_BSN : Channel organic nitrogen",
                    "| CH_OPCO_BSN : Channel organic phosphorus",
                    "| HLIFE_NGW_BSN : Nitrogen groundwater half-life",
                    "| RCN_SUB_BSN : Concentration of nitrate in precipitation",
                    "| BC1_BSN : Biological oxidation rate of NH3",
                    "| BC2_BSN : Oxidation rate NO2 to NO3",
                    "| BC3_BSN : Hydrolosis rate of organic nitrogen",
                    "| BC4_BSN : Organic phosphorus to dissolved decay",
                    "| DECR_MIN: Min daily residue decay",
                    "| ICFAC : C-factor calculation method",
                    "| RSD_COVCO : Residue cover factor",
                    "| VCRIT : Critical velocity",
                    "| RES_STLR_CO : Reservoir sediment settling coefficient",
                    "| BFLO_DIST: Baseflow distribution",
                    "| UHALPHA : Gamma function hydrograph alpha coefficient",
                    "| EROS_SPL: Splash erosion coefficient",
                    "| RILL_MULT: Rill erosion susceptibility multiplier",
                    "| EROS_EXPO: Erosion equation exponent",
                    "| SUBD_CHSED: Sediment transport model",
                    "| C_FACTOR: Cover and management scaling factor",
                    "| CH_D50 : Channel bed median particle diameter",
                    "| SIG_G : Geometric standard deviation particle size",
                    "| RE_BSN: Effective drain radius",
                    "| SDRAIN_BSN: Distance between drain tubes",
                    "| DRAIN_CO_BSN: Drainage coefficient",
                    "| PC_BSN: Pump capacity",
                    "| LATKSATF_BSN: Lateral ksat multiplier",
                    "| IABSTR: Initial abstraction on impervious cover",
                    "| R2ADJ_BSN: Basin retention parameter adjustment",
                    "| SSTMAXD_BSN: Max sediment concentration sand filter",
                    "| CO2_X2: Future CO2 quadratic coefficient",
                    "| CO2_X: Future CO2 linear coefficient",
                    "| SFSEDMEAN: Sand filter sediment concentration mean",
                    "| SFSEDSTDEV: Sand filter sediment concentration std deviation")
  )




  swat_parameters[[".sub"]] <- tibble::tibble(
    parameter = c("PLAPS", "TLAPS", "SNO_SUB", "CH_L1", "CH_S1",
                  "CH_W1", "CH_K1", "CH_N1", "CO2"),
    line = c(21, 22, 23, 25, 26,
             27, 28, 29, 35),
    start = rep(1, 9),
    stop = rep(16, 9),
    format = c(rep("%16.3f", 9)),
    description = c("| PLAPS : Precipitation lapse rate [mm/km]",
                    "| TLAPS : Temperature lapse rate [\u00B0C/km]",
                    "| SNO_SUB : Initial snow water content [mm]",
                    "| CH_L1 : Longest tributary channel length [km]",
                    "| CH_S1 : Average slope of tributary channel [m/m]",
                    "| CH_W1 : Average width of tributary channel [m]",
                    "| CH_K1 : Effective hydraulic conductivity in tributary channel [mm/hr]",
                    "| CH_N1 : Manning's 'n' value for the tributary channels",
                    "| CO2 : Carbon dioxide concentration [ppmv]")
  )

  swat_parameters[[".wgn"]] <- tibble::tibble(
    parameter = c("TMPMX", "TMPMN", "TMPSTDMX", "TMPSTDMN", "PCPMM", "PCPSTD",
                  "PCPSKW", "PR_W1", "PR_W2", "PCPD", "RAINHHMX", "SOLARAV",
                  "DEWPT", "WNDAV"),
    line = c(5, 6, 7, 8, 9, 10,
             11, 12, 13, 14, 15, 16,
             17, 18),
    start = rep(NA, 14),
    stop = rep(NA, 14),
    format = rep(NA, 14),

    description = c(
      "Mean maximum daily air temperature (\u00B0C)",
      "Mean minimum daily air temperature (\u00B0C)",
      "Standard deviation of maximum daily air temperature (\u00B0C)",
      "Standard deviation of minimum daily air temperature (\u00B0C)",
      "Mean monthly precipitation (mm)",
      "Standard deviation of daily precipitation (mm)",
      "Skew coefficient for daily precipitation",
      "Probability of a wet day following a dry day",
      "Probability of a wet day following a wet day",
      "Average number of precipitation days per month",
      "Maximum 0.5-hour rainfall during a storm (mm)",
      "Average daily solar radiation (MJ/m\u00B2)",
      "Average daily dew point temperature (\u00B0C)",
      "Average daily wind speed (m/s)"
    )
  )


  swat_parameters[[".swq"]] <- tibble::tibble(
    parameter = c("RS1", "RS2", "RS3", "RS4", "RS5",
                  "RS6", "RS7", "RK1", "RK2", "RK3",
                  "RK4", "RK5", "RK6", "BC1", "BC2",
                  "BC3", "BC4", "CHPST_REA", "CHPST_VOL",
                  "CHPST_KOC", "CHPST_STL", "CHPST_RSP",
                  "CHPST_MIX", "SEDPST_CONC", "SEDPST_REA",
                  "SEDPST_BRY", "SEDPST_ACT"),
    line = c(3, 4, 5, 6, 7,
             8, 9, 10, 11, 12,
             13, 14, 15, 16, 17,
             18, 19, 21, 22, 23,
             24, 25, 26, 27, 28,
             29, 30),
    start = rep(1, 27),
    stop = rep(16, 27),
    format = c("%16.3f", "%16.3f", "%16.3f", "%16.3f", "%16.3f",
               "%16.3f", "%16.3f", "%16.3f", "%16.3f", "%16.3f",
               "%16.3f", "%16.3f", "%16.3f", "%16.3f", "%16.3f",
               "%16.3f", "%16.3f", "%16.8f", "%16.8f", "%16.8f",
               "%16.8f", "%16.8f", "%16.8f", "%16.8f", "%16.8f",
               "%16.8f", "%16.8f"),
    description = c("| RS1: Local algal settling rate in the reach at 20\u00B0C [m/day]",
                    "| RS2: Benthic (sediment) source rate for dissolved phosphorus in the reach at 20\u00B0C [mg dissolved P/[m2\u00B7day]]",
                    "| RS3: Benthic source rate for NH4-N in the reach at 20\u00B0C [mg NH4-N/[m2\u00B7day]]",
                    "| RS4: Rate coefficient for organic N settling in the reach at 20\u00B0C [day-1]",
                    "| RS5: Organic phosphorus settling rate in the reach at 20\u00B0C [day-1]",
                    "| RS6: Rate coefficient for settling of arbitrary non-conservative constituent in the reach at 20\u00B0C [day-1]",
                    "| RS7: Benthic source rate for arbitrary non-conservative constituent in the reach at 20\u00B0C [mg ANC/[m2\u00B7day]]",
                    "| RK1: Carbonaceous biological oxygen demand deoxygenation rate coefficient in the reach at 20\u00B0C [day-1]",
                    "| RK2: Oxygen reaeration rate in accordance with Fickian diffusion in the reach at 20\u00B0C [day-1]",
                    "| RK3: Rate of loss of carbonaceous biological oxygen demand due to settling in the reach at 20\u00B0C [day-1]",
                    "| RK4: Benthic oxygen demand rate in the reach at 20\u00B0C [mg O2/[m2\u00B7day]]",
                    "| RK5: Coliform die-off rate in the reach at 20\u00B0C [day-1]",
                    "| RK6: Decay rate for arbitrary non-conservative constituent in the reach at 20\u00B0C [day-1]",
                    "| BC1: Rate constant for biological oxidation of NH4 to NO2 in the reach at 20\u00B0C [day-1]",
                    "| BC2: Rate constant for biological oxidation of NO2 to NO3 in the reach at 20\u00B0C [day-1]",
                    "| BC3: Rate constant for hydrolysis of organic N to NH4 in the reach at 20\u00B0C [day-1]",
                    "| BC4: Rate constant for mineralization of organic P to dissolved P in the reach at 20\u00B0C [day-1]",
                    "| CHPST_REA: Pesticide reaction coefficient in reach [day-1]",
                    "| CHPST_VOL: Pesticide volatilization coefficient in reach [m/day]",
                    "| CHPST_KOC: Pesticide partition coefficient between water and air in reach [m3/day]",
                    "| CHPST_STL: Settling velocity for pesticide sorbed to sediment [m/day]",
                    "| CHPST_RSP: Resuspension velocity for pesticide sorbed to sediment [m/day]",
                    "| CHPST_MIX: Mixing velocity (diffusion/dispersion) for pesticide in reach [m/day]",
                    "| SEDPST_CONC: Initial pesticide concentration in reach bed sediment [mg/m3 sediment]",
                    "| SEDPST_REA: Pesticide reaction coefficient in reach bed sediment [day-1]",
                    "| SEDPST_BRY: Pesticide burial velocity in reach bed sediment [m/day]",
                    "| SEDPST_ACT: Depth of active sediment layer for pesticide [m]")
  )



  swat_parameters[[".pnd"]] <- tibble::tibble(
    parameter = c("PND_FR", "PND_PSA", "PND_PVOL", "PND_ESA", "PND_EVOL", "PND_VOL",
                  "PND_SED", "PND_NSED", "PND_K", "NDTARG", "PSETLP1", "PSETLP2",
                  "NSETLP1", "NSETLP2", "CHLAP", "SECCIP", "PND_NO3", "PND_SOLP",
                  "PND_ORGN", "PND_ORGP", "PND_D50", "WET_FR", "WET_NSA", "WET_NVOL",
                  "WET_MXSA", "WET_MXVOL", "WET_VOL", "WET_SED", "WET_NSED",
                  "WET_K", "PSETLW1", "PSETLW2", "NSETLW1", "NSETLW2", "CHLAW",
                  "SECCIW", "WET_NO3", "WET_SOLP", "WET_ORGN", "WET_ORGP",
                  "PNDEVCOEFF", "WETEVCOEFF"),
    line = c(3, 4, 5, 6, 7, 8,
             9, 10, 11, 14, 15, 16,
             17, 18, 19, 20, 21, 22,
             23, 24, 25, 29, 30, 31,
             32, 33, 34, 35, 36, 37,
             38, 39, 40, 41, 42, 43,
             44, 45, 46, 47, 48, 49),
    start = rep(1, 42),
    stop = rep(16, 42),
    format = rep("%16.3f", 42),
    description = c("| PND_FR : Fraction of subbasin area that drains into ponds. The value for PND_FR should be between 0.0 and 1.0. If PND_FR = 1.0, the pond is at the outlet of the subbasin on the main channel",
                    "| PND_PSA: Surface area of ponds when filled to principal spillway [ha]",
                    "| PND_PVOL: Volume of water stored in ponds when filled to the principal spillway [104 m3]",
                    "| PND_ESA: Surface area of ponds when filled to emergency spillway [ha]",
                    "| PND_EVOL: Volume of water stored in ponds when filled to the emergency spillway [104 m3]",
                    "| PND_VOL: Initial volume of water in ponds [104 m3]",
                    "| PND_SED: Initial sediment concentration in pond water [mg/l]",
                    "| PND_NSED: Normal sediment concentration in pond water [mg/l]",
                    "| PND_K: Hydraulic conductivity through bottom of ponds [mm/hr]",
                    "| NDTARG: Number of days needed to reach target storage from current pond storage",
                    "| PSETLP1: Phosphorus settling rate in pond for months IPND1 through IPND2 [m/year]",
                    "| PSETLP2: Phosphorus settling rate in pond for months other than IPND1-IPND2 [m/year]",
                    "| NSETLP1: Initial dissolved oxygen concentration in the reach [mg O2/l]",
                    "| NSETLP2: Initial dissolved oxygen concentration in the reach [mg O2/l]",
                    "| CHLAP: Chlorophyll a production coefficient for ponds [ ]",
                    "| SECCIP: Water clarity coefficient for ponds [m]",
                    "| PND_NO3: Initial concentration of NO3-N in pond [mg N/l]",
                    "| PND_SOLP: Initial concentration of soluble P in pond [mg P/L]",
                    "| PND_ORGN: Initial concentration of organic N in pond [mg N/l]",
                    "| PND_ORGP: Initial concentration of organic P in pond [mg P/l]",
                    "| PND_D50: Median particle diameter of sediment [um]",
                    "| WET_FR : Fraction of subbasin area that drains into wetlands",
                    "| WET_NSA: Surface area of wetlands at normal water level [ha]",
                    "| WET_NVOL: Volume of water stored in wetlands when filled to normal water level [104 m3]",
                    "| WET_MXSA: Surface area of wetlands at maximum water level [ha]",
                    "| WET_MXVOL: Volume of water stored in wetlands when filled to maximum water level [104 m3]",
                    "| WET_VOL: Initial volume of water in wetlands [104 m3]",
                    "| WET_SED: Initial sediment concentration in wetland water [mg/l]",
                    "| WET_NSED: Normal sediment concentration in wetland water [mg/l]",
                    "| WET_K: Hydraulic conductivity of bottom of wetlands [mm/hr]",
                    "| PSETLW1: Phosphorus settling rate in wetland for months IPND1 through IPND2 [m/year]",
                    "| PSETLW2: Phosphorus settling rate in wetlands for months other than IPND1-IPND2 [m/year]",
                    "| NSETLW1: Nitrogen settling rate in wetlands for months IPND1 through IPND2 [m/year]",
                    "| NSETLW2: Nitrogen settling rate in wetlands for months other than IPND1-IPND2 [m/year]",
                    "| CHLAW: Chlorophyll a production coefficient for wetlands [ ]",
                    "| SECCIW: Water clarity coefficient for wetlands [m]",
                    "| WET_NO3: Initial concentration of NO3-N in wetland [mg N/l]",
                    "| WET_SOLP: Initial concentration of soluble P in wetland [mg P/l]",
                    "| WET_ORGN: Initial concentration of organic N in wetland [mg N/l]",
                    "| WET_ORGP: Initial concentration of organic P in wetland [mg P/l]",
                    "| PNDEVCOEFF: Actual pond evaporation is equal to the potential evaporation times the pond evaporation coefficient",
                    "| WETEVCOEFF: Actual wetland evaporation is equal to the potential evaporation times the wetland evaporation coefficient.")
  )



  swat_parameters[[".hru"]] <- tibble::tibble(
    parameter = c("SLSUBBSN", "HRU_SLP", "OV_N", "LAT_TTIME", "LAT_SED", "SLSOIL",
                  "CANMX", "ESCO", "EPCO", "RSDIN", "ERORGN", "ERORGP", "POT_FR",
                  "FLD_FR", "RIP_FR", "POT_TILE", "POT_VOLX", "POT_VOL", "POT_NSED",
                  "POT_NO3L", "DEP_IMP", "EVPOT", "DIS_STREAM", "CF", "CFH",
                  "CFDEC", "SED_CON", "ORGN_CON", "ORGP_CON", "SOLN_CON", "SOLP_CON",
                  "POT_SOLP", "POT_K", "N_REDUC", "N_LAG", "N_LN", "N_LNCO",
                  "SURLAG", "R2ADJ"),
    line = c(3, 4, 5, 6, 7, 8,
             9, 10, 11, 12, 13, 14, 15,
             16, 17, 19, 20, 21, 22,
             23, 24, 28, 29, 30, 31,
             32, 33, 34, 35, 36, 37,
             38, 39, 40, 41, 42, 43,
             44, 45),
    start = rep(1, 39),
    stop = rep(16, 39),
    format = c(rep("%16.3f", 20), "%16.0f", rep("%16.1f", 4), "%16.3f", rep("%16.1f", 13)),
    description = c("| SLSUBBSN : Average slope length [m]",
                    "| HRU_SLP : Average slope steepness [m/m]",
                    "| OV_N : Manning's 'n' value for overland flow",
                    "| LAT_TTIME : Lateral flow travel time [days]",
                    "| LAT_SED : Sediment concentration in lateral flow and groundwater flow [mg/l]",
                    "| SLSOIL : Slope length for lateral subsurface flow [m]",
                    "| CANMX : Maximum canopy storage [mm]",
                    "| ESCO : Soil evaporation compensation factor",
                    "| EPCO : Plant uptake compensation factor",
                    "| RSDIN : Initial residue cover [kg/ha]",
                    "| ERORGN : Organic N enrichment ratio",
                    "| ERORGP : Organic P enrichment ratio",
                    "| POT_FR : Fraction of HRU area that drains into pothole",
                    "| FLD_FR : Fraction of HRU that drains into floodplain",
                    "| RIP_FR : Fraction of HRU that drains into riparian zone",
                    "| POT_TILE : Average daily outflow to main channel from tile flow (depth [mm] over entire HRU)",
                    "| POT_VOLX : Maximum volume of water stored in the pothole (depth [mm] over entire HRU)",
                    "| POT_VOL : Initial volume of water stored in the pothole (depth [mm] over entire HRU)",
                    "| POT_NSED : Normal sediment concentration in pothole [mg/l]",
                    "| POT_NO3L : Nitrate decay rate in pothole [1/day]",
                    "| DEP_IMP : Depth to impervious layer in soil profile [mm]",
                    "| EVPOT : Pothole evaporation coefficient",
                    "| DIS_STREAM : Average distance to stream [m]",
                    "| CF : Decomposition response to soil temperature and moisture",
                    "| CFH : Maximum humification rate",
                    "| CFDEC : Undisturbed soil turnover rate under optimum soil water and temperature",
                    "| SED_CON : Sediment concentration in runoff, after urban BMP is applied",
                    "| ORGN_CON : Organic nitrogen concentration in runoff, after urban BMP is applied",
                    "| ORGP_CON : Organic phosphorus concentration in runoff, after urban BMP is applied",
                    "| SOLN_CON : Soluble nitrogen concentration in runoff, after urban BMP is applied",
                    "| SOLP_CON : Soluble phosphorus concentration in runoff, after urban BMP is applied",
                    "| POT_SOLP : Soluble P loss rate in the pothole",
                    "| POT_K : Hydraulic conductivity of soil surface of pothole",
                    "| N_REDUC : Nitrogen uptake reduction factor not currently used",
                    "| N_LAG : Lag coefficient for calculating nitrate concentration in subsurface drains",
                    "| N_LN : Power function exponent for calculating nitrate concentration in subsurface drains",
                    "| N_LNCO : Coefficient for power function for calculating nitrate concentration in subsurface drains",
                    "| SURLAG : Surface runoff lag time in the HRU (days)",
                    "| R2ADJ : Curve number retention parameter adjustment factor to adjust surface runoff for flat slopes")
  )


  swat_parameters[[".mgt"]] <- tibble::tibble(
    parameter = c("LAI_INIT", "BIO_INIT", "PHU_PLT", "BIOMIX",
                  "CN2", "USLE_P", "BIO_MIN", "FILTERW",
                  "FLOWMIN", "DIVMAX", "FLOWFR", "DDRAIN",
                  "TDRAIN", "GDRAIN"),
    line = c(6, 7, 8, 10,
             11, 12, 13, 14,
             21, 22, 23, 25,
             26, 27),
    start = rep(1, 14),
    stop = rep(16, 14),
    format = c(rep("%16.2f", 7), rep("%16.3f", 7)),
    description = c("| LAI_INIT: Initial leaf area index (IGRO = 1)",
                    "| BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)",
                    "| PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)",
                    "| BIOMIX: Biological mixing efficiency",
                    "| CN2: Initial SCS CN II value",
                    "| USLE_P: USLE support practice factor",
                    "| BIO_MIN: Minimum biomass for grazing (kg/ha)",
                    "| FILTERW: Width of edge of field filter strip (m)",
                    "| FLOWMIN: Min in-stream flow for irrigation diversions (m^3/s)",
                    "| DIVMAX: Max irrigation diversion from reach (+mm/-10^4m^3)",
                    "| FLOWFR: Fraction of flow allowed to be pulled for irrigation",
                    "| DDRAIN: Depth to subsurface tile drain (mm)",
                    "| TDRAIN: Time to drain soil to field capacity (hr)",
                    "| GDRAIN: Drain tile lag time (hr)")
  )


  swat_parameters[[".sol"]] <- tibble::tibble(
    parameter = c("SOL_ZMX", "ANION_EXCL", "SOL_CRK",
                  "SOL_Z", "SOL_BD", "SOL_AWC",
                  "SOL_K", "SOL_CBN", "SOL_ROCK",
                  "SOL_ALB", "USLE_K", "SOL_EC",
                  "SOL_PH", "SOL_CAL"

    ),
    line = c(4, 5, 6,
             8, 9, 10,
             11, 12, 16,
             17, 18, 19,
             20, 21),
    start = rep(NA, 14),
    stop = rep(NA, 14),
    format = c(rep("%8.2f", 1), rep("%6.3f", 2),
               rep("%12.2f", 11)
    ),

    description = c(
      " Maximum rooting depth(m) :",
      " Porosity fraction from which anions are excluded:",
      " Crack volume potential of soil:",
      " Depth                [mm]:",
      " Bulk Density Moist [g/cc]:",
      " Ave. AW Incl. Rock Frag  :",
      " Ksat. (est.)      [mm/hr]:",
      " Organic Carbon [weight %]:",
      " Rock Fragments   [vol. %]:",
      " Soil Albedo (Moist)      :",
      " Erosion K                :",
      " Salinity (EC, Form 5)    :",
      " Soil CACO3               :",
      " Soil pH                  :"
    )
  )



  swat_parameters[[".chm"]] <- tibble::tibble(
    line = c(4, 5, 6, 7, 8),
    parameter = c("SOL_NO3", "SOL_ORGN", "SOL_SOLP", "SOL_ORGP", "PPERCO_SUB"),
    start = rep(NA, 5),
    stop = rep(NA, 5),
    format = rep("%12.2f", 5),
    description = c(
      " Soil NO3 [mg/kg]         :",
      " Soil organic N [mg/kg]   :",
      " Soil labile P [mg/kg]    :",
      " Soil organic P [mg/kg]   :",
      " Phosphorus perc coef     :"
    )
  )



  swat_parameters[[".gw"]] <- tibble::tibble(
    parameter = c("SHALLST", "DEEPST", "GW_DELAY", "ALPHA_BF",
                  "GWQMN", "GW_REVAP", "REVAPMN", "RCHRG_DP",
                  "GWHT", "GW_SPYLD", "SHALLST_N", "GWSOLP",
                  "HLIFE_NGW", "LAT_ORGN", "LAT_ORGP", "ALPHA_BF_D"),
    line = c(2, 3, 4, 5,
             6, 7, 8, 9,
             10, 11, 12, 13,
             14, 15, 16, 17),
    start = rep(1, 16),
    stop = rep(16, 16),
    format = rep("%16.4f", 16),
    description = c("| SHALLST : Initial depth of water in the shallow aquifer [mm]",
                    "| DEEPST : Initial depth of water in the deep aquifer [mm]",
                    "| GW_DELAY : Groundwater delay [days]",
                    "| ALPHA_BF : Baseflow alpha factor [days]",
                    "| GWQMN : Threshold depth of water in the shallow aquifer required for return flow to occur [mm]",
                    "| GW_REVAP : Groundwater 'revap' coefficient",
                    "| REVAPMN : Threshold depth of water in the shallow aquifer for 'revap' to occur [mm]",
                    "| RCHRG_DP : Deep aquifer percolation fraction",
                    "| GWHT : Initial groundwater height [m]",
                    "| GW_SPYLD : Specific yield of the shallow aquifer [m3/m3]",
                    "| SHALLST_N : Initial concentration of nitrate in shallow aquifer [mg N/l]",
                    "| GWSOLP : Concentration of soluble phosphorus in groundwater contribution to streamflow from subbasin [mg P/l]",
                    "| HLIFE_NGW : Half-life of nitrate in the shallow aquifer [days]",
                    "| LAT_ORGN : Organic N in the base flow [mg/L]",
                    "| LAT_ORGP : Organic P in the base flow [mg/L]",
                    "| ALPHA_BF_D : Baseflow alpha factor for deep aquifer [days]")
  )






  swat_parameters[[".sdr"]] <- tibble::tibble(
    parameter = c("RE", "SDRAIN", "DRAIN_CO", "PC", "LATKSATF"),
    line = c(2, 3, 4, 5, 6),
    start = rep(1, 5),
    stop = rep(10, 5),
    format = "%10.2f",
    description = c("| RE: Effective radius of drains (mm)",
                    "| SDRAIN: Distance between two drain tubes or tiles (mm)",
                    "| DRAIN_CO: Drainage coefficient (mm/day)",
                    "| PC: Pump capacity (def pump cap - 1.042mm/hr or 25mm/day)",
                    "| LATKSATF: Multiplication factor to determine conk(j1,j) from sol_k(j1,k) for HRU")
  )



  swat_parameters[[".sep"]] <- tibble::tibble(
    parameter = c("SEP_CAP", "BZ_AREA", "ISEP_TFAIL", "BZ_Z", "BZ_THK",
                  "SEP_STRM_DIST", "SEP_DEN", "BIO_BD", "COEFF_BOD_DC",
                  "COEFF_BOD_CONV", "COEFF_FC1", "COEFF_FC2", "COEFF_FECAL",
                  "COEFF_PLQ", "COEFF_MRT", "COEFF_RSP", "COEFF_SLG1",
                  "COEFF_SLG2", "COEFF_NITR", "COEFF_DENITR", "COEFF_COEFF_PDISTRB",
                  "COEFF_PSORPMAX", "COEFF_SOLPSLP", "COEFF_SOLPINTC"),
    line = c(6, 7, 8, 9, 10,
             11, 12, 13, 14, 15,
             16, 17, 18, 19, 20,
             21, 22, 23, 24, 25,
             26, 27, 28, 29),
    start = c(1, 1, 1, 1, 1,
              1, 1, 1, 1, 1,
              1, 1, 1, 1, 1,
              1, 1, 1, 1, 1,
              1, 1, 1, 1),
    stop = c(13, 13, 13, 13, 13,
             13, 13, 13, 13, 13,
             13, 13, 13, 13, 13,
             13, 13, 13, 13, 13,
             13, 13, 13, 13),
    format = c("%13.1f", "%13.3f", "%13.0f", "%13.3f", "%13.3f",
               "%13.3f", "%13.3f", "%13.3f", "%13.3f", "%13.3f",
               "%13.3f", "%13.3f", "%13.3f", "%13.3f", "%13.3f",
               "%13.3f", "%13.5f", "%13.3f", "%13.3f", "%13.3f",
               "%13.5f", "%13.3f", "%13.3f", "%13.3f"),
    description = c("| SEP_CAP : Average number of permanent residents in a house",
                    "| BZ_AREA : Surface area of drainfield (m2)",
                    "| ISEP_TFAIL : Time until failing system gets fixed, days",
                    "| BZ_Z : Depth to the top of biozone layer (mm)",
                    "| BZ_THK : Thickness of biozone layer (mm)",
                    "| SEP_STRM_DIST : Distance to the stream from the septic HRU (km)",
                    "| SEP_DEN : Number of septic systems per square kilometer",
                    "| BIO_BD : Density of biomass (kg/m3)",
                    "| COEFF_BOD_DC : BOD decay rate coefficient, m3/d",
                    "| COEFF_BOD_CONV : Gram of bacterial growth/gram of BOD",
                    "| COEFF_FC1 : Field capacity coefficient 1, unitless",
                    "| COEFF_FC2 : Field capacity coefficient 2, unitless",
                    "| COEFF_FECAL : F. coli bacteria decay rate coefficient, m3/d",
                    "| COEFF_PLQ : Conversion factor for plaque from TDS",
                    "| COEFF_MRT : Mortality rate coefficient",
                    "| COEFF_RSP : Respiration rate coefficient",
                    "| COEFF_SLG1 : Sloughing coefficient 1",
                    "| COEFF_SLG2 : Sloughing coefficient 2",
                    "| COEFF_NITR : Nitrification rate coefficient",
                    "| COEFF_DENITR : Denitrification rate coefficient",
                    "| COEFF_COEFF_PDISTRB : Linear P sorption distribution coefficient, L/kg",
                    "| COEFF_PSORPMAX : Maximum P sorption capacity, mg P/kg Soil",
                    "| COEFF_SOLPSLP : Slope in the effluent soluble P equation",
                    "| COEFF_SOLPINTC : Intercept in the effluent soluble P equation")
  )



  swat_parameters[[".pcp"]] <- tibble::tibble(
    parameter = c("pcp"),
    line = NA,
    start = NA,
    stop = NA,
    format = NA,
    description = "Precipitation"
  )


  # Subbasin parameters for SWAT_T
  # only the position of following parameters changes
  swat_t_parameters <- list()
  swat_t_parameters[[".sub"]] = tibble::tibble(
    parameter = c("PLAPS", "TLAPS", "SNO_SUB", "CH_L1", "CH_S1",
                  "CH_W1", "CH_K1", "CH_N1", "CO2"),
    line = c(23, 24, 25, 27, 28,
             29, 30, 31, 37),
    start = rep(1, 9),
    stop = rep(16, 9),
    format = c(rep("%16.3f", 9)),
    description = c("| PLAPS : Precipitation lapse rate [mm/km]",
                    "| TLAPS : Temperature lapse rate [\u00B0C/km]",
                    "| SNO_SUB : Initial snow water content [mm]",
                    "| CH_L1 : Longest tributary channel length [km]",
                    "| CH_S1 : Average slope of tributary channel [m/m]",
                    "| CH_W1 : Average width of tributary channel [m]",
                    "| CH_K1 : Effective hydraulic conductivity in tributary channel [mm/hr]",
                    "| CH_N1 : Manning's 'n' value for the tributary channels",
                    "| CO2 : Carbon dioxide concentration [ppmv]")
  )


  if (version == "SWAT_T") {
    swat_parameters[[".sub"]] <- swat_t_parameters[[".sub"]]
  }

  swat_parameters_df <- swat_parameters %>%
    dplyr::bind_rows(.id = "component")


  return(swat_parameters_df)
}


# -------------------------------------------------------------------------
# split_swat_parameters
# -------------------------------------------------------------------------
#' @title Split SWAT parameters for parallel processing
#'
#' @description
#' Split a table of parameter sets into `cores` contiguous chunks for
#' parallel execution, distributing rows as evenly as possible.
#'
#' @param cores integer. Number of chunks to produce (e.g., CPU cores).
#' @param parameter_sample matrix or data.frame. Table of parameter sets
#'   to split (one row per set).
#'
#' @return
#' A list of length `cores` with subsets of `parameter_sample` (input row
#' order preserved).
#'
#' @family Parameter management
#'
#' @examples
#' \donttest{
#' x <- data.frame(a = 1:10, b = 11:20)
#' split <- split_swat_parameters(cores = 3, parameter_sample = x)
#' vapply(split, nrow, integer(1))
#' }
#'
#' @export
#'
split_swat_parameters <- function(cores, parameter_sample) {
  base_rows_per_core <- nrow(parameter_sample) %/% cores
  extra_rows <- nrow(parameter_sample) %% cores
  param_splits <- vector("list", cores)
  start_row <- 1
  for (i in 1:cores) {
    rows_for_core <- base_rows_per_core + ifelse(i <= extra_rows, 1, 0)
    end_row <- start_row + rows_for_core - 1
    param_splits[[i]] <- parameter_sample[start_row:end_row, ]
    start_row <- end_row + 1
  }
  return(param_splits)
}

