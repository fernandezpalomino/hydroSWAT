# =============================================================================
# Family: Project setup and execution
# SWAT model setup and execution (get_swat_example, setup_swat,
# download_swat_exe, run_swat_exe, run_swat) + helpers
# =============================================================================

# get_swat_example -------------------------------------------------------------
#' Write example SWAT TxtInOut files to a local folder
#'
#' Extracts the example SWAT input files stored in \link{swat_txtinout_data}
#' and writes them under the \code{TxtInOut} subfolder of \code{path}. This
#' provides a self-contained example SWAT project for testing and tutorials.
#'
#' @param path character. Destination directory where files will be written.
#'   Defaults to the current working directory.
#' @param overwrite logical. Whether to overwrite existing files. Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns the full path to the created \code{TxtInOut}
#'   directory.
#' @family Project setup and execution
#' @export
#'
#' @seealso \link{swat_txtinout_data}
#'
#' @examples
#' \donttest{
#' # Write files to a temporary directory
#' path <- tempdir()
#' get_swat_example(path)
#'
#' # Inspect files
#' list.files(file.path(path, "TxtInOut"))
#' }

get_swat_example <- function(path = ".", overwrite = FALSE) {
  stopifnot(is.character(path), length(path) == 1,
            is.logical(overwrite), length(overwrite) == 1)
  out_dir <- file.path(path, "TxtInOut")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in names(swat_txtinout_data)) {
    dest <- file.path(out_dir, file)
    if (!file.exists(dest) || overwrite) {
      writeLines(swat_txtinout_data[[file]], dest)
    }
  }

  message("SWAT TxtInOut files written to: ", out_dir)
  invisible(out_dir)
}

# setup_swat -------------------------------------------------------------------
#' @title Configure a SWAT project for simulation
#'
#' @description
#' Configures a SWAT project by editing the \code{file.cio} file. Sets the
#' simulation period, time step, warm-up years, and output variables for
#' reaches, subbasins, and HRUs.
#'
#' @param sim_start_date character. Simulation start date ("YYYY-MM-DD").
#' @param sim_end_date character. Simulation end date ("YYYY-MM-DD").
#' @param time_step character. Simulation time step: \code{"daily"} or
#'   \code{"monthly"}.
#' @param NYSKIP integer. Years to skip as warm-up before outputs are
#'   written.
#' @param rch_vars character or NULL. Reach variables to output (see
#'   Details for limits and valid names). If \code{NULL}, zeros are written
#'   and SWAT prints all reach variables by default.
#' @param sub_vars character or NULL. Subbasin variables to output (see
#'   Details for limits and valid names). If \code{NULL}, zeros are written
#'   and SWAT prints all subbasin variables by default.
#' @param hru_vars character or NULL. HRU variables to output (see Details
#'   for limits and valid names). If \code{NULL}, zeros are written and SWAT
#'   prints all HRU variables by default.
#' @param hrus integer or NULL. HRU IDs to include. If \code{NULL}, zeros
#'   are written and SWAT prints all HRUs by default.
#'
#' @details
#' The working directory must contain a valid SWAT \code{TxtInOut} project.
#'
#' \strong{Variable limits.} In custom mode SWAT allows up to 20 variables
#' for reaches, up to 15 for subbasins, and up to 20 for HRUs. Exceeding
#' these limits throws an error.
#'
#' \strong{Available variables.} Use \link{get_swat_vars} to list valid
#' names per element:
#' \itemize{
#'   \item Reaches: \code{get_swat_vars("rch")}
#'   \item Subbasins: \code{get_swat_vars("sub")}
#'   \item HRUs: \code{get_swat_vars("hru")}
#' }
#' Use exact SWAT variable names as returned by \code{get_swat_vars()}.
#'
#' Metadata columns (\code{COLNAME}, \code{SUB}, \code{RCH}, \code{GIS},
#' \code{MON}, \code{AREAkm2}, \code{LULC}, \code{HRU}, \code{MGT}) are
#' always printed and must not be included in the selection.
#'
#' @return A list summarizing the applied configuration with fields:
#' \itemize{
#'   \item \code{output_start_date}: first date with outputs after warm-up.
#'   \item \code{time_step}: the selected time step.
#'   \item \code{rch_vars}, \code{sub_vars}, \code{hru_vars}: the user
#'     selections (or \code{NULL} if all by default).
#'   \item \code{hrus}: selected HRU IDs (or \code{NULL} if all by default).
#' }
#'
#' @note
#' This function overwrites \code{file.cio}. Back up your project if needed.
#'
#' @family Project setup and execution
#' @export
#'
#' @examples
#' \donttest{
#'   old_wd <- getwd()
#'   on.exit(setwd(old_wd), add = TRUE)
#'
#'   tmpdir <- tempdir()
#'   get_swat_example(tmpdir)
#'   setwd(file.path(tmpdir, "TxtInOut"))
#'
#'   # List available variables
#'   get_swat_vars("rch")
#'
#'   # Configure project
#'   setup_swat(
#'     sim_start_date = "2010-01-01",
#'     sim_end_date   = "2015-12-31",
#'     time_step      = "daily",
#'     NYSKIP         = 1,
#'     rch_vars       = c("FLOW_OUTcms", "SEDCONCmg/L"),
#'     sub_vars       = c("PRECIPmm", "ETmm"),
#'     hru_vars       = c("PRECIPmm"),
#'     hrus           = c(1, 2, 3)
#'   )
#' }

setup_swat <- function(sim_start_date, sim_end_date, time_step, NYSKIP,
                       rch_vars = NULL, sub_vars = NULL, hru_vars = NULL,
                       hrus = NULL) {

  # Validation
  if (!is.null(rch_vars) && !is.character(rch_vars))
    stop("'rch_vars' must be character")

  if (!is.null(sub_vars) && !is.character(sub_vars))
    stop("'sub_vars' must be character")

  if (!is.null(hru_vars) && !is.character(hru_vars))
    stop("'hru_vars' must be character")

  if (!is.null(hrus) && (!is.numeric(hrus) || any(hrus <= 0)))
    stop("'hrus' must be positive integers")

  if (!is.null(hrus) && any(hrus != as.integer(hrus)))
    stop("'hrus' must be integers")

  if (length(time_step) != 1) stop("'time_step' must be length 1")

  if (!time_step %in% c("daily", "monthly")) {
    stop("Invalid argument: 'time_step' must be 'daily' or 'monthly'.")
  }

  if (length(NYSKIP) != 1)   stop("'NYSKIP' must be length 1")

  if (!is.numeric(NYSKIP) || NYSKIP != as.integer(NYSKIP)) {
    stop("'NYSKIP' must be an integer.", call. = FALSE)
  }

  if (NYSKIP < 0) {
    stop("'NYSKIP' must be >= 0.", call. = FALSE)
  }

  if (!is.character(sim_start_date ) || !is.character(sim_end_date)) {
    stop("Both 'sim_start_date' and 'sim_end_date' must be character strings.")
  }

  if (length(sim_start_date) != 1 || length(sim_end_date) != 1)
    stop("'sim_start_date' and 'sim_end_date' must be length 1")

  sim_start_date  <- as.Date(sim_start_date , format = "%Y-%m-%d")
  sim_end_date <- as.Date(sim_end_date, format = "%Y-%m-%d")

  if (is.na(sim_start_date) || is.na(sim_end_date)) {
    stop("Invalid date format: use 'YYYY-MM-DD'.")
  }

  if (sim_start_date  >= sim_end_date) {
    stop("'sim_start_date' must be earlier than 'sim_end_date'.")
  }

  # Ensure the SWAT project is available
  if (!file.exists(normalizePath("file.cio", mustWork = FALSE))) {
    stop("'file.cio' not found. Run inside a SWAT TxtInOut folder.")
  }

  # Read the SWAT configuration file
  file_cio <- readLines("file.cio")

  # Update simulation period settings
  NBYR <- lubridate::year(sim_end_date) - lubridate::year(sim_start_date ) + 1
  IYR <- lubridate::year(sim_start_date )
  IDAF <- lubridate::yday(sim_start_date )
  IDAL <- lubridate::yday(sim_end_date)

  file_cio[8] <- paste(sprintf("%16.0f", NBYR), sprintf("%4s", ""),
                       "| NBYR : Number of years simulated", sep="")
  file_cio[9] <- paste(sprintf("%16.0f", IYR), sprintf("%4s", ""),
                       "| IYR : Beginning year of simulation", sep="")
  file_cio[10] <- paste(sprintf("%16.0f", IDAF), sprintf("%4s", ""),
                        "| IDAF : Beginning julian day of simulation", sep="")
  file_cio[11] <- paste(sprintf("%16.0f", IDAL), sprintf("%4s", ""),
                        "| IDAL : Ending julian day of simulation", sep="")

  IPRINT <- ifelse(time_step == "daily", 1, 0)
  file_cio[59] <- paste(sprintf("%16.0f", IPRINT), sprintf("%4s", ""),
                        "| IPRINT: print code (month, day, year)", sep="")
  file_cio[60] <- paste(sprintf("%16.0f", NYSKIP), sprintf("%4s", ""),
                        "| NYSKIP: number of years to skip output printing/summarization", sep="")

  # Define available variables for each category
  variables <- list(
    rch =  c(
      "FLOW_INcms" = 1, "FLOW_OUTcms" = 2, "EVAPcms" = 3, "TLOSScms" = 4,
      "SED_INtons" = 5, "SED_OUTtons" = 6, "SEDCONCmg/L" = 7, "ORGN_INkg" = 8,
      "ORGN_OUTkg" = 9, "ORGP_INkg" = 10, "ORGP_OUTkg" = 11, "NO3_INkg" = 12,
      "NO3_OUTkg" = 13, "NH4_INkg" = 14, "NH4_OUTkg" = 15, "NO2_INkg" = 16,
      "NO2_OUTkg" = 17, "MINP_INkg" = 18, "MINP_OUTkg" = 19, "CHLA_INkg" = 20,
      "CHLA_OUTkg" = 21, "CBOD_INkg" = 22, "CBOD_OUTkg" = 23, "DISOX_INkg" = 24,
      "DISOX_OUTkg" = 25, "SOLPST_INmg" = 26, "SOLPST_OUTmg" = 27, "SORPST_INmg" = 28,
      "SORPST_OUTmg" = 29, "REACTPSTmg" = 30, "VOLPSTmg" = 31, "SETTLPSTmg" = 32,
      "RESUSP_PSTmg" = 33, "DIFFUSEPSTmg" = 34, "REACBEDPSTmg" = 35, "BURYPSTmg" = 36,
      "BED_PSTmg" = 37, "BACTP_OUTct" = 38, "BACTLP_OUTct" = 39, "CMETAL#1kg" = 40,
      "CMETAL#2kg" = 41, "CMETAL#3kg" = 42, "TOT Nkg" = 43, "TOT Pkg" = 44,
      "NO3ConcMg/l" = 45, "WTMPdegc" = 46, "Salt1" = 47, "Salt2" = 48,
      "Salt3" = 49, "Salt4" = 50, "Salt5" = 51, "Salt6" = 52,
      "Salt7" = 53, "Salt8" = 54, "Salt9" = 55, "Salt10" = 56,
      "SAR" = 57, "EC" = 58
    ),
    sub = c(
      "PRECIPmm" = 1, "SNOMELTmm" = 2, "PETmm" = 3, "ETmm" = 4, "SWmm" = 5,
      "PERCmm" = 6, "SURQmm" = 7, "GW_Qmm" = 8, "WYLDmm" = 9, "SYLDt/ha" = 10,
      "ORGNkg/ha" = 11, "ORGPkg/ha" = 12, "NSURQkg/ha" = 13, "SOLPkg/ha" = 14,
      "SEDPkg/ha" = 15, "LAT Q(mm)" = 16, "LATNO3kg/h" = 17, "GWNO3kg/ha" = 18,
      "CHOLAmic/L" = 19, "CBODU mg/L" = 20, "DOXQ mg/L" = 21, "TNO3kg/ha" = 22,
      "QTILEmm" = 23, "TVAPkg/ha" = 24
    ),
    hru = c(
      "PRECIPmm" = 1, "SNOFALLmm" = 2, "SNOMELTmm" = 3, "IRRmm" = 4,
      "PETmm" = 5, "ETmm" = 6, "SW_INITmm" = 7, "SW_ENDmm" = 8,
      "PERCmm" = 9, "GW_RCHGmm" = 10, "DA_RCHGmm" = 11, "REVAPmm" = 12,
      "SA_IRRmm" = 13, "DA_IRRmm" = 14, "SA_STmm" = 15, "DA_STmm" = 16,
      "SURQ_GENmm" = 17, "SURQ_CNTmm" = 18, "TLOSSmm" = 19, "LATQGENmm" = 20,
      "GW_Qmm" = 21, "WYLDmm" = 22, "DAILYCN" = 23, "TMP_AVdgC" = 24,
      "TMP_MXdgC" = 25, "TMP_MNdgC" = 26, "SOL_TMPdgC" = 27, "SOLARMJ/m2" = 28,
      "SYLDt/ha" = 29, "USLEt/ha" = 30, "N_APPkg/ha" = 31, "P_APPkg/ha" = 32,
      "NAUTOkg/ha" = 33, "PAUTOkg/ha" = 34, "NGRZkg/ha" = 35, "PGRZkg/ha" = 36,
      "NCFRTkg/ha" = 37, "PCFRTkg/ha" = 38, "NRAINkg/ha" = 39, "NFIXkg/ha" = 40,
      "F-MNkg/ha" = 41, "A-MNkg/ha" = 42, "A-SNkg/ha" = 43, "F-MPkg/ha" = 44,
      "AO-LPkg/ha" = 45, "L-APkg/ha" = 46, "A-SPkg/ha" = 47, "DNITkg/ha" = 48,
      "NUPkg/ha" = 49, "PUPkg/ha" = 50, "ORGNkg/ha" = 51, "ORGPkg/ha" = 52,
      "SEDPkg/ha" = 53, "NSURQkg/ha" = 54, "NLATQkg/ha" = 55, "NO3Lkg/ha" = 56,
      "NO3GWkg/ha" = 57, "SOLPkg/ha" = 58, "P_GWkg/ha" = 59, "W_STRS" = 60,
      "TMP_STRS" = 61, "N_STRS" = 62, "P_STRS" = 63, "BIOMt/ha" = 64,
      "LAI" = 65, "YLDt/ha" = 66, "BACTPct" = 67, "BACTLPct" = 68,
      "WTAB_CLIm" = 69, "WTAB_SOLm" = 70, "SNOmm" = 71, "CMUPkg/ha" = 72,
      "CMTOTkg/ha" = 73, "QTILEmm" = 74, "TNO3kg/ha" = 75, "LNO3kg/ha" = 76,
      "GW_Q_Dmm" = 77, "LATQCNTmm" = 78, "TVAPkg/ha" = 79
    )
  )

  update_variables <- function(vars, element_type, max_vars) {
    output_vector <- rep(0, max_vars)  # Default to zero

    if (!is.null(vars)) {
      vars <- unique(vars)
      if (element_type != "hrus") {
        # Check for invalid variables
        invalid_vars <- setdiff(vars, names(variables[[element_type]]))

        if (length(invalid_vars) > 0) {
          stop(sprintf(
            "Invalid variables for '%s': %s\nValid variables are: %s",
            element_type, paste(invalid_vars, collapse = ", "),
            paste(names(variables[[element_type]]), collapse = ", ")
          ))
        }

        # Ensure selection does not exceed max_vars
        if (length(vars) > max_vars) {
          stop(sprintf("Too many variables selected for '%s'. Maximum allowed: %d",
                       element_type, max_vars))
        }

        # Assign variables
        output_vector[seq_along(vars)] <- variables[[element_type]][vars]
      } else {
        # Special case for HRUs (no validation of variable names)
        if (length(vars) > max_vars) {
          stop(sprintf("Too many HRUs selected. Maximum allowed: %d", max_vars))
        }

        output_vector[seq_along(vars)] <- vars
      }
    }

    return(paste(sprintf("%4s", output_vector), collapse = ""))
  }

  # Apply function and update file_cio
  file_cio[65] <- update_variables(rch_vars, "rch", 20)  # Reach variables
  file_cio[67] <- update_variables(sub_vars, "sub", 15)  # Subbasin variables
  file_cio[69] <- update_variables(hru_vars, "hru", 20)  # HRU variables
  file_cio[71] <- update_variables(hrus, "hrus", 20)     # HRUs to be printed

  cat(file_cio, file = "file.cio", sep = "\n")

  # Compute the start date for SWAT outputs after the warm-up period
  output_start_date <- paste(IYR + NYSKIP, "01", "01", sep = "-")

  outputs <- list(
    sim_start_date  = sim_start_date,
    sim_end_date = sim_end_date,
    time_step = time_step,
    NYSKIP = NYSKIP,
    rch_vars = rch_vars,
    sub_vars = sub_vars,
    hru_vars = hru_vars,
    hrus = hrus,
    output_start_date = output_start_date
  )

  return(outputs)
}


# download_swat_exe ------------------------------------------------------------
#' @title Download SWAT executable
#'
#' @description
#' Downloads and extracts the official SWAT executable (Windows only) to a
#' specified directory, avoiding manual download and extraction steps.
#'
#' @note The default \code{url} targets the official SWAT executables.
#' If it changes upstream, supply a new URL via \code{url}.
#'
#' @param dest_dir character. Directory where the executable will be
#'   extracted. Defaults to the current working directory.
#' @param type character. Type of executable: either \code{"release"} or
#'   \code{"debug"}. Default is \code{"release"}.
#' @param url character. URL to the ZIP archive with SWAT executables.
#'   Defaults to the official ZIP used by this package.
#' @param overwrite logical. Whether to overwrite an existing executable at
#'   \code{dest_dir}. Default is \code{FALSE}.
#'
#' @return character. Full path to the extracted SWAT executable.
#'
#' @family Project setup and execution
#'
#' @examples
#' \donttest{
#' if (.Platform$OS.type == "windows") {
#'   tmpdir <- tempdir()
#'   exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")
#'   exe_path
#' }
#' }
#'
#' @export
download_swat_exe <- function(dest_dir = ".",
                              type = c("release", "debug"),
                              url = "https://swat.tamu.edu/media/3bcg3zvo/rev695_executables.zip",
                              overwrite = FALSE) {
  if (.Platform$OS.type != "windows") {
    stop("download_swat_exe() is only supported on Windows.")
  }

  stopifnot(is.character(dest_dir), length(dest_dir) == 1,
            is.logical(overwrite), length(overwrite) == 1)

  type <- match.arg(type)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)  # limpieza asegurada del ZIP temporal

  message("Downloading SWAT ZIP from:\n  ", url)
  ret <- utils::download.file(url, destfile = zip_file, mode = "wb")
  if (ret != 0) {
    stop("Download failed (status code = ", ret, "). Check the URL or your internet connection.")
  }

  zip_contents <- utils::unzip(zip_file, list = TRUE)$Name
  exe_files <- grep("\\.exe$", zip_contents, value = TRUE)

  pattern <- switch(type, "release" = "rel", "debug" = "debug")
  matched_exe <- exe_files[grepl(pattern, exe_files, ignore.case = TRUE)]

  if (length(matched_exe) == 0) {
    stop("No matching executable (", type, ") found in ZIP archive.")
  }
  if (length(matched_exe) > 1) {
    message("Multiple matching executables found. Using the first:\n  ",
            paste(matched_exe, collapse = "\n  "))
  }

  exe_name <- matched_exe[1]
  exe_path <- file.path(dest_dir, exe_name)

  if (file.exists(exe_path) && !overwrite) {
    exe_path_norm <- normalizePath(exe_path, winslash = "/", mustWork = FALSE)
    message("Executable already exists at:\n  ", exe_path_norm)
    return(invisible(exe_path_norm))
  }

  message("Extracting executable to:\n  ",
          normalizePath(dest_dir, winslash = "/", mustWork = FALSE))
  utils::unzip(zip_file, files = exe_name, exdir = dest_dir, overwrite = TRUE)

  if (!file.exists(exe_path)) {
    stop("Extraction failed. Executable not found at: ", exe_path)
  }

  exe_path_norm <- normalizePath(exe_path, winslash = "/", mustWork = FALSE)
  message("Executable successfully extracted to:\n  ", exe_path_norm)
  invisible(exe_path_norm)
}

# run_swat_exe -----------------------------------------------------------------
#' @title Run SWAT executable
#'
#' @description
#' Executes the SWAT model in the current working directory, which must
#' contain a valid SWAT project (i.e., a \code{TxtInOut} folder with
#' \code{file.cio}).
#'
#' @param swat_exe_path character. Path to the SWAT executable (e.g.,
#'   \code{"swat.exe"}). On Windows, see \link{download_swat_exe}; on
#'   Linux/macOS compile from source.
#'
#' @return integer. Exit status of the SWAT execution:
#' \itemize{
#'   \item \code{0}: Successful execution.
#'   \item Non-zero: An error occurred during execution.
#' }
#'
#' @family Project setup and execution
#'
#' @examples
#' \donttest{
#' if (.Platform$OS.type == "windows") {
#'   old_wd <- getwd()
#'   on.exit(setwd(old_wd), add = TRUE)
#'
#'   # Example SWAT project
#'   tmpdir <- tempdir()
#'   get_swat_example(tmpdir)
#'   setwd(file.path(tmpdir, "TxtInOut"))
#'
#'   # Download SWAT executable
#'   exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")
#'
#'   # Run SWAT
#'   run_swat_exe(exe_path)
#' }
#' }
#'
#' @export

run_swat_exe <- function(swat_exe_path = "swat.exe") {

  # Ensure the SWAT project is available
  if (!file.exists("file.cio")) {
    stop("'file.cio' not found. Ensure the working directory is a ",
         " SWAT project (TxtInOut).")
  }

  # Normalize executable path
  swat_exe_path <- normalizePath(swat_exe_path, mustWork = FALSE)

  # Verify that the executable exists
  if (!file.exists(swat_exe_path)) {
    stop(sprintf("SWAT executable '%s' not found! Check the path.", swat_exe_path))
  }

  # Check if the executable is actually executable (Unix systems)
  if (.Platform$OS.type == "unix" && file.access(swat_exe_path, mode = 1) == -1) {
    stop(sprintf("'%s' is not executable. Check file permissions.", swat_exe_path))
  }

  # Run SWAT
  exit_code <- system(swat_exe_path, intern = FALSE, ignore.stdout = FALSE,
                      ignore.stderr = FALSE)

  return(exit_code)
}


# run_swat ---------------------------------------------------------------------
#' @title Run SWAT model with or without parameter updates
#'
#' @description
#' Executes a SWAT project in one of three modes:
#' \itemize{
#'   \item \strong{"exe"}: Run SWAT without modifying parameters (as-is).
#'   \item \strong{"single_sample"}: Update one parameter set, run, extract
#'         outputs (e.g., apply calibrated params).
#'   \item \strong{"multiple_sample"}: Iterate over many parameter sets; for
#'         each set, update inputs, run, extract outputs, then restore.
#' }
#'
#' Internally, this function integrates:
#' \enumerate{
#'   \item \link{change_multiple_parameters} to update inputs.
#'   \item \link{run_swat_exe} to execute SWAT.
#'   \item \link{output_rch}, \link{output_sub}, \link{output_hru} to extract
#'         outputs.
#' }
#'
#' For batch runs, \link{create_calibration_project} is recommended; it creates
#' a safe copy with a \code{Backup} folder used to restore inputs between runs.
#'
#' \strong{Note:} In \code{"multiple_sample"} mode you must pass at least one
#' of \code{output_rch_args}, \code{output_sub_args}, or \code{output_hru_args}
#' to retrieve model outputs; otherwise no outputs are extracted.
#'
#' @param TxtInOut_dir character. Path to the SWAT \code{TxtInOut} folder.
#'   Defaults to \code{getwd()}.
#' @param swat_exe_path character. Path to the SWAT executable (e.g.,
#'   \code{"swat.exe"}). On Windows, see \link{download_swat_exe}; on
#'   Linux/macOS compile from source.
#' @param execution_mode character. One of \code{"exe"}, \code{"single_sample"},
#'   or \code{"multiple_sample"}.
#' @param parameter_info data.frame. Parameter definitions used to update SWAT
#'   inputs (required for \code{"single_sample"} and \code{"multiple_sample"}).
#'   Minimum columns:
#'   \itemize{
#'     \item \code{component} (e.g., \code{".gw"}, \code{".mgt"})
#'     \item \code{parameter} (name in SWAT input)
#'     \item \code{value} (numeric; value or factor)
#'     \item \code{method} (character; \code{"v"} or \code{"r"})
#'   }
#'   Optional: \code{version} (\code{"SWAT"} or \code{"SWAT_T"}), and
#'   \code{plant_type} (for \code{plant.dat}). Passed to
#'   \link{change_multiple_parameters}.
#' @param subbasins numeric. Subbasin IDs where changes apply. \code{NULL}
#'   applies to all. Passed to \link{change_multiple_parameters}.
#' @param parameter_sample data.frame. Parameter sets for
#'   \code{"multiple_sample"}; each row is one set whose column order matches
#'   \code{parameter_info}. Passed to \link{change_multiple_parameters}.
#' @param output_rch_args list. Arguments for \link{output_rch}.
#' @param output_sub_args list. Arguments for \link{output_sub}.
#' @param output_hru_args list. Arguments for \link{output_hru}.
#' @param cores integer. CPU cores for \code{"multiple_sample"} (parallel).
#'   Default \code{1} (sequential).
#'
#' @return
#' \itemize{
#'   \item \code{"exe"}: Integer SWAT exit code (\code{0} = success).
#'   \item \code{"single_sample"}, \code{"multiple_sample"}: A list with any of
#'         \code{$rch}, \code{$sub}, \code{$hru} if the corresponding
#'         \code{*_args} were provided.
#' }
#'
#' @family Project setup and execution
#' @seealso \link{create_calibration_project},
#'   \link{change_multiple_parameters}, \link{output_rch},
#'   \link{output_sub}, \link{output_hru}
#'
#' @examples
#' \donttest{
#' if (.Platform$OS.type == "windows") {
#'   old_wd <- getwd()
#'   on.exit(setwd(old_wd), add = TRUE)
#'
#'   # Example SWAT project
#'   tmpdir <- tempdir()
#'   get_swat_example(tmpdir)
#'
#'   # Create calibration project (adds Backup)
#'   create_calibration_project(
#'     swat_TxtInOut   = file.path(tmpdir, "TxtInOut"),
#'     destination_dir = tmpdir,
#'     project_name    = "calib_project",
#'     set_working_dir = TRUE
#'   )
#'
#'   # Download SWAT executable
#'   exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")
#'
#'   # Configure project
#'   setup_swat(
#'     sim_start_date = "2010-01-01",
#'     sim_end_date   = "2015-12-31",
#'     time_step      = "daily",
#'     NYSKIP         = 1,
#'     rch_vars       = c("FLOW_OUTcms", "SEDCONCmg/L"),
#'     sub_vars       = c("PRECIPmm", "ETmm"),
#'     hru_vars       = c("PRECIPmm"),
#'     hrus           = c(1, 2, 3)
#'   )
#'
#'   # Parameter definitions
#'   parameter_info <- tibble::tibble(
#'     component = c(".gw", ".mgt"),
#'     parameter = c("GW_DELAY", "CN2"),
#'     value     = c(30, 0.1),
#'     method    = c("v", "r")
#'   )
#'
#'   # Output extraction
#'   output_rch_args <- list(
#'     file = "output.rch",
#'     variable = c("FLOW_OUTcms", "SEDCONCmg/L"),
#'     target_id = 1,
#'     time_step = "daily",
#'     output_start_date = "2011-01-01"
#'   )
#'
#'   # Run as-is (exe)
#'   run_swat(getwd(), exe_path, execution_mode = "exe")
#'
#'   # Multiple sets (e.g., sensitivity)
#'   parameter_sample <- tibble::tibble(
#'     GW_DELAY = c(30, 35),
#'     CN2      = c(0.1, 0.2)
#'   )
#'   run_swat(getwd(), exe_path, "multiple_sample",
#'            parameter_info   = parameter_info,
#'            parameter_sample = parameter_sample,
#'            output_rch_args  = output_rch_args,
#'            cores = 1)
#'
#'   # Single set (e.g., calibrated)
#'   run_swat(getwd(), exe_path, "single_sample",
#'            parameter_info  = parameter_info,
#'            output_rch_args = output_rch_args)
#' }
#' }
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @export

run_swat = function (
    TxtInOut_dir = getwd(),
    swat_exe_path,
    execution_mode = "exe",
    parameter_info = NULL,
    subbasins = NULL,
    parameter_sample = NULL,
    output_rch_args = NULL,
    output_sub_args = NULL,
    output_hru_args = NULL,
    cores = 1
) {

  start_time <- Sys.time()

  # Ensure the SWAT project is available
  validate_swat_directory(TxtInOut_dir, check_backup = FALSE)

  # Set working directory
  setwd(TxtInOut_dir)

  if (!execution_mode %in% c("exe", "single_sample", "multiple_sample")) {
    stop("Invalid 'execution_mode'. Choose from 'exe', 'single_sample', ",
         "or 'multiple_sample'.")
  }


  # mode "exe"
  if (execution_mode == "exe") {
    message("Executing SWAT without modifying parameters (mode 'exe').")
    swat_exit_code <- run_swat_exe(swat_exe_path)
    return(swat_exit_code)  # Executes SWAT only, no results to handle
  }


  # mode "single_sample"
  if (execution_mode == "single_sample") {
    message("Executing SWAT with a single parameter set (mode 'single_sample').")

    if (is.null(parameter_info)) {
      stop("'parameter_info' must be provided when executing in ",
           "'single_sample' mode.")
    }

    sim_results <- run_swat_and_collect_results(
      swat_exe_path, parameter_info, subbasins,
      output_rch_args, output_sub_args, output_hru_args,
      restore_parameters = FALSE
    )

    # Check if any output arguments were provided
    if (all(sapply(list(output_rch_args, output_sub_args, output_hru_args),
                   is.null))) {
      warning("No output arguments provided for 'output_rch_args', ",
              "'output_sub_args', or 'output_hru_args'. ",
              "The simulation will return 'NULL'.")
    }

    return(sim_results)
  }


  # mode "multiple_sample"
  validate_swat_directory(TxtInOut_dir, check_backup = TRUE)

  if (execution_mode == "multiple_sample") {
    message("Executing SWAT with each parameter set (mode 'multiple_sample').")

    if (is.null(parameter_sample)) {
      stop("'parameter_sample' must be provided when executing in ",
           "'multiple_sample' mode.")
    }

    if (is.null(parameter_info)) {
      stop("'parameter_info' must be provided when 'parameter_sample' is ",
           "supplied.")
    }

    if (is.null(output_rch_args) && is.null(output_sub_args) &&
        is.null(output_hru_args)) {
      stop("When 'execution_mode' is 'multiple_sample', provide at least one of ",
           "'output_rch_args', 'output_sub_args', or 'output_hru_args'.")
    }

    if (!is.data.frame(parameter_sample) || nrow(parameter_sample) == 0)
      stop("'parameter_sample' must be a non-empty data.frame")

    if (!is.data.frame(parameter_info) || nrow(parameter_info) == 0)
      stop("'parameter_info' must be a non-empty data.frame")

    if (ncol(parameter_sample) != nrow(parameter_info))
      stop("Number of columns in 'parameter_sample' must match rows in 'parameter_info'")


    if (cores == 1) {
      message("Running in sequential mode (1 core)...")

      result <- lapply(1:nrow(parameter_sample), function(j) {
        parameter_info$value <- as.numeric(parameter_sample[j, ]) #updated
        run_swat_and_collect_results(
          swat_exe_path, parameter_info, subbasins,
          output_rch_args, output_sub_args, output_hru_args,
          restore_parameters = TRUE
        )
      })

    } else {
      if (!is.numeric(cores) || cores != as.integer(cores) || cores < 1)
        stop("'cores' must be a positive integer")

      message("Running in parallel mode with ", cores, " cores...")

      available_cores <- parallel::detectCores()
      cores_to_use <- min(cores, available_cores)

      # Create temporary directories for each core
      core_working_dirs <- create_core_working_dirs(cores_to_use, TxtInOut_dir)
      core_working_dirs_remove <- suppressWarnings(
        normalizePath(file.path(core_working_dirs, ".."))
      )

      # Split parameter sample for parallel processing
      parent_splits <- split_swat_parameters(
        cores = cores_to_use, parameter_sample = parameter_sample
      )

      cl <- parallel::makeCluster(cores_to_use)
      registerDoParallel(cl)

      # Stop cluster and clean temp directories
      on.exit({
        parallel::stopCluster(cl)
        lapply(core_working_dirs_remove, unlink, recursive = TRUE)
      }, add = TRUE)

      # Export necessary functions/data to the cluster
      parallel::clusterEvalQ(cl, library(hydroSWAT))
      parallel::clusterExport(cl, varlist = c("parent_splits", "core_working_dirs"),
                              envir = environment())

      parent_objs <- foreach(i = 1:cores_to_use) %dopar% {
        setwd(core_working_dirs[i])
        params_for_core <- parent_splits[[i]]
        results_for_core <- lapply(1:nrow(params_for_core), function(j) {
          parameter_info$value <- as.numeric(params_for_core[j, ]) #updated
          run_swat_and_collect_results(
            swat_exe_path, parameter_info, subbasins,
            output_rch_args, output_sub_args, output_hru_args,
            restore_parameters = TRUE
          )
        })
        results_for_core
      }
      result <- do.call(c, parent_objs)
    }
  }

  # Execution time
  end_time <- Sys.time()
  formatted_time <- format_execution_time(start_time, end_time)
  cat("Execution time:", formatted_time, "\n")

  return(result)
}


# =============================== Helpers ======================================

# validate_output_args ----------------------------------------------------------
#' @keywords internal
#' @noRd
validate_output_args <- function(args, arg_name) {
  required_keys <- c("file", "variable", "target_id", "time_step",
                     "output_start_date")
  missing_keys <- setdiff(required_keys, names(args))
  if (length(missing_keys) > 0) {
    stop("The following required keys are missing in ", arg_name, ": ",
         paste(missing_keys, collapse = ", "), ".")
  }
}

# extract_output ----------------------------------------------------------------
#' @keywords internal
#' @noRd
extract_output <- function(output_args, output_func, name) {
  validate_output_args(output_args, name)
  config <- output_args
  result <- output_func(
    file = config$file,
    variable = config$variable,
    target_id = config$target_id,
    time_step = config$time_step,
    output_start_date = config$output_start_date
  )

  missing_vars <- setdiff(config$variable, colnames(result))
  if (length(missing_vars) > 0)
    stop("The following variables are missing in '", config$file, "': ",
         paste(missing_vars, collapse = ", "),
         ". Ensure these variables are included in the SWAT output.")

  return(result)
}


# run_swat_and_collect_results --------------------------------------------------
#' @keywords internal
#' @noRd
run_swat_and_collect_results <- function(
    swat_exe_path,
    parameter_info,
    subbasins,
    output_rch_args = NULL,
    output_sub_args = NULL,
    output_hru_args = NULL,
    restore_parameters = FALSE
) {

  # Update parameters with calibration values
  change_output <- change_multiple_parameters(parameter_info, subbasins)

  # Run SWAT model
  swat_exit_code <- run_swat_exe(swat_exe_path)

  if (!is.null(swat_exit_code) && swat_exit_code != 0) {
    stop("SWAT execution failed with exit code: ", swat_exit_code)
  }

  # Retrieve simulated results
  simulated_results <- list()

  if (!is.null(output_rch_args)) {
    simulated_results$rch <- extract_output(
      output_rch_args, output_rch, "output_rch_args"
    )
  }

  if (!is.null(output_sub_args)) {
    simulated_results$sub <- extract_output(
      output_sub_args, output_sub, "output_sub_args"
    )
  }

  if (!is.null(output_hru_args)) {
    simulated_results$hru <- extract_output(
      output_hru_args, output_hru, "output_hru_args"
    )
  }

  # Restore original files from "Backup" only if restore_parameters is TRUE
  if (restore_parameters && !is.null(change_output)) {
    restore_swat_input_files(change_output)
  }

  return(simulated_results)
}

# restore_swat_input_files ------------------------------------------------------
#' @keywords internal
#' @noRd
restore_swat_input_files <- function(change_output) {
  backup_path <- file.path(getwd(), "Backup")

  if (!dir.exists(backup_path)) {
    stop("The 'Backup' folder is missing. Ensure that the working directory ",
         "contains the necessary 'Backup' folder with SWAT input data.")
  }

  files <- unique(unlist(lapply(change_output, function(y) y$files)))

  files_missing <- files[!file.exists(file.path(backup_path, files))]

  if (length(files_missing) > 0) {
    stop("The following files are missing in 'Backup' and cannot be restored: ",
         paste(files_missing, collapse = ", "),
         ". Ensure all required files are present.")
  }

  file.copy(
    from = file.path(backup_path, files),
    to = getwd(),
    overwrite = TRUE,
    copy.mode = TRUE
  )
}

