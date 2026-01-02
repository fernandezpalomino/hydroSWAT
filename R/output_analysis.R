# =============================================================================
# Family: Output analysis
# Summarizing and aggregating SWAT outputs (summarize_swat_output) + helper
# (aggregate_swat_output)
# =============================================================================

# -------------------------------------------------------------------------
# summarize_swat_output
# -------------------------------------------------------------------------
#' @title Summarize SWAT output data
#'
#' @description
#' Processes SWAT model output data from reaches, subbasins, or HRUs and
#' aggregates them at different temporal and spatial scales.
#'
#' - If the input data are daily, they are aggregated to monthly and then
#'   to annual scale.\cr
#' - If the input data are monthly, they are aggregated directly to annual
#'   scale.
#'
#' Additionally, the function computes climatological summaries:\cr
#' - Mean annual cycle and mean annual values for each simulated variable
#'   at the HRU, subbasin, or reach level.\cr
#' - Mean annual values by land use/land cover (LULC) for HRU outputs.\cr
#' - Basin-wide climatological summaries, including mean annual cycle and
#'   mean annual values, which are area-weighted averages across the entire
#'   watershed (only for subbasin and HRU outputs).
#'
#' @param swat_output A data frame containing SWAT output data, typically
#'   read using the \link{output_rch}, \link{output_sub}, or \link{output_hru}
#'   functions.
#' @param water_year Logical. If `TRUE`, adjusts data to hydrological years,
#'   starting from the specified month.
#' @param water_year_start Integer. The month to start the hydrological year
#'   (default is 9 for September).
#'
#' @return A list containing:
#' \itemize{
#'   \item `daily`: Original daily data (if the input is daily, otherwise
#'     `NULL`).
#'   \item `monthly`: Aggregated monthly data or original input if already
#'     monthly.
#'   \item `annual`: Aggregated annual data.
#'   \item `mean_annual`: Mean annual values for each HRU, subbasin, or reach.
#'   \item `mean_annual_cycle`: Mean monthly climatology for each HRU,
#'     subbasin, or reach.
#'   \item `mean_annual_LULC`: Mean annual values aggregated by land use/land
#'     cover (only for HRUs).
#'   \item `mean_annual_basin`: Area-weighted mean annual values across the
#'     entire basin (only for subbasins and HRUs).
#'   \item `mean_annual_cycle_basin`: Area-weighted mean monthly climatology
#'     across the entire basin (only for subbasins and HRU outputs).
#'   \item `basin_area`: Total area of the basin (only for subbasin and HRU
#'     outputs).
#' }
#'
#' @family Output analysis
#' @seealso \link{output_rch}, \link{output_sub}, \link{output_hru}
#' @examples
#' # --- Example with Reach data ---
#' tmpdir <- tempdir()
#' get_swat_example(tmpdir)
#' rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
#' rch_data <- output_rch(file = rch_file, variable = c("FLOW_OUTcms"),
#'                        target_id = NULL, time_step = "daily",
#'                        output_start_date = "2011-01-01")
#' summary_rch <- summarize_swat_output(rch_data)
#' names(summary_rch)
#'
#' # --- Example with Subbasin data ---
#' sub_file <- file.path(tmpdir, "TxtInOut", "output.sub")
#' sub_data <- output_sub(file = sub_file,
#'                        variable = c("AREAkm2", "PRECIPmm", "ETmm", "WYLDmm"),
#'                        target_id = NULL, time_step = "daily",
#'                        output_start_date = "2011-01-01")
#' summary_sub <- summarize_swat_output(sub_data, water_year = TRUE)
#' head(summary_sub$mean_annual)
#'
#' # --- Example with HRU data ---
#' hru_file <- file.path(tmpdir, "TxtInOut", "output.hru")
#' hru_data <- output_hru(file = hru_file,
#'                        variable = c("AREAkm2", "PRECIPmm", "ETmm", "WYLDmm"),
#'                        target_id = NULL, time_step = "daily",
#'                        output_start_date = "2011-01-01")
#' summary_hru <- summarize_swat_output(hru_data)
#' head(summary_hru$mean_annual_LULC)
#'
#'
#' @importFrom tidyr all_of
#' @importFrom dplyr first across any_of where ungroup
#' @importFrom rlang sym
#' @export


summarize_swat_output <- function(swat_output,
                                  water_year = FALSE,
                                  water_year_start = 9) {
  # Determine the source based on available columns
  sources_keycolumn <- c("HRU" = "GIS", "SUB" = "SUB", "RCH" = "RCH")
  source <- match(TRUE, names(sources_keycolumn) %in% names(swat_output))
  if (is.na(source)) stop("swat_output must come from the functions output_hru, output_rch, or output_sub.")
  source <- names(sources_keycolumn)[source]

  # Validate presence of AREAkm2 for SUB and HRU outputs
  # Basin-wide summaries (mean_annual_basin, basin_area, etc.) require area information.
  # If AREAkm2 is missing, stop with an informative error to guide the user.
  if (source %in% c("SUB", "HRU") && !"AREAkm2" %in% names(swat_output)) {
    stop(
      "The input data for '", source, "' must include the column 'AREAkm2'.\n",
      "This column is required to calculate basin-wide summaries.\n",
      "Make sure to use output_sub() or output_hru() with AREAkm2 included."
    )
  }


  # Rename key column and remove unnecessary columns based on source
  if (source == "HRU") {
    swat_output <- swat_output %>% dplyr::rename(KEY = GIS)
    vars_to_sel <- setdiff(colnames(swat_output), c("LULC", "MGT"))
    hru_lulc_gis <- swat_output %>% dplyr::select("LULC", "KEY") %>% unique()
  } else if (source == "SUB") {
    swat_output <- swat_output %>% dplyr::rename(KEY = SUB)
    vars_to_sel <- setdiff(colnames(swat_output), c("COLNAME", "GIS"))
  } else if (source == "RCH") {
    swat_output <- swat_output %>% dplyr::rename(KEY = RCH)
    vars_to_sel <- setdiff(colnames(swat_output), c("COLNAME", "GIS"))
  }

  # Select relevant variables
  swat_output <- swat_output %>%
    dplyr::select(all_of(vars_to_sel))

  # Detect the temporal resolution (daily or monthly)
  sim_dates <- swat_output %>%
    dplyr::select(KEY, MON) %>%
    dplyr::filter(KEY == first(KEY)) %>%
    dplyr::arrange(MON) %>%
    dplyr::slice(1:2) %>%
    dplyr::pull(MON)

  diff_in_days <- as.numeric(difftime(sim_dates[2], sim_dates[1], units = "days"))
  is_daily <- diff_in_days == 1

  # Aggregate daily data to monthly if necessary
  if (is_daily) {
    swat_output_m <- swat_output %>%
      dplyr::rename(!!sym(sources_keycolumn[source]) := KEY) %>%
      aggregate_swat_output(agg_level = "month")

    swat_output_d <- swat_output %>%
      dplyr::rename(!!sym(sources_keycolumn[source]) := KEY)

  } else {
    swat_output_m <- swat_output %>%
      dplyr::rename(!!sym(sources_keycolumn[source]) := KEY)
  }

  # Adjust the data for hydrological year if water_year is TRUE
  if (water_year) {
    swat_output_m <- swat_output_m %>%
      dplyr::rename(KEY = !!sym(source)) %>%
      dplyr::mutate(WATER_YEAR = ifelse(lubridate::month(MON) < water_year_start,
                                        lubridate::year(MON) - 1,
                                        lubridate::year(MON)))
    # Filter for complete years (12 months)
    complete_years <- swat_output_m %>%
      dplyr::count(KEY, WATER_YEAR) %>%
      dplyr::filter(n == 12) %>%  # Complete year has 12 months
      dplyr::select(KEY, WATER_YEAR)

    swat_output_m <- swat_output_m %>%
      dplyr::inner_join(complete_years, by = c("KEY", "WATER_YEAR")) %>%
      dplyr::select(-WATER_YEAR) %>%
      dplyr::rename(!!sym(source) := KEY)
  }

  # Create monthly summary by key and month
  monthly_summary <- swat_output_m %>%
    dplyr::rename(KEY = !!sym(sources_keycolumn[source])) %>%
    dplyr::mutate(MON = lubridate::month(MON)) %>%
    dplyr::group_by(KEY, MON) %>%
    dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    ungroup()

  # Calculate the total area of the basin
  if(source %in% c("SUB", "HRU")){
    basin_area <- monthly_summary %>%
      dplyr::distinct(KEY, AREAkm2, .keep_all = TRUE) %>%  # Select unique sub-basins
      dplyr::summarize(total_area = sum(AREAkm2, na.rm = TRUE), .groups = "drop")

  }

  # Calculate area-weighted mean for each variable
  if(source %in% c("SUB", "HRU")){
    monthly_summary_area_weighted <- monthly_summary %>%
      dplyr::group_by(MON) %>%
      dplyr::summarize(
        across(
          where(is.numeric) & !any_of(c("AREAkm2", "KEY")),
          ~ weighted.mean(.x, w = AREAkm2, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      )%>%
      dplyr::select(-any_of(c("HRU", "SUB")))
  }



  # Annual summary by key (sub-basin, river, or HRU)

  if (water_year) {
    swat_output_y <- swat_output_m %>%
      #dplyr::rename(!!sym(source) := !!sym(sources_keycolumn[source])) %>%
      aggregate_swat_output(agg_level = "year", water_year = water_year,
                            water_year_start = water_year_start)
  } else {
    swat_output_y <- swat_output_m %>%
      aggregate_swat_output(agg_level = "year", water_year = FALSE)
  }

  annual_summary <- swat_output_y %>%
    dplyr::rename(KEY = !!sym(sources_keycolumn[source])) %>%
    dplyr::group_by(KEY) %>%
    dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    ungroup()

  # Calculate area-weighted annual summary
  if(source %in% c("SUB", "HRU")){
    annual_summary_area_weighted <- annual_summary %>%
      dplyr::summarize(
        across(
          where(is.numeric) & !any_of(c("MON", "AREAkm2", "KEY")),
          ~ weighted.mean(.x, w = AREAkm2, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) %>%
      dplyr::select(-any_of(c("HRU", "SUB")))
  }

  # Calculate area-weighted annual summary per LULC
  if(source %in% c("HRU")){
    annual_summary_lulc <- dplyr::right_join(hru_lulc_gis,
                                             annual_summary, by = "KEY") %>%
      dplyr::group_by(LULC) %>%
      dplyr::summarize(
        across(
          where(is.numeric) & !any_of(c("MON", "AREAkm2", "KEY")),
          ~ weighted.mean(.x, w = AREAkm2, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) %>%
      dplyr::select(-any_of(c("HRU", "SUB")))
  }

  # Rename some fields
  monthly_summary <- monthly_summary  %>%
    dplyr::rename(!!sym(sources_keycolumn[source]) := KEY)

  annual_summary <-   annual_summary%>%
    dplyr::rename(!!sym(sources_keycolumn[source]) := KEY)



  if (source == "HRU") {
    result <- list(
      daily = if (is_daily) swat_output_d else NULL,
      monthly = swat_output_m,
      annual = swat_output_y,
      mean_annual = annual_summary,
      mean_annual_cycle = monthly_summary,
      mean_annual_basin = annual_summary_area_weighted,
      mean_annual_cycle_basin = monthly_summary_area_weighted,
      mean_annual_LULC = annual_summary_lulc,
      basin_area = basin_area
    )
  } else if (source == "SUB") {
    result <- list(
      daily = if (is_daily) swat_output_d else NULL,
      monthly = swat_output_m,
      annual = swat_output_y,
      mean_annual = annual_summary,
      mean_annual_cycle = monthly_summary,
      mean_annual_basin = annual_summary_area_weighted,
      mean_annual_cycle_basin = monthly_summary_area_weighted,
      basin_area = basin_area
    )
  } else {
    result <- list(
      daily = if (is_daily) swat_output_d else NULL,
      monthly = swat_output_m,
      annual = swat_output_y,
      mean_annual = annual_summary,
      mean_annual_cycle = monthly_summary
    )
  }

  return(result)
}


# -------------------------------------------------------------------------
# aggregate_swat_output
# -------------------------------------------------------------------------
#' @title Aggregate SWAT model output
#'
#' @description
#' Aggregates SWAT model outputs read by the \link{output_rch},
#' \link{output_sub}, and \link{output_hru} functions on the specified
#' aggregation level (monthly or yearly), with an option to use custom water years.
#'
#' @param swat_output A data frame containing SWAT model output, which must
#'   include a `MON` column (of Date class), representing the time period for
#'   aggregation, and other columns corresponding to the model's output variables.
#' @param agg_level A character string specifying the level of aggregation.
#'   Must be one of "month" or "year". Default is "month". If "year" is chosen,
#'   the data will be aggregated by calendar year or water year (depending on
#'   the `water_year` parameter).
#' @param water_year Logical. If `TRUE`, the function will aggregate data by
#'   water year. The water year is defined by the `water_year_start` parameter.
#'   If `FALSE`, the aggregation will be based on the calendar year.
#' @param water_year_start An integer specifying the month in which the water
#'   year starts (default is 9, representing September). This argument is only
#'   relevant if `water_year = TRUE`. The value must be an integer between 1
#'   and 12.
#' @return A data frame with the aggregated data.
#' @examples
#' \dontrun{
#' # Example: Aggregating SWAT output by month
#' sub_data_daily <- output_sub(file = "output.sub", variable = c("PRECIPmm",
#'   "ETmm"), target_id = 1, time_step = "daily", output_start_date = "1983-01-01",
#'   rev = "692")
#' aggregated_monthly <- aggregate_swat_output(swat_output = sub_data_daily,
#'   agg_level = "month")
#'
#' # Example: Aggregating SWAT output by water year starting in September
#' aggregated_water_year <- aggregate_swat_output(swat_output = sub_data_daily,
#'   agg_level = "year", water_year = TRUE, water_year_start = 9)
#' }
#'
#' @importFrom rlang sym
#' @importFrom tidyr all_of
#' @importFrom dplyr first
#' @noRd

aggregate_swat_output <- function(swat_output,
                                  agg_level = "month",
                                  water_year = FALSE,
                                  water_year_start = 9) {
  # Supported aggregation levels
  valid_agg_levels <- c("month", "year")
  agg_level <- match.arg(agg_level, valid_agg_levels)

  # Validate water_year_start
  if (!is.numeric(water_year_start) || water_year_start < 1 || water_year_start > 12) {
    stop("Invalid water_year_start. Choose a month number between 1 and 12.")
  }

  # Define variable categories for each source
  source_vars <- list(
    HRU = list(
      sum_vars = c(
        "PRECIPmm", "SNOFALLmm", "SNOMELTmm", "IRRmm", "PETmm", "ETmm",
        "PERCmm", "GW_RCHGmm", "DA_RCHGmm", "REVAPmm", "SA_IRRmm", "DA_IRRmm",
        "SURQ_GENmm", "SURQ_CNTmm", "TLOSSmm", "LATQGENmm", "GW_Qmm", "WYLDmm",
        "SYLDt/ha", "USLEt/ha", "N_APPkg/ha", "P_APPkg/ha", "NAUTOkg/ha",
        "PAUTOkg/ha", "NGRZkg/ha", "PGRZkg/ha", "NCFRTkg/ha", "PCFRTkg/ha",
        "NRAINkg/ha", "NFIXkg/ha", "F-MNkg/ha", "A-MNkg/ha", "A-SNkg/ha",
        "F-MPkg/ha", "AO-LPkg/ha", "L-APkg/ha", "A-SPkg/ha", "DNITkg/ha",
        "NUPkg/ha", "PUPkg/ha", "ORGNkg/ha", "ORGPkg/ha", "SEDPkg/ha",
        "NSURQkg/ha", "NLATQkg/ha", "NO3Lkg/ha", "NO3GWkg/ha", "SOLPkg/ha",
        "P_GWkg/ha", "W_STRS", "TMP_STRS", "N_STRS", "P_STRS", "BIOMt/ha",
        "YLDt/ha", "SNOmm", "CMUPkg/ha", "CMTOTkg/ha", "QTILEmm", "TNO3kg/ha",
        "LNO3kg/ha", "GW_Q_Dmm", "LATQCNTmm", "TVAPkg/ha"
      ),
      mean_vars = c(
        "HRU", "SUB", "AREAkm2", "SW_INITmm", "SW_ENDmm", "SA_STmm", "DA_STmm",
        "DAILYCN", "TMP_AVdgC", "TMP_MXdgC", "TMP_MNdgC", "SOL_TMPdgC",
        "SOLARMJ/m2", "LAI", "BACTPct", "BACTLPct", "WTAB_CLIm", "WTAB_SOLm"
      )
    ),
    SUB = list(
      sum_vars = c("PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "PERCmm",
                   "SURQmm", "GW_Qmm", "WYLDmm", "SYLDt/ha", "ORGNkg/ha",
                   "ORGPkg/ha", "NSURQkg/ha", "SOLPkg/ha", "SEDPkg/ha",
                   "LAT Q(mm)", "LATNO3kg/h", "GWNO3kg/ha", "TNO3kg/ha",
                   "QTILEmm", "TVAPkg/ha"),
      mean_vars = c("AREAkm2", "SWmm", "CHOLAmic/L", "CBODU mg/L", "DOXQ mg/L")
    ),
    RCH = list(
      sum_vars = c("SED_INtons", "SED_OUTtons", "ORGN_INkg", "ORGN_OUTkg",
                   "ORGP_INkg", "ORGP_OUTkg", "NO3_INkg", "NO3_OUTkg",
                   "NH4_INkg", "NH4_OUTkg", "NO2_INkg", "NO2_OUTkg",
                   "MINP_INkg", "MINP_OUTkg", "CHLA_INkg", "CHLA_OUTkg",
                   "CBOD_INkg", "CBOD_OUTkg", "DISOX_INkg", "DISOX_OUTkg",
                   "SOLPST_INmg", "SOLPST_OUTmg", "SORPST_INmg", "SORPST_OUTmg",
                   "REACTPSTmg", "VOLPSTmg", "SETTLPSTmg", "RESUSP_PSTmg",
                   "DIFFUSEPSTmg", "REACBEDPSTmg", "BURYPSTmg", "BED_PSTmg",
                   "BACTP_OUTct", "BACTLP_OUTct", "CMETAL#1kg", "CMETAL#2kg",
                   "CMETAL#3kg", "TOT Nkg", "TOT Pkg"),
      mean_vars = c("AREAkm2", "FLOW_INcms", "FLOW_OUTcms", "EVAPcms",
                    "TLOSScms", "SEDCONCmg/L", "NO3ConcMg/l", "WTMPdegc",
                    "Salt1", "Salt2", "Salt3", "Salt4", "Salt5", "Salt6",
                    "Salt7", "Salt8", "Salt9", "Salt10", "SAR", "EC")
    )

  )

  # Determine the source based on available columns
  sources_keycolumn <- c("HRU" = "GIS", "SUB" = "SUB", "RCH" = "RCH")
  source <- match(TRUE, names(sources_keycolumn) %in% names(swat_output))
  if (is.na(source)) stop("swat_output must come from the functions output_hru,",
                          " output_rch, or output_sub.")
  source <- names(sources_keycolumn)[source]


  # Extract variable lists for the identified source
  sum_vars <- source_vars[[source]]$sum_vars
  mean_vars <- source_vars[[source]]$mean_vars


  # Validate MON column
  if (!"MON" %in% colnames(swat_output) || !lubridate::is.Date(swat_output$MON)) {
    stop("The 'MON' column must be present and of class Date.")
  }


  # Rename key column and remove unnecessary columns based on source
  if (source == "HRU") {
    swat_output <- swat_output %>% dplyr::rename(KEY = GIS)
    vars_to_sel <- setdiff(colnames(swat_output), c("LULC", "MGT"))

    #stop("Supported only for reach and subbasin outputs")
  } else if (source == "SUB") {
    swat_output <- swat_output %>% dplyr::rename(KEY = SUB)
    vars_to_sel <- setdiff(colnames(swat_output), c("COLNAME", "GIS"))
  } else if (source == "RCH") {
    swat_output <- swat_output %>% dplyr::rename(KEY = RCH)
    vars_to_sel <- setdiff(colnames(swat_output), c("COLNAME", "GIS"))
  }

  # Select relevant variables
  swat_output <- swat_output %>%
    dplyr::select(all_of(vars_to_sel))

  # Detect the temporal resolution (daily or monthly)
  sim_dates <- swat_output %>%
    dplyr::select(KEY, MON) %>%
    dplyr::filter(KEY == first(KEY)) %>%
    dplyr::arrange(MON) %>%
    dplyr::slice(1:2) %>%
    dplyr::pull(MON)

  diff_in_days <- as.numeric(difftime(sim_dates[2], sim_dates[1], units = "days"))

  # Determine if the data is monthly or daily
  is_monthly <- diff_in_days >= 28

  # Adjust temporal aggregation
  if (agg_level == "year") {
    if (water_year) {
      # Create the WATER_YEAR column based on the water year start
      swat_output <- swat_output %>%
        dplyr::mutate(WATER_YEAR = ifelse(lubridate::month(MON) < water_year_start,
                                          lubridate::year(MON) - 1,
                                          lubridate::year(MON)))

      # Identify complete years based on temporal resolution
      if (is_monthly) {
        complete_years <- swat_output %>%
          dplyr::count(KEY, WATER_YEAR) %>%
          dplyr::filter(n == 12) %>%  # Complete year has 12 months
          dplyr::select(KEY, WATER_YEAR)
      } else {
        complete_years <- swat_output %>%
          dplyr::count(KEY, WATER_YEAR) %>%
          dplyr::filter(n >= 365 & n <= 366) %>%  # Complete year has 365 or 366 days
          dplyr::select(KEY, WATER_YEAR)
      }

      # Filter to include only complete years
      swat_output <- swat_output %>%
        dplyr::inner_join(complete_years, by = c("KEY", "WATER_YEAR")) %>%
        dplyr::mutate(MON = WATER_YEAR) %>%
        dplyr::select(-WATER_YEAR)
    } else {
      # Use calendar year for aggregation
      swat_output <- swat_output %>%
        dplyr::mutate(MON = lubridate::year(MON))
    }
  } else {
    # If not aggregating to yearly, round down to the nearest month
    swat_output <- swat_output %>%
      dplyr::mutate(MON = lubridate::floor_date(MON, unit = "month"))
  }


  # Initialize an empty data frame for the aggregated results
  vars_to_process <- setdiff(vars_to_sel, c("MON", "KEY"))
  agg_data <- NULL

  # Process each variable according to its aggregation type
  for (var in vars_to_process) {
    if (var %in% sum_vars) {
      agg_func <- sum
    } else if (var %in% mean_vars) {
      agg_func <- mean
    } else {
      stop(paste("The variable", var, "is not supported."))
    }

    # Aggregate data for the current variable
    agg_data_var <- swat_output %>%
      dplyr::select(KEY, MON, !!sym(var)) %>%
      dplyr::group_by(KEY, MON) %>%
      dplyr::summarize(!!var := agg_func(!!sym(var)),#removed na.rm = TRUE
                       .groups = "drop")

    # Merge the aggregated data
    if (is.null(agg_data)) {
      agg_data <- agg_data_var
    } else {
      agg_data <- dplyr::full_join(agg_data, agg_data_var,
                                   by = c("KEY", "MON"))
    }
  }

  # Restore the original key column name
  agg_data <- agg_data %>%
    dplyr::rename(!!sym(sources_keycolumn[source]) := KEY)

  return(agg_data)
}

