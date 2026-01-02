# =============================================================================
# Family: Preprocessing tools
# Preprocessing gridded data for SWAT (areal_mean, pcp2swat_editor,
# tmp2swat_editor, pcp2swat, tmp2swat)
# =============================================================================

# -------------------------------------------------------------------------
# areal_mean
# -------------------------------------------------------------------------

#' @title Compute areal means of raster data over polygons
#'
#' @description
#' Estimates mean values of raster cells for each polygon. Supports parallel
#' processing, allowing each core to independently load and process raster
#' layers from a NetCDF file. Used internally by \link{pcp2swat_editor}
#' and \link{tmp2swat_editor} for SWAT input preparation.
#'
#' @param grid A `SpatRaster` object for single-core processing.
#' @param geom A set of polygons in `sf` format to perform the extraction.
#' @param start_date The start date for the time series.
#' @param time_step Time step between raster layers. Either 'day' or 'month'.
#'   Default is 'day'.
#' @param cores Number of cores for parallel processing. Default is 1
#'   (single-core).
#' @param grid_file (Optional) Path to the NetCDF file for parallel processing.
#'
#' @return A data frame with the mean values extracted for each polygon and
#'   each raster layer.
#' @family Preprocessing tools
#' @seealso \code{\link{pcp2swat_editor}}, \code{\link{tmp2swat_editor}},
#' \code{\link{pcp2swat}}, \code{\link{tmp2swat}}
#'
#' @examples
#' \donttest{
#' # Example with package NetCDF data
#' pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
#' data(subs1, package = "hydroSWAT")
#' grid <- terra::rast(pr_nc)
#' result <- areal_mean(grid, subs1, "2010-01-01", time_step = "day")
#' }
#'
#' \dontrun{
#' # Example with external NetCDF and shapefile
#' grid <- terra::rast("path_to_raster_file.nc")
#' geom <- sf::st_read("path_to_shapefile.shp")
#' result <- areal_mean(grid, geom, "1981-01-01", time_step = "day",
#'                      cores = 2, grid_file = "path_to_netCDF.nc")
#' }
#' @export

areal_mean <- function(grid, geom, start_date, time_step = 'day', cores = 1,
                       grid_file = NULL) {

  # Checking inputs
  if (!inherits(grid, 'SpatRaster')) {
    stop("'grid' should be a 'SpatRaster' object for processing")
  }

  if (is.na(terra::crs(grid))) {
    stop("Error: 'grid' does not have a CRS (Coordinate Reference System)")
  }

  if (!time_step %in% c('day', 'month')) {
    stop("'time_step' should be 'day' or 'month'")
  }

  stopifnot(is.character(start_date))

  if (!inherits(geom, 'sf')) {
    stop("'geom' should be an 'sf' object")
  }

  if (is.na(sf::st_crs(geom))) {
    stop("Error: 'geom' does not have a CRS")
  }

  # Reproject geom if CRS differs
  if (!identical(terra::crs(grid), sf::st_crs(geom))) {
    geom <- sf::st_transform(geom, terra::crs(grid))
  }

  # Period of data --
  start_date <- as.Date(start_date)
  dates <- seq.Date(start_date, length.out = terra::nlyr(grid), by = time_step)
  final_day <- utils::tail(dates, 1)

  message("Data period is from: ", as.character(start_date), " to ", as.character(final_day))

  # Extraction logic
  num_layers <- terra::nlyr(grid)

  if (cores == 1) {
    # Single-core processing
    avw <- exactextractr::exact_extract(grid, geom, 'mean')
    avw <- as.data.frame(t(avw))
    colnames(avw) <- as.character(1:ncol(avw))
  } else {
    # Parallel processing: check if grid_file is provided
    if (is.null(grid_file) || !file.exists(grid_file)) {
      stop("'grid_file' must be provided and must exist for parallel processing")
    }

    layer_chunks <- split(1:num_layers, sort(rep(1:cores, length.out = num_layers)))

    # Create the parallel processing cluster
    cl <- parallel::makeCluster(cores)
    parallel::clusterEvalQ(cl, {
      library(exactextractr)
      library(terra)
    })

    # Export geom and layer_chunks to the clusters
    parallel::clusterExport(cl, varlist = c("grid_file", "geom", "layer_chunks"),
                            envir = environment())

    # Parallel processing:
    # each core loads the NetCDF file and extracts the corresponding layers
    result_p <- parallel::parLapply(cl, layer_chunks, function(layer_indices) {
      # Load the NetCDF file
      grid <- terra::rast(grid_file)

      # Extract the corresponding layers using `[[` to select layers
      extracted <- exactextractr::exact_extract(grid[[layer_indices]], geom, 'mean')
      extracted <- as.data.frame(t(extracted))
      colnames(extracted) <- as.character(1:ncol(extracted))
      return(extracted)
    })

    parallel::stopCluster(cl)

    # Combine the results obtained in parallel
    avw <- do.call(rbind, result_p)
  }

  # Create final result
  result <- dplyr::bind_cols(Date = as.character(dates), avw)
  row.names(result) <- NULL

  return(result)
}



# -------------------------------------------------------------------------
# pcp2swat_editor
# -------------------------------------------------------------------------
#' @title Generate SWAT Editor-compatible precipitation input files
#'
#' @description
#' Writes SWAT Editor-compatible precipitation input files for each subbasin
#' by estimating areal mean precipitation from raster or NetCDF data using
#' \link{areal_mean}.
#'
#' @param grid A `SpatRaster` object representing precipitation data.
#' @param subs1 An `sf` object representing the subbasins (e.g., shapefile from
#'   ArcSWAT or QSWAT).
#' @param start_date Start date of the record in "yyyy-mm-dd" format.
#' @param time_step Time step between raster layers. Either 'day' or 'month'.
#'   Default is 'day'.
#' @param unit Precipitation unit. Either 'mm' or 'kg/m2/s'. Default is 'mm'.
#' @param cores Number of CPU cores for parallel processing. Default is 1.
#' @param grid_file Path to the NetCDF file (required for parallel processing).
#' @param output_dir Directory where SWAT input files will be written. Defaults
#'   to current working directory.
#'
#' @return Writes SWAT Editor-compatible precipitation input files and a station
#' table in the specified directory.
#' @family Preprocessing tools
#' @seealso \code{\link{areal_mean}}, \code{\link{pcp2swat}}
#'
#' @examples
#' \donttest{
#' # Example with package NetCDF data
#' pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
#' data(subs1, package = "hydroSWAT")
#' grid <- terra::rast(pr_nc)
#' pcp2swat_editor(grid, subs1, "2010-01-01", output_dir = tempdir())
#' }
#'
#' \dontrun{
#' # Example with external NetCDF and shapefile
#' subs1 <- sf::st_read("path_to_subbasin_shapefile.shp")
#' grid <- terra::rast("path_to_precipitation_data.nc")
#' pcp2swat_editor(grid, subs1, "1981-01-01", cores = 3,
#'                 grid_file = "path_to_precipitation_data.nc",
#'                 output_dir = "C:/swat_inputs")
#' }
#' @export
pcp2swat_editor <- function(
    grid,
    subs1,
    start_date,
    time_step = 'day',
    unit = 'mm',
    cores = 1,
    grid_file = NULL,
    output_dir = getwd()
) {

  # Validate unit -------------------------------------------------------------
  if (!unit %in% c('mm', 'kg/m2/s')) {
    stop("'unit' must be either 'mm' or 'kg/m2/s'.")
  }

  # Validate output directory ------------------------------------------------
  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist: ", output_dir)
  }

  # Convert subbasin ID to numeric and sort ---------------------------------
  subs1$Subbasin <- as.numeric(subs1$Subbasin)
  subs1 <- subs1[order(subs1$Subbasin), ]

  # Mean areal precipitation estimation -------------------------------------
  message("Estimating areal mean precipitation for each subbasin...")

  # Calculate mean precipitation for each subbasin using the `areal_mean` function
  mean_rainfall <- areal_mean(
    grid = grid,
    geom = subs1,
    start_date = start_date,
    time_step = time_step,
    cores = cores,
    grid_file = grid_file
  )

  # Unit conversion from kg/m2/s to mm/day
  if (unit == 'kg/m2/s') {
    suppressWarnings({
      mean_rainfall[-1] <- mean_rainfall[-1] * 86400
    })
  }

  # Convert start_date to Date format
  start_date <- as.Date(start_date)

  # Create the output directory for SWAT input files ------------------------
  pcp_swat_dir <- file.path(output_dir, "pcp_swat")
  if (!dir.exists(pcp_swat_dir)) {
    dir.create(pcp_swat_dir, showWarnings = FALSE)
  }

  # Writing rainfall input files for SWAT -----------------------------------
  message("Writing SWAT precipitation input files...")

  formatted_start_date <- format(start_date, "%Y%m%d")

  lapply(2:ncol(mean_rainfall), FUN = function(i) {
    subbasin_data <- round(mean_rainfall[[i]], 1)
    subbasin_data[is.na(subbasin_data)] <- -99
    subbasin_output <- c(formatted_start_date, subbasin_data)

    utils::write.table(
      subbasin_output,
      file = file.path(pcp_swat_dir,
                       paste0("pcp_", sprintf("%05.0f", subs1$Subbasin[i - 1]), ".txt")),
      row.names = FALSE, col.names = FALSE, quote = FALSE
    )
  })

  # Writing stations table ---------------------------------------------------
  stations <- data.frame(
    ID = sprintf("%05.0f", subs1$Subbasin),
    NAME = paste0("pcp_", sprintf("%05.0f", subs1$Subbasin)),
    LAT = round(as.numeric(subs1$Lat), 6),
    LONG = round(as.numeric(subs1$Long_), 6),
    ELEVATION = round(as.numeric(subs1$Elev), 0)
  )

  utils::write.table(stations, file = file.path(pcp_swat_dir, "pcp_stations.txt"),
                     row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

  message("Output files written to: ", pcp_swat_dir)
}

# -------------------------------------------------------------------------
# tmp2swat_editor
# -------------------------------------------------------------------------
#' @title Generate SWAT Editor-compatible temperature input files
#'
#' @description
#' Writes SWAT Editor-compatible temperature input files for each subbasin
#' by estimating areal mean maximum and minimum temperature from raster or
#' NetCDF data using \link{areal_mean}.
#'
#' @param grid_tmax A `SpatRaster` object representing maximum temperature.
#' @param grid_tmin A `SpatRaster` object representing minimum temperature.
#' @param subs1 An `sf` object representing the subbasins (e.g., shapefile from
#'   ArcSWAT or QSWAT).
#' @param start_date Start date of the record in "yyyy-mm-dd" format.
#' @param time_step Time step between raster layers. Either 'day' or 'month'.
#'   Default is 'day'.
#' @param unit Temperature unit. Either 'C' (Celsius) or 'K' (Kelvin).
#'   Default is 'C'.
#' @param cores Number of CPU cores for parallel processing. Default is 1.
#' @param grid_tmax_file Path to the NetCDF file for Tmax (required if cores >1).
#' @param grid_tmin_file Path to the NetCDF file for Tmin (required if cores >1).
#' @param output_dir Directory where SWAT input files will be written. Defaults
#'   to current working directory.
#'
#' @return Writes SWAT Editor-compatible temperature input files and a station
#' table in the specified directory.
#' @family Preprocessing tools
#' @seealso \code{\link{areal_mean}}, \code{\link{tmp2swat}}
#'
#' @examples
#' \donttest{
#' # Example with package NetCDF data
#' tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
#' tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
#' data(subs1, package = "hydroSWAT")
#' grid_tmax <- terra::rast(tmax_nc)
#' grid_tmin <- terra::rast(tmin_nc)
#' tmp2swat_editor(grid_tmax, grid_tmin, subs1, "2010-01-01", output_dir = tempdir())
#' }
#'
#' \dontrun{
#' # Example with external NetCDF and shapefile
#' subs1 <- sf::st_read("path_to_subbasin_shapefile.shp")
#' grid_tmax <- terra::rast("path_to_tmax_data.nc")
#' grid_tmin <- terra::rast("path_to_tmin_data.nc")
#' tmp2swat_editor(grid_tmax, grid_tmin, subs1, "1981-01-01", cores = 3,
#'                 grid_tmax_file = "path_to_tmax_data.nc",
#'                 grid_tmin_file = "path_to_tmin_data.nc",
#'                 output_dir = "C:/swat_inputs")
#' }
#' @export
tmp2swat_editor <- function(
    grid_tmax,
    grid_tmin,
    subs1,
    start_date,
    time_step = 'day',
    unit = 'C',
    cores = 1,
    grid_tmax_file = NULL,
    grid_tmin_file = NULL,
    output_dir = getwd()
) {
  # Validaciones -------------------------------------------------------------
  if (!inherits(grid_tmax, "SpatRaster")) stop("grid_tmax must be a 'SpatRaster' object.")
  if (!inherits(grid_tmin, "SpatRaster")) stop("grid_tmin must be a 'SpatRaster' object.")

  if (terra::nlyr(grid_tmax) != terra::nlyr(grid_tmin)) {
    stop("grid_tmax and grid_tmin must have the same number of layers.")
  }

  # Validate unit -------------------------------------------------------------
  if (!unit %in% c('C', 'K')) stop("'unit' must be either 'C' (Celsius) or 'K' (Kelvin).")

  # Validate output directory ------------------------------------------------
  if (!dir.exists(output_dir)) {
    stop("The specified output directory does not exist: ", output_dir)
  }

  # Check cores and grid file existence --------------------------------------
  if (cores > 1) {
    if (is.null(grid_tmax_file) || is.null(grid_tmin_file)) {
      stop("When using multiple cores (cores > 1), both grid_tmax_file and grid_tmin_file must be specified.")
    }
    if (!file.exists(grid_tmax_file)) {
      stop("The specified grid_tmax_file does not exist: ", grid_tmax_file)
    }
    if (!file.exists(grid_tmin_file)) {
      stop("The specified grid_tmin_file does not exist: ", grid_tmin_file)
    }
  }

  # Convert subbasin ID to numeric and sort ---------------------------------
  subs1$Subbasin <- as.numeric(subs1$Subbasin)
  subs1 <- subs1[order(subs1$Subbasin), ]

  # Mean areal temperature estimation ---------------------------------------
  message("Estimating areal mean temperature for each subbasin...")

  # Estimate mean temperature per subbasin using areal_mean function
  mean_tmax <- areal_mean(grid = grid_tmax, geom = subs1,
                          start_date = start_date, time_step = time_step,
                          cores = cores, grid_file = grid_tmax_file)
  mean_tmin <- areal_mean(grid = grid_tmin, geom = subs1,
                          start_date = start_date, time_step = time_step,
                          cores = cores, grid_file = grid_tmin_file)

  # Remove date column from temperature dataframes and adjust to Celsius if needed
  mean_tmax_data <- mean_tmax[-1]
  mean_tmin_data <- mean_tmin[-1]

  if (unit == "K") {
    mean_tmax_data <- mean_tmax_data - 273.15  # Convert Kelvin to Celsius
    mean_tmin_data <- mean_tmin_data - 273.15
  }

  # Convert start_date to Date format ---------------------------------------
  start_date <- as.Date(start_date)

  # Create output directory -------------------------------------------------
  temp_swat_dir <- file.path(output_dir, "tmp_swat")
  if (!dir.exists(temp_swat_dir)) {
    dir.create(temp_swat_dir, showWarnings = FALSE)
  }

  # Writing temperature input files for SWAT --------------------------------
  message("Writing SWAT temperature input files...")

  formatted_start_date <- format(start_date, "%Y%m%d")

  lapply(1:ncol(mean_tmax_data), function(i) {
    tmax_data <- round(mean_tmax_data[[i]], 1)
    tmin_data <- round(mean_tmin_data[[i]], 1)

    tmax_data[is.na(tmax_data)] <- -99
    tmin_data[is.na(tmin_data)] <- -99

    temp_data <- data.frame(c(formatted_start_date, tmax_data),
                            c(formatted_start_date, tmin_data))

    # Write data to file
    output_file <- file.path(temp_swat_dir,
                             paste0("tmp_", sprintf("%05.0f", subs1$Subbasin[i]), ".txt"))
    utils::write.table(temp_data, file = output_file, row.names = FALSE,
                       col.names = FALSE, quote = FALSE, sep = ",")

    # Modify the first line of the written file
    mod <- readLines(con = output_file)
    mod[1] <- formatted_start_date
    writeLines(mod, output_file)
  })

  # Writing stations table --------------------------------------------------
  stations <- data.frame(
    ID = sprintf("%05.0f", subs1$Subbasin),
    NAME = paste0("tmp_", sprintf("%05.0f", subs1$Subbasin)),
    LAT = round(as.numeric(subs1$Lat), 6),
    LONG = round(as.numeric(subs1$Long_), 6),
    ELEVATION = round(as.numeric(subs1$Elev), 0)
  )

  utils::write.table(stations, file = file.path(temp_swat_dir, "tmp_stations.txt"),
                     row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

  message("Output files written to: ", temp_swat_dir)
}

# -------------------------------------------------------------------------
# pcp2swat
# -------------------------------------------------------------------------
#' @title Update SWAT precipitation input files (`pcp*.pcp`)
#'
#' @description
#' Updates daily SWAT precipitation files (`pcp*.pcp`) using an alternative
#' precipitation product provided as a dataframe of subbasin time series,
#' typically estimated with \link{areal_mean}. This enables forcing SWAT with
#' different gridded precipitation datasets instead of the original inputs.
#'
#' @param x A dataframe where the first column ('Date') contains dates in
#' "yyyy-mm-dd" format, and the remaining columns contain subbasin precipitation
#' values. This dataframe is typically produced by \link{areal_mean}.
#' @param pcp A vector of file paths to existing `pcp*.pcp` files, used as
#' templates for writing the updated precipitation data.
#' @param output_dir A character string specifying the directory where the
#' updated files will be written. Defaults to the current working directory.
#'
#' @family Preprocessing tools
#' @seealso \code{\link{areal_mean}}, \code{\link{pcp2swat_editor}}
#' @export
#'
#' @examples
#' \donttest{
#' # Prepare example SWAT TxtInOut and NetCDF inputs
#' tmpdir <- tempdir()
#' txtinout <- get_swat_example(tmpdir)
#' pcp_files <- list.files(file.path(txtinout), pattern = "^pcp.*\\.pcp$", full.names = TRUE)
#'
#' pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
#' pr_grid <- terra::rast(pr_nc)
#'
#' # Example subbasin polygons from the package
#' data(subs1, package = "hydroSWAT")
#'
#' # Compute areal means
#' pr_data <- areal_mean(pr_grid, subs1, "2010-01-01", time_step = "day")
#'
#' # Update pcp*.pcp files
#' outdir <- file.path(tmpdir, "updated_pcp")
#' dir.create(outdir)
#' pcp2swat(pr_data, pcp_files, output_dir = outdir)
#' }

pcp2swat <- function(x, pcp, output_dir = getwd()) {

  # Validate input
  if (!"Date" %in% colnames(x)) stop("The dataframe 'x' must contain a column named 'Date'.")

  # Convert dates
  date <- lubridate::as_date(x$Date)
  year <- lubridate::year(date)
  day <- lubridate::yday(date)

  for (z in seq_along(pcp)) {

    if (!file.exists(pcp[z])) stop("'", pcp[z], "' doesn't exist.")

    l <- readLines(pcp[z])

    # Extract the necessary information from the file
    l2 <- strsplit(l[1], " ")[[1]][3]
    l3 <- strsplit(l2, ",")[[1]]
    id <- as.character(as.numeric(substr(l3, 5, 10)))

    # Define the output file path
    file.out <- file.path(output_dir, basename(pcp[z]))

    # Write metadata to the output file
    cat(l[1:4], file = file.out, sep = "\n", append = FALSE)

    # Prepare time series of precipitation
    dd <- x[, id, drop = FALSE]  # Ensure dd is a dataframe
    dd[is.na(dd)] <- -99  # Replace NA values with -99
    dd <- data.frame(year, day, dd)

    # Prepare output data
    dd_out <- apply(dd, 1, function(row) {
      paste(sprintf("%4.0f", row[1]),
            sprintf("%03.0f", row[2]),
            paste(sprintf("%05.1f", row[3:ncol(dd)]), collapse = ""),
            sep = "")
    })

    # Write time series to the output file
    cat(dd_out, file = file.out, sep = "\n", append = TRUE)
  }

  message("pcp*.pcp files were written into: ", output_dir)
}

# -------------------------------------------------------------------------
# tmp2swat
# -------------------------------------------------------------------------
#' @title Update SWAT temperature input files (`tmp*.tmp`)
#'
#' @description
#' Updates daily SWAT temperature files (`tmp*.tmp`) using alternative
#' maximum and minimum temperature products provided as dataframes of subbasin
#' time series, typically estimated with \link{areal_mean}. This enables
#' forcing SWAT with different gridded temperature datasets instead of the
#' original inputs.
#'
#' @param tmax A dataframe where the first column ('Date') contains dates in
#' "yyyy-mm-dd" format, and the remaining columns contain subbasin maximum
#' temperature values. This dataframe is typically produced by \link{areal_mean}.
#' @param tmin A dataframe with the same structure as `tmax`, but containing
#' subbasin minimum temperature values.
#' @param tmp A vector of file paths to existing `tmp*.tmp` files, used as
#' templates for writing the updated temperature data.
#' @param output_dir A character string specifying the directory where the
#' updated files will be written. Defaults to the current working directory.
#'
#' @family Preprocessing tools
#' @seealso \code{\link{areal_mean}}, \code{\link{tmp2swat_editor}}
#' @export
#'
#' @examples
#' \donttest{
#' # Prepare example SWAT TxtInOut and NetCDF inputs
#' tmpdir <- tempdir()
#' txtinout <- get_swat_example(tmpdir)
#' tmp_files <- list.files(file.path(txtinout), pattern = "^tmp.*\\.tmp$", full.names = TRUE)
#'
#' tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
#' tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
#' tmax_grid <- terra::rast(tmax_nc)
#' tmin_grid <- terra::rast(tmin_nc)
#'
#' # Example subbasin polygons from the package
#' data(subs1, package = "hydroSWAT")
#'
#' # Compute areal means
#' tmax_data <- areal_mean(tmax_grid, subs1, "2010-01-01", time_step = "day")
#' tmin_data <- areal_mean(tmin_grid, subs1, "2010-01-01", time_step = "day")
#'
#' # Update tmp*.tmp files
#' outdir <- file.path(tmpdir, "updated_tmp")
#' dir.create(outdir)
#' tmp2swat(tmax_data, tmin_data, tmp_files, output_dir = outdir)
#' }

tmp2swat <- function(tmax, tmin, tmp, output_dir = getwd()) {

  # Validations
  if (!"Date" %in% colnames(tmax)) stop("The dataframe 'tmax' must contain a column named 'Date'.")
  if (!"Date" %in% colnames(tmin)) stop("The dataframe 'tmin' must contain a column named 'Date'.")
  if (ncol(tmax) != ncol(tmin)) stop("tmax and tmin must have the same number of columns.")
  if (nrow(tmax) != nrow(tmin)) stop("tmax and tmin must have the same number of rows.")

  # Convert dates
  date <- lubridate::as_date(tmax$Date)
  year <- lubridate::year(date)
  day <- lubridate::yday(date)

  # Loop through each file in the `tmp` vector
  for (z in seq_along(tmp)) {

    # Check if the file exists
    if (!file.exists(tmp[z])) stop("'", tmp[z], "' doesn't exist.")

    # Read the contents of the tmp file
    l <- readLines(tmp[z])

    # Extract the subbasin ID from the file
    l2 <- strsplit(l[1], " ")[[1]][3]
    l3 <- strsplit(l2, ",")[[1]]
    id <- as.character(as.numeric(substr(l3, 5, 10)))

    # Define the output file path
    file.out <- file.path(output_dir, basename(tmp[z]))

    # Write metadata to the output file
    cat(l[1:4], file = file.out, sep = "\n", append = FALSE)

    # Prepare the temperature data (maximum and minimum)
    tmax_z <- tmax[, id, drop = FALSE]
    tmax_z[is.na(tmax_z)] <- -99

    tmin_z <- tmin[, id, drop = FALSE]
    tmin_z[is.na(tmin_z)] <- -99

    # Intercalate tmax and tmin columns for each subbasin
    dd <- do.call(cbind, lapply(seq_len(ncol(tmax_z)), function(x) {
      cbind(tmax_z[, x], tmin_z[, x])
    }))

    # Combine with year and day columns
    dd <- data.frame(year, day, dd)

    # Prepare output data
    dd_out <- apply(dd, 1, function(row) {
      paste(sprintf("%4.0f", row[1]),  # Year
            sprintf("%03.0f", row[2]),  # Day of the year
            paste(sprintf("%05.1f", row[3:ncol(dd)]), collapse = ""),
            sep = "")
    })

    # Write time series to the output file
    cat(dd_out, file = file.out, sep = "\n", append = TRUE)
  }

  message("tmp*.tmp files were written into: ", output_dir)

}
