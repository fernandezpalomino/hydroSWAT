# =============================================================================
# Family: Data — included datasets
# =============================================================================

# -------------------------------------------------------------------------
# Example basin datasets (overview)
# -------------------------------------------------------------------------
#' Datasets included with the package for demos, vignettes, and tests.
#' Unless otherwise noted, they correspond to the **Huancane River basin (Peru)**.
#'
#' @section Included objects:
#' - \code{gridded_climate_netcdfs}: NetCDF precipitation and temperature (daily).
#' - \code{swat_txtinout_data}: SWAT \code{TxtInOut} inputs as a named list.
#' - \code{subs1}: Subbasins (\code{sf} polygons).
#' - \code{riv1}: River network (\code{sf} lines).
#' - \code{qobserved}: Daily streamflow at the outlet gauge.
#'
#' @family Data
#' @keywords datasets
#' @name example_basin_datasets
NULL


# -------------------------------------------------------------------------
# gridded_climate_netcdfs
# -------------------------------------------------------------------------
#' @title Gridded climate NetCDFs (precipitation and temperature)
#'
#' @description
#' Small NetCDFs with daily precipitation (RAIN4PE) and daily min/max temperature
#' (PISCO). Period: 7 days in Jan 2010. Resolution: 0.1 deg (WGS84; EPSG:4326).
#'
#' @format
#' Three NetCDF files:
#' \describe{
#'   \item{pr.nc}{Daily precipitation (mm/day), 7 layers.}
#'   \item{tasmax.nc}{Daily maximum temperature (deg C), 7 layers.}
#'   \item{tasmin.nc}{Daily minimum temperature (deg C), 7 layers.}
#' }
#'
#' @details
#' Files are located under \code{inst/extdata} and should be accessed with
#' \code{system.file()}.
#'
#' @source
#' \itemize{
#'   \item RAIN4PE: \doi{10.5880/pik.2020.010}
#'   \item PISCO temperature v1.1:
#'   \url{https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/}
#' }
#'
#' @examples
#' pr_nc     <- system.file("extdata", "pr.nc", package = "hydroSWAT")
#' tasmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
#' tasmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
#' # r <- terra::rast(pr_nc)
#' # terra::plot(r[[1]], main = "Precipitation (mm/day) 2010-01-01")
#'
#' @family Data
#' @keywords datasets
#' @name gridded_climate_netcdfs
NULL


# -------------------------------------------------------------------------
# swat_txtinout_data
# -------------------------------------------------------------------------
#' @title SWAT TxtInOut inputs
#'
#' @description
#' Named list that mirrors a SWAT \code{TxtInOut} directory from the example project.
#'
#' @format
#' Named list of character vectors. Each element is the content of one SWAT input
#' file (e.g., \code{file.cio}, \code{.gw}, \code{pcp*.pcp}).
#'
#' @source Prepared by the hydroSWAT authors.
#'
#' @examples
#' str(swat_txtinout_data[1:3])
#'
#' @family Data
#' @keywords datasets
"swat_txtinout_data"


# -------------------------------------------------------------------------
# subs1
# -------------------------------------------------------------------------
#' @title Subbasins (sf)
#'
#' @description
#' Subbasins used by the example SWAT project.
#'
#' @format
#' An \code{sf} object with polygon features and related attributes.
#'
#' @source Prepared by the hydroSWAT authors.
#'
#' @examples
#' # plot(subs1["Subbasin"])
#'
#' @family Data
#' @keywords datasets
"subs1"


# -------------------------------------------------------------------------
# riv1
# -------------------------------------------------------------------------
#' @title River network (sf)
#'
#' @description
#' River reaches used by the example SWAT project.
#'
#' @format
#' An \code{sf} object with line features and related attributes.
#'
#' @source Prepared by the hydroSWAT authors.
#'
#' @examples
#' # plot(riv1["Subbasin"])
#'
#' @family Data
#' @keywords datasets
"riv1"


# -------------------------------------------------------------------------
# qobserved
# -------------------------------------------------------------------------
#' @title Observed streamflow (daily)
#'
#' @description
#' Daily discharge at the outlet gauge of the example basin.
#'
#' @format
#' Tibble/data.frame with:
#' \describe{
#'   \item{Date}{Date (daily).}
#'   \item{Flow}{Discharge (m^3/s).}
#' }
#'
#' @source National Meteorology and Hydrology Service of Peru (SENAMHI).
#'
#' @examples
#' head(qobserved)
#' # plot(qobserved$Date, qobserved$Flow, type = "l",
#' #      xlab = "Date", ylab = "Flow (m^3/s)")
#'
#' @family Data
#' @keywords datasets
"qobserved"
