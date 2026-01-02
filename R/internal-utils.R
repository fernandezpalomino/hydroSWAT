# =============================================================================
# Internal helpers: readr compatibility + global bindings
# - is.collector2(), col_spec2(), col_concise2(), cols2()
# - utils::globalVariables(...) to silence R CMD check notes
# =============================================================================


# -----------------------------------------------------------------------------
# readr compatibility
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
# modified version of is.collector function for internal use
is.collector2 <- function (x) inherits(x, "collector")


#' @keywords internal
#' @noRd
# modified version of col_spec function for internal use
col_spec2 <- function (col_types, default = readr::col_guess()) {
  stopifnot(is.list(col_types))
  stopifnot(is.collector2(default))
  is_collector <- vapply(col_types, is.collector2, logical(1))
  if (any(!is_collector)) {
    stop("Some `col_types` are not S3 collector objects: ",
         paste(which(!is_collector), collapse = ", "), call. = FALSE)
  }
  structure(list(cols = col_types, default = default), class = "col_spec")
}


#' @keywords internal
#' @noRd
# modified version of col_concise function for internal use
col_concise2 <- function (x) {
  switch(x, `_` = , `-` = readr::col_skip(), `?` = readr::col_guess(),
         c = readr::col_character(), f = readr::col_factor(),
         d = readr::col_double(), i = readr::col_integer(),
         l = readr::col_logical(), n = readr::col_number(),
         D = readr::col_date(), T = readr::col_datetime(),
         t = readr::col_time(),
         stop("Unknown shortcut: ", x, call. = FALSE))
}


#' @keywords internal
#' @noRd
# modified version of cols function of readr package
# x is character vector of column types, see cols function
cols2 <- function (x, .default = readr::col_guess()) {
  #col_types <- list(...)
  x <- as.character(x)

  col_types <- list()
  for(i in 1:length(x)) col_types[[i]] = x[i]

  is_character <- vapply(col_types, is.character, logical(1))
  col_types[is_character] <- lapply(col_types[is_character],
                                    col_concise2)
  if (is.character(.default)) {
    .default <- col_concise2(.default)
  }
  col_spec2(col_types, .default)
}

# -----------------------------------------------------------------------------
# Global variable bindings to avoid R CMD check notes
# -----------------------------------------------------------------------------
#' @noRd
# Global variable bindings to avoid R CMD check notes
utils::globalVariables(c(
  ".", ".data", ":=", "AREAkm2", "Date", "ExceedanceProbability", "Flow",
  "GIS", "HRU", "KEY", "LULC", "MON", "RCH", "SUB", "WATER_YEAR",
  "col_names", "gof", "i", "month", "n", "obs", "period", "sim",
  "swat_txtinout_data", "target_id", "time_step", "unit", "value",
  "yday", "year"
))
