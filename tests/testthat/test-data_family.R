# =============================================================================
# Family: Data
#
# Tests verify the integrity and basic structure of datasets bundled
# with the package. No numerical validation is performed.
# =============================================================================

skip_on_cran()
skip_if_not_installed("sf")

# -----------------------------------------------------------------------------
# gridded_climate_netcdfs (NetCDF files)
# -----------------------------------------------------------------------------

test_that("gridded climate NetCDF files are accessible", {

  pr_nc     <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  tasmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
  tasmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")

  expect_true(file.exists(pr_nc))
  expect_true(file.exists(tasmax_nc))
  expect_true(file.exists(tasmin_nc))
})

# -----------------------------------------------------------------------------
# subs1 — Subbasins
# -----------------------------------------------------------------------------

test_that("subs1 is a valid sf polygon object", {

  expect_s3_class(subs1, "sf")
  expect_true(nrow(subs1) > 0)
  expect_true("Subbasin" %in% names(subs1))
})

# -----------------------------------------------------------------------------
# riv1 — River network
# -----------------------------------------------------------------------------

test_that("riv1 is a valid sf lines object", {

  expect_s3_class(riv1, "sf")
  expect_true(nrow(riv1) > 0)
})

# -----------------------------------------------------------------------------
# swat_txtinout_data
# -----------------------------------------------------------------------------

test_that("swat_txtinout_data is a named list of SWAT inputs", {

  expect_type(swat_txtinout_data, "list")
  expect_true(length(swat_txtinout_data) > 0)
  expect_true(is.character(names(swat_txtinout_data)))
})

# -----------------------------------------------------------------------------
# qobserved — Observed discharge
# -----------------------------------------------------------------------------

test_that("qobserved has expected structure", {

  expect_s3_class(qobserved, "data.frame")
  expect_true(all(c("Date", "Flow") %in% names(qobserved)))
  expect_true(inherits(qobserved$Date, "Date"))
})
