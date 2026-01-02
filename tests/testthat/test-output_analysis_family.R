# =============================================================================
# Family: Output analysis
#
# Tests focus on:
#  - summarizing SWAT outputs from different spatial units (RCH, SUB, HRU)
#  - validating required inputs for basin-wide aggregation
#  - ensuring consistent and documented output structure
#
# Heavy SWAT execution is intentionally avoided.
# =============================================================================

skip_on_cran()

# -----------------------------------------------------------------------------
# summarize_swat_output — Reach outputs (RCH)
# -----------------------------------------------------------------------------

test_that("summarize_swat_output works with reach data (no AREAkm2 required)", {

  skip_if_not_installed("dplyr")

  # Prepare example reach output (RCH does not require area information)
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  rch_data <- output_rch(
    file = file.path(tmpdir, "TxtInOut", "output.rch"),
    variable = "FLOW_OUTcms",
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  result <- summarize_swat_output(rch_data)

  # Basic structural validation
  expect_type(result, "list")
  expect_true(all(c(
    "monthly", "annual", "mean_annual", "mean_annual_cycle"
  ) %in% names(result)))

  # Reach outputs must NOT include basin-wide summaries
  expect_false("mean_annual_basin" %in% names(result))
  expect_false("basin_area" %in% names(result))
})

# -----------------------------------------------------------------------------
# summarize_swat_output — Subbasin outputs (SUB)
# -----------------------------------------------------------------------------

test_that("summarize_swat_output works with subbasin data when AREAkm2 is provided", {

  skip_if_not_installed("dplyr")

  # Prepare example subbasin output including AREAkm2
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  sub_data <- output_sub(
    file = file.path(tmpdir, "TxtInOut", "output.sub"),
    variable = c("PRECIPmm", "ETmm", "WYLDmm", "AREAkm2"),
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  result <- summarize_swat_output(sub_data, water_year = TRUE)

  # Basin-wide summaries must be available for SUB outputs
  expect_true("mean_annual_basin" %in% names(result))
  expect_true("mean_annual_cycle_basin" %in% names(result))
  expect_true("basin_area" %in% names(result))

  expect_s3_class(result$mean_annual_basin, "data.frame")
  expect_true(is.numeric(result$basin_area$total_area))
})

test_that("summarize_swat_output errors for subbasin data without AREAkm2", {

  skip_if_not_installed("dplyr")

  # Prepare subbasin output and intentionally drop AREAkm2
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  sub_data <- output_sub(
    file = file.path(tmpdir, "TxtInOut", "output.sub"),
    variable = c("PRECIPmm", "ETmm", "WYLDmm", "AREAkm2"),
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  ) |>
    dplyr::select(-AREAkm2)

  # Basin-wide aggregation must fail without area information
  expect_error(
    summarize_swat_output(sub_data),
    "AREAkm2"
  )
})

# -----------------------------------------------------------------------------
# summarize_swat_output — HRU outputs
# -----------------------------------------------------------------------------

test_that("summarize_swat_output works with HRU data when AREAkm2 is provided", {

  skip_if_not_installed("dplyr")

  # Prepare example HRU output including AREAkm2
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  hru_data <- output_hru(
    file = file.path(tmpdir, "TxtInOut", "output.hru"),
    variable = c("PRECIPmm", "ETmm", "WYLDmm", "AREAkm2"),
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  result <- summarize_swat_output(hru_data)

  # HRU-specific summaries must be present
  expect_true("mean_annual_LULC" %in% names(result))
  expect_s3_class(result$mean_annual_LULC, "data.frame")
  expect_true(nrow(result$mean_annual_LULC) > 0)
})

test_that("summarize_swat_output errors for HRU data without AREAkm2", {

  skip_if_not_installed("dplyr")

  # Prepare HRU output and intentionally drop AREAkm2
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  hru_data <- output_hru(
    file = file.path(tmpdir, "TxtInOut", "output.hru"),
    variable = c("PRECIPmm", "ETmm", "WYLDmm", "AREAkm2"),
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  ) |>
    dplyr::select(-AREAkm2)

  # Basin-wide aggregation must fail without area information
  expect_error(
    summarize_swat_output(hru_data),
    "AREAkm2"
  )
})
