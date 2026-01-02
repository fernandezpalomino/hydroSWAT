# =============================================================================
# Family: Output readers
#
# Tests focus on:
#  - reading SWAT output files (RCH, SUB, HRU)
#  - validating user inputs and error handling
#  - auxiliary readers for climate gauge files (PCP, TMP)
#  - variable discovery utilities (get_swat_vars)
#
# Tests rely on the packaged SWAT example project.
# Heavy computation is intentionally avoided.
# =============================================================================

skip_on_cran()

# -----------------------------------------------------------------------------
# output_rch — Reach outputs
# -----------------------------------------------------------------------------

test_that("output_rch reads reach data correctly from example project", {

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  data <- output_rch(
    file = file.path(tmpdir, "TxtInOut", "output.rch"),
    variable = "FLOW_OUTcms",
    target_id = c(1, 3),
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  expect_s3_class(data, "data.frame")
  expect_true(all(c("RCH", "MON", "FLOW_OUTcms") %in% names(data)))
  expect_true(all(unique(data$RCH) %in% c(1, 3)))
  expect_true(inherits(data$MON, "Date"))
})

test_that("output_rch validates input arguments", {

  expect_error(
    output_rch(
      file = "non_existing_file.rch",
      variable = "FLOW_OUTcms",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "doesn't exist"
  )

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")

  expect_error(
    output_rch(
      file = rch_file,
      variable = "INVALID_VAR",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "Invalid variable"
  )

  expect_error(
    output_rch(
      file = rch_file,
      variable = "FLOW_OUTcms",
      time_step = "hourly",
      output_start_date = "2011-01-01"
    ),
    "daily.*monthly"
  )

  expect_error(
    output_rch(
      file = rch_file,
      variable = "FLOW_OUTcms",
      time_step = "daily",
      output_start_date = "01-01-2011"
    ),
    "output_start_date"
  )

  expect_error(
    output_rch(
      file = rch_file,
      variable = "FLOW_OUTcms",
      target_id = "A",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "target_id"
  )
})

# -----------------------------------------------------------------------------
# output_sub — Subbasin outputs
# -----------------------------------------------------------------------------

test_that("output_sub reads subbasin data correctly from example project", {

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  data <- output_sub(
    file = file.path(tmpdir, "TxtInOut", "output.sub"),
    variable = c("PRECIPmm", "WYLDmm"),
    target_id = 1,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  expect_s3_class(data, "data.frame")
  expect_true(all(c("SUB", "MON", "PRECIPmm", "WYLDmm") %in% names(data)))
  expect_true(all(unique(data$SUB) == 1))
  expect_true(inherits(data$MON, "Date"))
})

test_that("output_sub validates input arguments", {

  expect_error(
    output_sub(
      file = "non_existing_file.sub",
      variable = "PRECIPmm",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "doesn't exist"
  )

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  sub_file <- file.path(tmpdir, "TxtInOut", "output.sub")

  expect_error(
    output_sub(
      file = sub_file,
      variable = "INVALID_VAR",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "Invalid variable"
  )

  expect_error(
    output_sub(
      file = sub_file,
      variable = "PRECIPmm",
      time_step = "hourly",
      output_start_date = "2011-01-01"
    ),
    "daily.*monthly"
  )

  expect_error(
    output_sub(
      file = sub_file,
      variable = "PRECIPmm",
      time_step = "daily",
      output_start_date = "01-01-2011"
    ),
    "output_start_date"
  )

  expect_error(
    output_sub(
      file = sub_file,
      variable = "PRECIPmm",
      target_id = "A",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "target_id"
  )
})

# -----------------------------------------------------------------------------
# output_hru — HRU outputs
# -----------------------------------------------------------------------------

test_that("output_hru reads HRU data correctly from example project", {

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  data <- output_hru(
    file = file.path(tmpdir, "TxtInOut", "output.hru"),
    variable = c("PRECIPmm", "ETmm"),
    target_id = NULL,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  expect_s3_class(data, "data.frame")
  expect_true(all(c("GIS", "MON", "PRECIPmm", "ETmm") %in% names(data)))
  expect_true(inherits(data$MON, "Date"))
})

test_that("output_hru validates input arguments", {

  expect_error(
    output_hru(
      file = "non_existing_file.hru",
      variable = "PRECIPmm",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "doesn't exist"
  )

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  hru_file <- file.path(tmpdir, "TxtInOut", "output.hru")

  expect_error(
    output_hru(
      file = hru_file,
      variable = "INVALID_VAR",
      time_step = "daily",
      output_start_date = "2011-01-01"
    ),
    "Invalid variable"
  )

  expect_error(
    output_hru(
      file = hru_file,
      variable = "PRECIPmm",
      time_step = "hourly",
      output_start_date = "2011-01-01"
    ),
    "daily.*monthly"
  )

  expect_error(
    output_hru(
      file = hru_file,
      variable = "PRECIPmm",
      time_step = "daily",
      output_start_date = "01-01-2011"
    ),
    "output_start_date"
  )
})

# -----------------------------------------------------------------------------
# get_swat_vars — Variable discovery helper
# -----------------------------------------------------------------------------

test_that("get_swat_vars returns expected structures", {

  all_vars <- get_swat_vars("all")
  expect_type(all_vars, "list")
  expect_true(all(c("rch", "sub", "hru") %in% names(all_vars)))

  for (type in c("rch", "sub", "hru")) {
    vars <- get_swat_vars(type)
    expect_type(vars, "character")
    expect_gt(length(vars), 0)
  }

  expect_identical(get_swat_vars(), get_swat_vars("all"))
})

test_that("get_swat_vars errors for invalid type", {
  expect_error(
    get_swat_vars("invalid_category"),
    "one of"
  )
})

# -----------------------------------------------------------------------------
# read_pcp — Precipitation gauge reader
# -----------------------------------------------------------------------------

test_that("read_pcp reads SWAT .pcp files correctly", {

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  result <- read_pcp(file.path(tmpdir, "TxtInOut", "pcp1.pcp"))

  expect_type(result, "list")
  expect_true(all(c("gauges", "pcp") %in% names(result)))

  expect_s3_class(result$gauges, "tbl_df")
  expect_s3_class(result$pcp, "tbl_df")
  expect_gt(nrow(result$pcp), 0)
})

test_that("read_pcp errors for invalid input", {
  expect_error(read_pcp("non_existing_file.pcp"))
})

# -----------------------------------------------------------------------------
# read_tmp — Temperature gauge reader
# -----------------------------------------------------------------------------

test_that("read_tmp reads SWAT .tmp files correctly", {

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  result <- read_tmp(file.path(tmpdir, "TxtInOut", "tmp1.tmp"))

  expect_type(result, "list")
  expect_true(all(c("gauges", "tmax", "tmin") %in% names(result)))

  expect_s3_class(result$gauges, "tbl_df")
  expect_s3_class(result$tmax, "tbl_df")
  expect_s3_class(result$tmin, "tbl_df")

  expect_equal(nrow(result$tmax), nrow(result$tmin))
})

test_that("read_tmp errors for invalid input", {
  expect_error(read_tmp("non_existing_file.tmp"))
})
