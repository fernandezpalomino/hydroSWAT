# -------------------------------------------------------------------------
# Tests for Project setup and execution family
# -------------------------------------------------------------------------
# This file tests:
# - get_swat_example()
# - setup_swat()
# - download_swat_exe()
# - run_swat_exe()
# - run_swat()
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Tests for get_swat_example
# -------------------------------------------------------------------------

test_that("get_swat_example writes TxtInOut folder correctly", {
  skip_on_cran()

  tmpdir <- tempdir()
  out <- get_swat_example(tmpdir)

  expect_true(dir.exists(file.path(tmpdir, "TxtInOut")))
  expect_type(out, "character")
  expect_true(file.exists(file.path(out, "file.cio")))
})

test_that("get_swat_example respects overwrite argument (file creation logic)", {
  skip_on_cran()

  tmpdir <- tempdir()
  outdir <- file.path(tmpdir, "TxtInOut")

  # First write
  get_swat_example(tmpdir)
  cio_file <- file.path(outdir, "file.cio")
  expect_true(file.exists(cio_file))

  info1 <- file.info(cio_file)

  # Second call without overwrite → file should not be rewritten
  get_swat_example(tmpdir, overwrite = FALSE)
  info2 <- file.info(cio_file)

  expect_identical(info1$mtime, info2$mtime)
  expect_identical(info1$size,  info2$size)

  # Third call with overwrite → file SHOULD be rewritten
  Sys.sleep(2)  # safe even on Windows / CI
  get_swat_example(tmpdir, overwrite = TRUE)
  info3 <- file.info(cio_file)

  # At least one metadata field must change
  expect_false(
    identical(info2$mtime, info3$mtime) &&
      identical(info2$size,  info3$size)
  )
})

test_that("get_swat_example validates inputs", {
  expect_error(get_swat_example(path = 1))
  expect_error(get_swat_example(path = ".", overwrite = "yes"))
})


# -------------------------------------------------------------------------
# Tests for setup_swat
# -------------------------------------------------------------------------

test_that("setup_swat configures file.cio correctly", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  setwd(file.path(tmpdir, "TxtInOut"))

  out <- setup_swat(
    sim_start_date = "2010-01-01",
    sim_end_date   = "2015-12-31",
    time_step      = "daily",
    NYSKIP         = 1,
    rch_vars       = c("FLOW_OUTcms"),
    sub_vars       = c("PRECIPmm"),
    hru_vars       = c("PRECIPmm"),
    hrus           = c(1, 2)
  )

  expect_type(out, "list")
  expect_named(out,
               c("sim_start_date", "sim_end_date", "time_step", "NYSKIP",
                 "rch_vars", "sub_vars", "hru_vars", "hrus",
                 "output_start_date"))

  expect_equal(out$time_step, "daily")
  expect_equal(out$output_start_date, "2011-01-01")
})

test_that("setup_swat throws errors on invalid inputs", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  setwd(file.path(tmpdir, "TxtInOut"))

  expect_error(
    setup_swat("2015-01-01", "2010-01-01", "daily", 0),
    "must be earlier"
  )

  expect_error(
    setup_swat("2010-01-01", "2015-01-01", "hourly", 0),
    "time_step"
  )

  expect_error(
    setup_swat("2010-01-01", "2015-01-01", "daily", -1),
    "NYSKIP"
  )

  expect_error(
    setup_swat("2010-01-01", "2015-01-01", "daily", 0,
               rch_vars = "INVALID"),
    "Invalid variables"
  )
})

test_that("setup_swat fails outside TxtInOut directory", {
  skip_on_cran()

  tmpdir <- tempdir()
  setwd(tmpdir)

  expect_error(
    setup_swat("2010-01-01", "2015-01-01", "daily", 0),
    "file.cio"
  )
})


# -------------------------------------------------------------------------
# Tests for download_swat_exe
# -------------------------------------------------------------------------

test_that("download_swat_exe fails on non-Windows systems", {
  if (.Platform$OS.type != "windows") {
    expect_error(
      download_swat_exe(tempdir()),
      "only supported on Windows"
    )
  }
})


# -------------------------------------------------------------------------
# Tests for run_swat_exe
# -------------------------------------------------------------------------

test_that("run_swat_exe validates presence of file.cio", {
  skip_on_cran()

  tmpdir <- tempdir()
  setwd(tmpdir)

  expect_error(
    run_swat_exe("swat.exe"),
    "file.cio"
  )
})

test_that("run_swat_exe validates executable path", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  setwd(file.path(tmpdir, "TxtInOut"))

  expect_error(
    run_swat_exe("non_existing.exe"),
    "not found"
  )
})


# -------------------------------------------------------------------------
# Tests for run_swat
# -------------------------------------------------------------------------
# - execution_mode validation
# - argument validation for single_sample and multiple_sample
# - correct delegation to run_swat_exe in exe mode


test_that("run_swat validates execution_mode", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  expect_error(
    run_swat(
      TxtInOut_dir   = file.path(tmpdir, "TxtInOut"),
      swat_exe_path  = "swat.exe",
      execution_mode = "invalid"
    ),
    "execution_mode"
  )
})


test_that("run_swat delegates exe mode to run_swat_exe", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  # run_swat_exe should be called and fail on missing executable
  expect_error(
    run_swat(
      TxtInOut_dir   = file.path(tmpdir, "TxtInOut"),
      swat_exe_path  = "non_existing.exe",
      execution_mode = "exe"
    ),
    "not found"
  )
})


test_that("run_swat requires parameter_info in single_sample mode", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  expect_error(
    run_swat(
      TxtInOut_dir   = file.path(tmpdir, "TxtInOut"),
      swat_exe_path  = "swat.exe",
      execution_mode = "single_sample"
    ),
    "parameter_info"
  )
})




# -------------------------------------------------------------------------
# Tests for run_swat() — multiple_sample mode
# -------------------------------------------------------------------------
test_that("run_swat validates inputs in multiple_sample mode", {
  skip_on_cran()

  # ---- Common setup ----
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  create_calibration_project(
    swat_TxtInOut   = file.path(tmpdir, "TxtInOut"),
    destination_dir = tmpdir,
    project_name    = "calib",
    set_working_dir = FALSE
  )

  TxtInOut_calib <- file.path(tmpdir, "calib", "TxtInOut")

  # ---- Minimal valid output args ----
  output_rch_args <- list(
    file              = "output.rch",
    variable          = "FLOW_OUTcms",
    target_id         = 1,
    time_step         = "daily",
    output_start_date = "2011-01-01"
  )

  # ---- parameter_info (1 parameter) ----
  parameter_info_1 <- tibble::tibble(
    component = ".gw",
    parameter = "GW_DELAY",
    value     = 30,
    method    = "v"
  )

  # 1. parameter_sample required
  expect_error(
    run_swat(
      TxtInOut_dir   = TxtInOut_calib,
      swat_exe_path  = "swat.exe",
      execution_mode = "multiple_sample",
      parameter_info = parameter_info_1
    ),
    "parameter_sample"
  )

  # 2. output args required
  expect_error(
    run_swat(
      TxtInOut_dir      = TxtInOut_calib,
      swat_exe_path     = "swat.exe",
      execution_mode    = "multiple_sample",
      parameter_info    = parameter_info_1,
      parameter_sample  = tibble::tibble(GW_DELAY = 30)
    ),
    "provide at least one of"
  )

  # ---- parameter_info (2 parameters) ----
  parameter_info_2 <- tibble::tibble(
    component = c(".gw", ".mgt"),
    parameter = c("GW_DELAY", "CN2"),
    value     = c(30, 0.1),
    method    = c("v", "r")
  )

  # 3. parameter dimension mismatch
  expect_error(
    run_swat(
      TxtInOut_dir      = TxtInOut_calib,
      swat_exe_path     = "swat.exe",
      execution_mode    = "multiple_sample",
      parameter_info    = parameter_info_2,
      parameter_sample  = tibble::tibble(GW_DELAY = 30), # mismatch
      output_rch_args   = output_rch_args
    ),
    "Number of columns"
  )
})
