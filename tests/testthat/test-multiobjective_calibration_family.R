# =============================================================================
# Family: Multiobjective calibration
#
# These tests focus on:
#  - calibration project creation
#  - early input validation (fail-fast behavior)
#  - lightweight helpers used in parallel workflows
#  - pure algorithmic utilities (no SWAT execution)
#
# Heavy SWAT / NSGA-II execution is intentionally avoided.
# =============================================================================

skip_on_cran()

# -----------------------------------------------------------------------------
# Common setup helper for calibration-related tests
# -----------------------------------------------------------------------------
# Creates a minimal, valid SWAT calibration project in a temporary directory.
# Returned paths are reused across tests to avoid duplication.
# -----------------------------------------------------------------------------

setup_calibration_project <- function() {

  original_wd <- getwd()
  base_tmpdir <- tempdir()

  # Write example SWAT project shipped with the package
  get_swat_example(base_tmpdir)

  # Create a calibration project (adds Backup folder)
  create_calibration_project(
    swat_TxtInOut   = file.path(base_tmpdir, "TxtInOut"),
    destination_dir = base_tmpdir,
    project_name    = "calib_project",
    set_working_dir = FALSE
  )

  list(
    original_wd  = original_wd,
    base_tmpdir  = base_tmpdir,
    txtinout_dir = file.path(base_tmpdir, "calib_project", "TxtInOut")
  )
}

# -----------------------------------------------------------------------------
# create_calibration_project
# -----------------------------------------------------------------------------

test_that("create_calibration_project creates a valid calibration directory structure", {

  project <- setup_calibration_project()
  on.exit(setwd(project$original_wd), add = TRUE)

  # Main TxtInOut directory exists
  expect_true(dir.exists(project$txtinout_dir))

  # Backup folder is created for safe restoration of inputs
  expect_true(dir.exists(file.path(project$txtinout_dir, "Backup")))

  # Presence of file.cio confirms a valid SWAT project
  expect_true(file.exists(file.path(project$txtinout_dir, "file.cio")))
})

test_that("create_calibration_project fails early on invalid input paths", {

  # Non-existing path
  expect_error(
    create_calibration_project(
      swat_TxtInOut = "non_existing_path",
      project_name  = "x"
    ),
    "path does not exist"
  )

  # Existing directory that is not a SWAT project
  tmpdir <- tempdir()
  dir.create(tmpdir, showWarnings = FALSE)

  expect_error(
    create_calibration_project(
      swat_TxtInOut = tmpdir,
      project_name  = "x"
    ),
    "is not a SWAT project"
  )
})

# -----------------------------------------------------------------------------
# fn_args validators
# -----------------------------------------------------------------------------
# These tests verify that invalid calibration configurations are rejected
# before any SWAT execution or NSGA-II iteration starts.
# -----------------------------------------------------------------------------

test_that("validate_fn_args rejects completely missing calibration inputs", {

  # Empty list: none of the required entries are present
  expect_error(
    validate_fn_args(list()),
    "Missing required calibration data inputs"
  )
})

test_that("validate_fn_args rejects empty objectives list explicitly", {

  # All required entries are present, but objectives is empty
  fn_args_with_empty_objectives <- list(
    objectives     = list(),  # <- invalid by design
    observed_data  = list(daily = list()),
    output_config  = list(),
    parameter_info = tibble::tibble(
      component = ".gw",
      parameter = "GW_DELAY",
      value     = NA,
      method    = "v"
    ),
    subbasins      = NULL,
    swat_exe_path  = tempfile()
  )

  # Failure must be caused by objectives validation, not by parameter_info
  expect_error(
    validate_fn_args(fn_args_with_empty_objectives),
    "non-empty list"
  )
})

test_that("validate_fn_args rejects malformed parameter_info table", {

  # Minimal valid observed data to reach parameter_info validation
  valid_observed_data <- list(
    daily = list(
      rch = list(
        FLOW_OUTcms = tibble::tibble(
          Date      = as.Date("2010-01-01"),
          value     = 1,
          target_id = 1
        )
      )
    )
  )

  fn_args_with_invalid_parameters <- list(
    objectives = list(
      list(
        element          = "rch",
        variable         = "FLOW_OUTcms",
        target_id        = 1,
        metric           = "NSE",
        calib_time_step  = "daily",
        calib_start_date = "2010-01-01",
        calib_end_date   = "2011-01-01"
      )
    ),
    observed_data  = valid_observed_data,
    output_config  = list(
      rch = list(
        file              = "output.rch",
        variable          = "FLOW_OUTcms",
        target_id         = 1,
        time_step         = "daily",
        output_start_date = "2010-01-01"
      )
    ),
    # alformed on purpose
    parameter_info = tibble::tibble(
      component = ".gw",
      parameter = "GW_DELAY"
    ),
    subbasins     = NULL,
    swat_exe_path = tempfile()
  )

  expect_error(
    validate_fn_args(fn_args_with_invalid_parameters),
    "Missing required column"
  )
})


# -----------------------------------------------------------------------------
# create_core_working_dirs (parallel helper)
# -----------------------------------------------------------------------------
# These helpers are used internally when running NSGA-II in parallel.
# -----------------------------------------------------------------------------

test_that("create_core_working_dirs creates one independent working directory per core", {

  project <- setup_calibration_project()

  cores <- 2
  core_dirs <- create_core_working_dirs(
    cores        = cores,
    TxtInOut_dir = project$txtinout_dir
  )

  # One directory per core
  expect_length(core_dirs, cores)

  # All directories exist and contain a valid SWAT project
  expect_true(all(dir.exists(core_dirs)))
  expect_true(all(file.exists(file.path(core_dirs, "file.cio"))))
})

test_that("create_core_working_dirs validates its inputs strictly", {

  # Invalid number of cores
  expect_error(
    create_core_working_dirs(cores = 0, TxtInOut_dir = tempdir()),
    "positive integer"
  )

  # Non-existing TxtInOut directory
  expect_error(
    create_core_working_dirs(cores = 2, TxtInOut_dir = "nope"),
    "does not exist"
  )
})

# -----------------------------------------------------------------------------
# calibrate_swat (validation only)
# -----------------------------------------------------------------------------
# These tests ensure invalid configurations are rejected before optimization.
# -----------------------------------------------------------------------------

test_that("calibrate_swat rejects cores greater than population size", {

  project <- setup_calibration_project()

  expect_error(
    calibrate_swat(
      fn           = function(x, ...) c(1, 1),
      varNo        = 2,
      objDim       = 2,
      lowerBounds  = c(0, 0),
      upperBounds  = c(1, 1),
      popSize      = 2,
      cores        = 4,  # invalid: more cores than population
      TxtInOut_dir = project$txtinout_dir,
      fn_args      = list()
    ),
    "cores.*popSize"
  )
})

# -----------------------------------------------------------------------------
# best_compromise_solution (pure algorithmic utility)
# -----------------------------------------------------------------------------
# This function is independent of SWAT and NSGA-II execution.
# -----------------------------------------------------------------------------

test_that("best_compromise_solution selects a valid Pareto solution", {

  pareto_solutions <- matrix(
    c(0.20, 0.10,
      0.15, 0.20,
      0.10, 0.30),
    ncol = 2,
    byrow = TRUE
  )

  result <- best_compromise_solution(pareto_solutions)

  expect_type(result, "list")
  expect_named(result, c("objectives", "index"))

  # Returned objective vector has correct dimensionality
  expect_length(result$objectives, ncol(pareto_solutions))

  # Index refers to a valid Pareto solution
  expect_true(result$index %in% seq_len(nrow(pareto_solutions)))
})

test_that("best_compromise_solution validates its inputs strictly", {

  expect_error(
    best_compromise_solution("not_a_matrix"),
    "must be a matrix"
  )

  expect_error(
    best_compromise_solution(matrix(c(1, NA), ncol = 1)),
    "must not contain missing values"
  )
})
