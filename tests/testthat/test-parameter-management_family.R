# =============================================================================
# Family: Parameter management
# SWAT parameter querying and editing
# - get_swat_parameters()
# - change_parameter()
# - change_multiple_parameters()
# - split_swat_parameters()
# =============================================================================

test_that("Parameter management workflow works as expected", {

  skip_on_cran()

  # ---------------------------------------------------------------------------
  # Common setup
  # ---------------------------------------------------------------------------
  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  old_wd <- getwd()
  setwd(file.path(tmpdir, "TxtInOut"))
  on.exit(setwd(old_wd), add = TRUE)

  # ---------------------------------------------------------------------------
  # get_swat_parameters()
  # ---------------------------------------------------------------------------
  params <- get_swat_parameters()

  expect_s3_class(params, "tbl_df")
  expect_true(all(c(
    "component", "parameter", "line", "start", "stop", "format", "description"
  ) %in% names(params)))

  expect_true(any(params$component == ".gw"))
  expect_true(any(params$parameter == "GW_DELAY"))

  expect_error(
    get_swat_parameters(version = "INVALID"),
    "should be one of"
  )

  # ---------------------------------------------------------------------------
  # change_parameter(): basic validation
  # ---------------------------------------------------------------------------
  expect_error(
    change_parameter(
      parameter = "GW_DELAY",
      value = 30,
      method = "v"
    ),
    "component"
  )

  expect_error(
    change_parameter(
      component = ".gw",
      value = 30,
      method = "v"
    ),
    "parameter"
  )

  expect_error(
    change_parameter(
      component = ".gw",
      parameter = "GW_DELAY",
      value = "a",
      method = "v"
    ),
    "numeric"
  )

  expect_error(
    change_parameter(
      component = ".gw",
      parameter = "GW_DELAY",
      value = 2,
      method = "r"
    ),
    "between -1 and 1"
  )

  # ---------------------------------------------------------------------------
  # change_parameter(): successful execution (global)
  # ---------------------------------------------------------------------------
  out <- change_parameter(
    component = ".gw",
    parameter = "GW_DELAY",
    value     = 30,
    method    = "v"
  )

  expect_s3_class(out, "changed_parameter")
  expect_equal(out$component, ".gw")
  expect_equal(out$parameter, "GW_DELAY")
  expect_true(length(out$files) > 0)

  # ---------------------------------------------------------------------------
  # change_parameter(): subbasin filtering
  # ---------------------------------------------------------------------------
  out_sub <- change_parameter(
    subbasins = 1,
    component = ".mgt",
    parameter = "CN2",
    value     = 0.1,
    method    = "r"
  )

  expect_s3_class(out_sub, "changed_parameter")
  expect_true(all(substr(out_sub$files, 1, 5) == "00001"))

  expect_error(
    change_parameter(
      subbasins = "A",
      component = ".mgt",
      parameter = "CN2",
      value     = 0.1,
      method    = "r"
    ),
    "numeric vector"
  )

  # ---------------------------------------------------------------------------
  # change_parameter(): special components
  # ---------------------------------------------------------------------------
  expect_error(
    change_parameter(
      component = ".wgn",
      parameter = "TMPMX",
      value     = 10,
      method    = "v"
    ),
    "method must be 'r'"
  )

  expect_error(
    change_parameter(
      component = "plant.dat",
      parameter = "BIO_E",
      value     = 5,
      method    = "v"
    ),
    "plant_type"
  )

  # ---------------------------------------------------------------------------
  # change_multiple_parameters()
  # ---------------------------------------------------------------------------
  param_info <- tibble::tibble(
    component = c(".gw", ".mgt"),
    parameter = c("GW_DELAY", "CN2"),
    value     = c(20, 0.05),
    method    = c("v", "r")
  )

  res_multi <- change_multiple_parameters(param_info)

  expect_type(res_multi, "list")
  expect_length(res_multi, 2)
  expect_true(all(vapply(res_multi, inherits, logical(1), "changed_parameter")))

  expect_error(
    change_multiple_parameters("not_a_df"),
    "must be a data frame"
  )

  expect_error(
    change_multiple_parameters(
      tibble::tibble(component = ".gw")
    ),
    "Missing required columns"
  )

  # ---------------------------------------------------------------------------
  # split_swat_parameters()
  # ---------------------------------------------------------------------------
  sample <- data.frame(
    GW_DELAY = 1:10,
    CN2      = 11:20
  )

  splits <- split_swat_parameters(cores = 3, parameter_sample = sample)

  expect_type(splits, "list")
  expect_length(splits, 3)
  expect_equal(sum(vapply(splits, nrow, integer(1))), nrow(sample))
  expect_equal(do.call(rbind, splits), sample)

})
