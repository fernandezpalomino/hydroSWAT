# -------------------------------------------------------------------------
# Tests for Performance evaluation family
# -------------------------------------------------------------------------
# This file tests:
# - evaluate_swat()
# - gof_metrics()
# - trmse()
# - fdc_signature()
# - pbias_fdc_midsegment()
# - pbias_fdc_target_segment()
# - plot_timeseries()
# - plot_mean_annual_cycle()
# - plot_fdc()
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Tests for evaluate_swat
# -------------------------------------------------------------------------

test_that("evaluate_swat works as expected", {
  skip_on_cran() # Skip on CRAN because it writes files to disk

  # --- Setup temporary SWAT example project ---
  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")

  # --- Read SWAT reach output ---
  rch_data <- output_rch(
    file = rch_file,
    variable = "FLOW_OUTcms",
    target_id = 3,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  # --- Create observed data ---
  observed_data <- tibble::tibble(
    Date = qobserved$Date,
    value = qobserved$Flow,
    target_id = 3
  )

  # --- Run evaluation ---
  evaluation_results <- evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE","KGE","MAE","PBIAS"),
    start_dates = c("2011-01-01","2014-01-01"),
    end_dates = c("2013-12-31","2015-12-31")
  )

  # --- Validate returned object is a list ---
  expect_type(evaluation_results, "list")

  # --- Validate types of returned objects ---
  expect_s3_class(evaluation_results$evaluation_metric, "data.frame")
  expect_s3_class(evaluation_results$daily_data, "data.frame")
  expect_s3_class(evaluation_results$monthly_data, "data.frame")
  expect_s3_class(evaluation_results$annual_data, "data.frame")
  expect_s3_class(evaluation_results$mean_annual_cycle, "data.frame")
  expect_s3_class(evaluation_results$daily_fdc_data, "data.frame")

  # --- Validate key columns in evaluation_metric ---
  expect_gt(nrow(evaluation_results$evaluation_metric), 0)
  expect_true(all(c("variable", "target_id", "time_step",
                    "start_date", "end_date") %in%
                    colnames(evaluation_results$evaluation_metric)))
  # --- Validate metrics are present as columns ---
  expect_true(all(c("NSE","KGE","MAE","PBIAS") %in%
                    colnames(evaluation_results$evaluation_metric)))

  # --- Validate daily_data columns (if available) ---
  if (!is.null(evaluation_results$daily_data)) {
    expect_true(all(c("source", "date", "value") %in%
                      colnames(evaluation_results$daily_data)))
  }

  # --- Validate monthly_data columns ---
  expect_true(all(c("source", "date", "value") %in%
                    colnames(evaluation_results$monthly_data)))

  # --- Validate annual_data columns ---
  expect_true(all(c("source", "date", "value") %in%
                    colnames(evaluation_results$annual_data)))

  # --- Validate mean_annual_cycle columns ---
  expect_true(all(c("source", "period", "month", "value") %in%
                    colnames(evaluation_results$mean_annual_cycle)))

  # --- Validate daily_fdc_data columns ---
  expect_true(all(c("source", "period", "date", "value") %in%
                    colnames(evaluation_results$daily_fdc_data)))
})



# Additional error tests for evaluate_swat
test_that("evaluate_swat handles invalid inputs", {
  skip_on_cran()

  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")

  rch_data <- output_rch(
    file = rch_file,
    variable = "FLOW_OUTcms",
    target_id = 3,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  observed_data <- tibble::tibble(
    Date = qobserved$Date,
    value = qobserved$Flow,
    target_id = 3
  )

  # --- Non-data frame rch_output ---
  expect_error(evaluate_swat(
    rch_output = "not_a_df",
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE"),
    start_dates = "2011-01-01",
    end_dates = "2013-12-31"
  ), "`rch_output` must be a data frame")

  # --- Invalid variable ---
  expect_error(evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "INVALID_VAR",
    metrics = c("NSE"),
    start_dates = "2011-01-01",
    end_dates = "2013-12-31"
  ), "`variable` must be one of the following available variables")

  # --- Missing columns in rch_output ---
  rch_bad <- rch_data[, !names(rch_data) %in% "FLOW_OUTcms"]
  expect_error(evaluate_swat(
    rch_output = rch_bad,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE"),
    start_dates = "2011-01-01",
    end_dates = "2013-12-31"
  ), "`rch_output` is missing the following required columns")

  # --- Invalid metrics ---
  expect_error(evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("INVALID_METRIC"),
    start_dates = "2011-01-01",
    end_dates = "2013-12-31"
  ), "`metrics` contains invalid metric")

  # --- start_dates and end_dates length mismatch ---
  expect_error(evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE"),
    start_dates = c("2011-01-01","2012-01-01"),
    end_dates = "2013-12-31"
  ), "`start_dates` and `end_dates` must have the same length")
})


# -------------------------------------------------------------------------
# Tests for gof_metrics
# -------------------------------------------------------------------------
test_that("gof_metrics computes single metric correctly", {
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.1  # introduce bias

  # Single metric: NSE
  val <- gof_metrics(obs, sim, "NSE")
  expect_type(val, "double")
  expect_true(val <= 1)  # NSE must be <=1

  # Single metric with minimization
  val_min <- gof_metrics(obs, sim, "NSE", minimize = TRUE)
  expect_type(val_min, "double")
  expect_true(val_min >= 0)
})

test_that("gof_metrics computes all metrics as tibble", {
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * runif(100, min = 0.8, max = 1.5)

  val <- gof_metrics(obs, sim, all_metrics = TRUE)
  expect_s3_class(val, "tbl_df")
  expect_named(val, c("gof", "value"))
  expect_true("NSE" %in% val$gof)
  expect_true("FDCsign" %in% val$gof)
})

test_that("gof_metrics handles NA values properly", {
  set.seed(123)
  obs <- abs(rnorm(50, mean = 50, sd = 10))
  sim <- obs * 1.05
  obs[c(1,5,10)] <- NA
  sim[c(2,6,11)] <- NA

  val <- gof_metrics(obs, sim, "KGE")
  expect_type(val, "double")
})

test_that("gof_metrics throws errors on invalid inputs", {
  obs <- c(10, 20, 30)
  sim <- c(15, 25)  # different length
  expect_error(gof_metrics(obs, sim, "NSE"))

  obs <- "not numeric"
  sim <- 1:10
  expect_error(gof_metrics(obs, sim, "NSE"))
})


# -------------------------------------------------------------------------
# Tests for trmse
# -------------------------------------------------------------------------

test_that("trmse computes value correctly", {
  set.seed(123)
  obs <- abs(rnorm(50, mean = 10, sd = 5))
  sim <- obs * 1.1
  val <- trmse(sim, obs)
  expect_type(val, "double")
  expect_true(val >= 0)
})

test_that("trmse handles NA values", {
  sim <- c(1, 2, 3, NA, 5)
  obs <- c(1, 2.1, 3.1, 4, NA)
  val <- trmse(sim, obs)
  expect_type(val, "double")
})

test_that("trmse throws errors on invalid inputs", {
  obs <- c(1, 2, 3)
  sim <- c(1, 2)
  expect_error(trmse(sim, obs))

  obs <- 1:5
  sim <- 1:5
  expect_error(trmse(sim, obs, lambda = -0.5))
})


# -------------------------------------------------------------------------
# Tests for fdc_signature
# -------------------------------------------------------------------------
test_that("fdc_signature returns correct structure", {
  # Check tibble structure and column types
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.1

  result <- fdc_signature(sim, obs)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("Speak", "Shigh", "Smid", "Slow", "FDCsign"))
  expect_equal(ncol(result), 5)
  expect_true(all(sapply(result, is.numeric)))
})

test_that("fdc_signature handles NA correctly", {
  # Add NAs and verify tibble still returned
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.1
  obs[c(5, 20, 50)] <- NA
  sim[c(10, 25, 70)] <- NA

  result <- fdc_signature(sim, obs)
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 5)
})


# -------------------------------------------------------------------------
# Tests for pbias_fdc_midsegment
# -------------------------------------------------------------------------
test_that("pbias_fdc_midsegment works with synthetic data", {
  # Generate synthetic data with 20% bias
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.2

  val <- pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2)
  expect_type(val, "double")
  expect_true(val > 0)
})

test_that("pbias_fdc_midsegment handles NA and errors", {
  # Add some NAs to test na.rm
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.2
  obs[c(5, 20, 50)] <- NA
  sim[c(10, 25, 70)] <- NA

  # NA removal works
  val <- pbias_fdc_midsegment(
    sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2, na.rm = TRUE
  )
  expect_type(val, "double")

  # Error when all values are NA
  expect_error(
    pbias_fdc_midsegment(
      rep(NA, 100), rep(NA, 100), lowQ_EP = 0.7, highQ_EP = 0.2
    )
  )
})

test_that("pbias_fdc_midsegment throws errors on invalid thresholds", {
  # Test invalid EP thresholds
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs

  expect_error(
    pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.7)
  )
})


# -------------------------------------------------------------------------
# Tests for pbias_fdc_target_segment
# -------------------------------------------------------------------------
test_that("pbias_fdc_target_segment works with synthetic data", {
  # Generate synthetic data with 10% bias
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.1

  val <- pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0)
  expect_type(val, "double")
  expect_true(val > 0)  # Expect positive bias
})

test_that("pbias_fdc_target_segment handles NA and errors", {
  # Add some NAs to test na.rm
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs * 1.1
  obs[c(5, 20, 50)] <- NA
  sim[c(10, 25, 70)] <- NA

  # NA removal works
  val <- pbias_fdc_target_segment(
    sim, obs, lowQ_EP = 0.5, highQ_EP = 0.1, na.rm = TRUE
  )
  expect_type(val, "double")

  # Error when all values are NA
  expect_error(
    pbias_fdc_target_segment(
      rep(NA, 100), rep(NA, 100), lowQ_EP = 0.5, highQ_EP = 0.1
    )
  )
})

test_that("pbias_fdc_target_segment throws error for invalid thresholds", {
  # Test invalid EP thresholds
  set.seed(123)
  obs <- abs(rnorm(100, mean = 50, sd = 10))
  sim <- obs

  expect_error(
    pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.1, highQ_EP = 0.5)
  )
  expect_error(
    pbias_fdc_target_segment(sim, obs, lowQ_EP = 1.5, highQ_EP = 0.2)
  )
})


# -------------------------------------------------------------------------
# Tests for plot_timeseries
# -------------------------------------------------------------------------
test_that("plot_timeseries works as in example", {
  skip_on_cran()
  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
  rch_data <- output_rch(
    file = rch_file, variable = "FLOW_OUTcms",
    target_id = 3, time_step = "daily",
    output_start_date = "2011-01-01"
  )
  observed_data <- tibble::tibble(
    Date = qobserved$Date,
    value = qobserved$Flow,
    target_id = 3
  )
  evaluation_results <- evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE", "KGE", "MAE", "PBIAS"),
    start_dates = c("2011-01-01", "2014-01-01"),
    end_dates = c("2013-12-31", "2015-12-31")
  )
  p <- plot_timeseries(evaluation_results$daily_data)
  expect_s3_class(p, "ggplot")
})

# -------------------------------------------------------------------------
# Tests for plot_mean_annual_cycle
# -------------------------------------------------------------------------
test_that("plot_mean_annual_cycle works as in example", {
  skip_on_cran()
  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
  rch_data <- output_rch(
    file = rch_file, variable = "FLOW_OUTcms",
    target_id = 3, time_step = "daily",
    output_start_date = "2011-01-01"
  )
  observed_data <- tibble::tibble(
    Date = qobserved$Date,
    value = qobserved$Flow,
    target_id = 3
  )
  evaluation_results <- evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE", "KGE", "MAE", "PBIAS"),
    start_dates = c("2011-01-01", "2014-01-01"),
    end_dates = c("2013-12-31", "2015-12-31")
  )
  p <- plot_mean_annual_cycle(evaluation_results$mean_annual_cycle)
  expect_s3_class(p, "ggplot")
})

# -------------------------------------------------------------------------
# Tests for plot_fdc
# -------------------------------------------------------------------------
test_that("plot_fdc works as in example", {
  skip_on_cran()
  tmpdir <- tempdir()
  get_swat_example(tmpdir)
  rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
  rch_data <- output_rch(
    file = rch_file, variable = "FLOW_OUTcms",
    target_id = 3, time_step = "daily",
    output_start_date = "2011-01-01"
  )
  observed_data <- tibble::tibble(
    Date = qobserved$Date,
    value = qobserved$Flow,
    target_id = 3
  )
  evaluation_results <- evaluate_swat(
    rch_output = rch_data,
    observed_data = observed_data,
    target_id = 3,
    variable = "FLOW_OUTcms",
    metrics = c("NSE", "KGE", "MAE", "PBIAS"),
    start_dates = c("2011-01-01", "2014-01-01"),
    end_dates = c("2013-12-31", "2015-12-31")
  )
  p <- plot_fdc(
    evaluation_results$daily_fdc_data,
    exceedance_thresholds = c(0, 0.02, 0.2, 0.75, 1)
  )
  expect_s3_class(p, "ggplot")
})
