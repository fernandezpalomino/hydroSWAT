# Evaluate SWAT model performance

Evaluates SWAT model performance by comparing simulated and observed
data using user-selected or all available goodness-of-fit (GOF) metrics
at daily, monthly, and annual time steps.

## Usage

``` r
evaluate_swat(
  rch_output,
  observed_data,
  target_id,
  variable,
  metrics,
  start_dates,
  end_dates,
  all_metrics = FALSE
)
```

## Arguments

- rch_output:

  data frame. SWAT model output for one or more reaches, typically
  generated with
  [output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md).
  Must include columns RCH (reach ID), MON (dates), and the simulated
  variable of interest (e.g., FLOW_OUTcms).

- observed_data:

  data frame. Observed data for one or more reaches, including columns
  Date (observation date), value (observed values), and target_id (reach
  ID).

- target_id:

  numeric. Reach ID to evaluate.

- variable:

  character. Name of the variable to evaluate (e.g., "FLOW_OUTcms"). Use
  [get_swat_vars](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md)("rch")
  to list available variables.

- metrics:

  character vector. Goodness-of-fit metrics to compute (e.g.,
  c("NSE","KGE","MAE","PBIAS")).

- start_dates:

  character vector. Start dates for evaluation periods in "YYYY-MM-DD"
  format.

- end_dates:

  character vector. End dates for evaluation periods in "YYYY-MM-DD"
  format. Must match length of start_dates.

- all_metrics:

  logical. Whether to calculate all available metrics. Defaults to
  FALSE.

## Value

A list of data frames:

- evaluation_metric: Evaluation metrics for daily, monthly, and annual
  time steps. Daily metrics are included only if daily data are
  available.

- daily_data: Daily observed and simulated values (if available).

- monthly_data: Monthly observed and simulated values with the same
  structure as daily_data.

- annual_data: Annual observed and simulated values with the same
  structure as daily_data. These three data frames (daily_data,
  monthly_data, annual_data) are designed for use with
  [plot_timeseries](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md).

- mean_annual_cycle: Mean annual cycle of observed and simulated values.
  Columns: source, period, month, value. This object is used by
  [plot_mean_annual_cycle](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md).

- daily_fdc_data: Daily data for flow duration curves (FDC). Columns:
  source, period, date, value. This object is used by
  [plot_fdc](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md).

## See also

Other Performance evaluation:
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
[`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md),
[`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md),
[`plot_mean_annual_cycle()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md),
[`plot_timeseries()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md)

## Examples

``` r
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpWxZ4hT/TxtInOut
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
  metrics = c("NSE","KGE","MAE","PBIAS"),
  start_dates = c("2011-01-01","2014-01-01"),
  end_dates = c("2013-12-31","2015-12-31")
)
head(evaluation_results$evaluation_metric)
#> # A tibble: 6 × 9
#>   variable target_id time_step start_date end_date        NSE    KGE   MAE PBIAS
#>   <chr>        <dbl> <chr>     <date>     <date>        <dbl>  <dbl> <dbl> <dbl>
#> 1 FLOW_OU…         3 daily     2011-01-01 2013-12-31    0.637  0.751  8.61  -7  
#> 2 FLOW_OU…         3 daily     2014-01-01 2015-12-31    0.582  0.635  9.41 -31.2
#> 3 FLOW_OU…         3 monthly   2011-01-01 2013-12-31    0.804  0.839  5.78  -7.3
#> 4 FLOW_OU…         3 monthly   2014-01-01 2015-12-31    0.724  0.679  7.22 -31.4
#> 5 FLOW_OU…         3 annual    2011-01-01 2013-12-31    0.710  0.867  1.85 -10  
#> 6 FLOW_OU…         3 annual    2014-01-01 2015-12-31 -434.    -2.95   6.50 -31.4
```
