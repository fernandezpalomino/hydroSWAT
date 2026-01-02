# Plot time series of simulated and observed data

Generates a time series plot comparing simulated and observed values
(e.g., streamflow) from the outputs of
[evaluate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md).

## Usage

``` r
plot_timeseries(
  data,
  x_label = "Time",
  y_label = "Q [m3/s]",
  show_legend = TRUE
)
```

## Arguments

- data:

  data frame. Time series of simulated and observed values, typically
  one of the outputs from
  [evaluate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md)
  (daily_data, monthly_data, or annual_data).

- x_label:

  character. Label for the x-axis. Default is "Time".

- y_label:

  character. Label for the y-axis. Default is "Q \[m3/s\]".

- show_legend:

  logical. Whether to display the legend. Default is TRUE.

## Value

A ggplot object representing the time series plot of observed and
simulated values.

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
[`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md),
[`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md),
[`plot_mean_annual_cycle()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md)

## Examples

``` r
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpUu15Uv/TxtInOut
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

# Plot daily time series of observed vs simulated flow
plot_timeseries(evaluation_results$daily_data)
```
