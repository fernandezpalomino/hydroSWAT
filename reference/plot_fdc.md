# Plot Flow Duration Curve (FDC) by evaluation periods

Generates a Flow Duration Curve (FDC) comparing simulated and observed
values (e.g., streamflow), grouped by evaluation periods.

## Usage

``` r
plot_fdc(
  data,
  exceedance_thresholds = NULL,
  x_label = "% of time flow is equaled or exceeded",
  y_label = "Q [m3/s]",
  epsilon_value = 0.01,
  show_legend = TRUE
)
```

## Arguments

- data:

  data frame. Daily flow values of simulated and observed data,
  typically the `daily_fdc_data` output from
  [evaluate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md).
  Must include columns `date`, `source`, `period`, and `value`.

- exceedance_thresholds:

  numeric vector. Exceedance probability thresholds (e.g.,
  `c(0, 0.02, 0.2, 0.75, 1)`) for vertical reference lines. Default:
  `NULL` (no lines added).

- x_label:

  character. Label for the x-axis. Default: "% of time flow is equaled
  or exceeded".

- y_label:

  character. Label for the y-axis. Default: "Q \[m3/s\]".

- epsilon_value:

  numeric. Value added to flows before log transformation to avoid
  issues with zeros. Default: `0.01`.

- show_legend:

  logical. Whether to display the legend. Default: `TRUE`.

## Value

A `ggplot` object visualizing the Flow Duration Curve (FDC).

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
[`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md),
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

# Plot Flow Duration Curve
plot_fdc(evaluation_results$daily_fdc_data,
         exceedance_thresholds = c(0, 0.02, 0.2, 0.75, 1))
```
