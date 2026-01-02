# Plot mean annual cycle by evaluation periods

Generates a plot of the mean annual cycle for simulated and observed
values (e.g., streamflow) by evaluation periods. Includes an option to
adjust the starting month for the water year.

## Usage

``` r
plot_mean_annual_cycle(
  data,
  water_year = FALSE,
  water_year_start,
  x_label = "Month",
  y_label = "Q [m3/s]",
  show_legend = TRUE
)
```

## Arguments

- data:

  data frame. Mean monthly values of simulated and observed data,
  typically the `mean_annual_cycle` output from
  [evaluate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md).
  Must include columns: `source`, `period`, `month`, and `value`.

- water_year:

  logical. Whether the data should follow a water year (starting in a
  month other than January). Default: `FALSE`.

- water_year_start:

  numeric. The month number (1 to 12) that the water year starts.
  Required when `water_year = TRUE` and ignored when
  `water_year = FALSE`.

- x_label:

  character. Label for the x-axis. Default: "Month".

- y_label:

  character. Label for the y-axis. Default: "Q \[m3/s\]".

- show_legend:

  logical. Whether to display the legend. Default: `TRUE`.

## Value

A `ggplot` object representing the mean annual cycle plot.

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
[`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md),
[`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md),
[`plot_timeseries()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md)

## Examples

``` r
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpitMhWQ/TxtInOut
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

# Plot mean annual cycle by evaluation periods
plot_mean_annual_cycle(evaluation_results$mean_annual_cycle)
```
