# Percent bias in the midsegment slope of a Flow Duration Curve (FDC)

Computes the percent bias for the midsegment slope of the flow duration
curves (FDC) between simulated and observed flows, delimited by
exceedance probability (EP) thresholds, useful for evaluating model
performance in intermediate flow regimes.

## Usage

``` r
pbias_fdc_midsegment(
  sim,
  obs,
  lowQ_EP = 0.7,
  highQ_EP = 0.2,
  na.rm = TRUE,
  plot = FALSE,
  ...
)
```

## Arguments

- sim:

  numeric. Simulated flow values.

- obs:

  numeric. Observed flow values.

- lowQ_EP:

  numeric. Upper EP bound of the midsegment (e.g., 0.7 for 70%).

- highQ_EP:

  numeric. Lower EP bound of the midsegment (e.g., 0.2 for 20%).

- na.rm:

  logical. Whether to remove NA values before computation. Default:
  TRUE.

- plot:

  logical. Whether to generate a log-scale plot highlighting the
  selected segment. Default: FALSE.

- ...:

  additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

numeric. Percent bias for the FDC midsegment slope.

## Details

The EP thresholds must satisfy \\1 \ge lowQ\\EP \> highQ\\EP \ge 0\\.

Percent bias for the midsegment slope is calculated as: \$\$
pbiasFDC\_{\text{midsegment}} = \frac{\big\[(S\_{m1} - S\_{m2}) -
(O\_{m1} - O\_{m2})\big\]} {(O\_{m1} - O\_{m2})} \times 100 \$\$ where
\\S\_{m1}\\, \\S\_{m2}\\ and \\O\_{m1}\\, \\O\_{m2}\\ are the simulated
and observed flows at the EP thresholds defining the segment.

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md),
[`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md),
[`plot_mean_annual_cycle()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md),
[`plot_timeseries()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md)

## Examples

``` r
# Synthetic daily flow data
set.seed(123)
obs <- abs(rnorm(730, mean = 50, sd = 20))
sim <- obs * runif(730, min = 0.8, max = 1.5)

# Percent bias for midsegment slope (20–70% EP)
pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2)
#> [1] 27.82472

# Plot selected segment
pbias_fdc_midsegment(sim, obs, lowQ_EP = 0.7, highQ_EP = 0.2,
                     plot = TRUE)

#> [1] 27.82472
```
