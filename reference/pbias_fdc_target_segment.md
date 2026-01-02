# Percent bias in a Flow Duration Curve (FDC) segment

Computes the percent bias between simulated and observed flows in a
specified segment of their flow duration curves (FDC), delimited by
exceedance probability (EP) thresholds. Both curves are interpolated
onto a common EP grid (defined by \`resolution\`), ensuring that
simulated and observed flows are compared at exactly the same
probabilities. Facilitates evaluation of model performance across flow
regimes (e.g., peak, high, or low flows).

## Usage

``` r
pbias_fdc_target_segment(
  sim,
  obs,
  lowQ_EP,
  highQ_EP,
  resolution = 1e-04,
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

  numeric. Upper EP bound of the segment (e.g., 0.2 for 20%).

- highQ_EP:

  numeric. Lower EP bound of the segment (e.g., 0 for 0%).

- resolution:

  numeric. Resolution for EP interpolation, defining the common grid
  where simulated and observed flows are compared. Default: 0.0001.

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

numeric. Percent bias for the specified FDC segment.

## Details

The EP thresholds must satisfy \\1 \ge lowQ\\EP \> highQ\\EP \ge 0\\.

Percent bias is calculated as: \$\$ pbiasFDC\_{\text{segment}} =
\frac{\sum\_{p=1}^P (S_p - O_p)}{\sum\_{p=1}^P O_p} \times 100 \$\$
where \\S_p\\ and \\O_p\\ are simulated and observed flows at EP \\p\\
on the interpolated grid.

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
[`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md),
[`plot_mean_annual_cycle()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md),
[`plot_timeseries()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md)

## Examples

``` r
# Synthetic daily flow data
set.seed(123)
obs <- abs(rnorm(730, mean = 50, sd = 20))
sim <- obs * runif(730, min = 0.8, max = 1.5)

# Percent bias for peak flows (0–2% EP)
pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.02, highQ_EP = 0)
#> [1] 29.64331

# Percent bias for high flows (2–20% EP)
pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.02)
#> [1] 23.18478

# Percent bias for low flows (70–100% EP)
pbias_fdc_target_segment(sim, obs, lowQ_EP = 1, highQ_EP = 0.7)
#> [1] 9.227436

# Plot selected segment
pbias_fdc_target_segment(sim, obs, lowQ_EP = 0.2, highQ_EP = 0.02,
                         plot = TRUE)

#> [1] 23.18478
```
