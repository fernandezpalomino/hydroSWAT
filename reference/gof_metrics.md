# Goodness-of-fit (GOF) metrics and FDC-based signatures

Computes a set of goodness-of-fit (GOF) metrics and flow duration curve
(FDC) signatures to evaluate model performance by comparing simulated
vs. observed values.

## Usage

``` r
gof_metrics(
  obs,
  sim,
  metric,
  minimize = FALSE,
  all_metrics = FALSE,
  digits = 4
)
```

## Arguments

- obs:

  numeric. Observed values.

- sim:

  numeric. Simulated values.

- metric:

  character. Name of the metric to compute. See Details for the list of
  available metrics.

- minimize:

  logical. Whether to transform the metric so that it can be used as a
  minimization objective in optimization-based model calibration
  workflows. Default: `FALSE`.

- all_metrics:

  logical. Whether to compute all available metrics. If `TRUE`, a tibble
  with all metrics is returned. Default: `FALSE`.

- digits:

  integer. Number of decimal places for rounding. Default: 4.

## Value

If `all_metrics = FALSE`, a numeric value for the requested metric. If
`all_metrics = TRUE`, a tibble with two columns:

- `gof`: Metric name.

- `value`: Metric value (rounded).

## Details

The following metrics are available. Parentheses indicate value domains
(e.g., -Inf to Inf), and brackets indicate exceedance probability
segments of FDC (e.g., `[-EP: 0–2%]`):

- `NSE`: Nash-Sutcliffe efficiency (-Inf to 1).

- `log_NSE`: Log-transformed NSE (-Inf to 1).

- `mNSE`: Modified NSE (-Inf to 1).

- `rNSE`: Relative NSE (-Inf to 1).

- `KGE`: Kling-Gupta efficiency (-Inf to 1).

- `KGElf`: KGE for low values (-Inf to 1).

- `KGEnp`: Non-parametric KGE (-Inf to 1).

- `KGEkm`: Knowable Moments KGE (-Inf to 1).

- `RMSE`: Root mean squared error (\>= 0).

- `TRMSE`: Box-Cox transformed RMSE (\>= 0).

- `MAE`: Mean absolute error (\>= 0).

- `PBIAS`: Percent bias (-Inf to Inf).

- `VE`: Volumetric efficiency (-Inf to 1).

- `RSR`: Ratio of RMSE to SD of observations (0 to +Inf).

- `rPearson`: Pearson correlation (-1 to 1).

- `rSpearman`: Spearman correlation (-1 to 1).

- `R2`: Coefficient of determination (0 to 1).

- `FDCsign`: Aggregated FDC signature (\>= 0).

- `FDC_Speak`: Percent bias in peak segment volume \[-EP: 0–2%\] (-Inf
  to Inf).

- `FDC_Shigh`: Percent bias in high segment volume \[-EP: 2–20%\] (-Inf
  to Inf).

- `FDC_Smid`: Percent bias in midsegment slope \[-EP: 20–70%\] (-Inf to
  Inf).

- `FDC_Slow`: Percent bias in low segment volume \[-EP: 70–100%\] (-Inf
  to Inf).

Metrics `TRMSE` and all `FDC*` signatures are computed using functions
from this package
([trmse](https://fernandezpalomino.github.io/hydroSWAT/reference/trmse.md)
and
[fdc_signature](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md)),
while the remaining metrics are computed with the hydroGOF package.

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md),
[`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md),
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

# Compute a single metric (NSE)
gof_metrics(obs, sim, "NSE")
#> [1] 0.4847

# Compute all metrics
gof_metrics(obs, sim, all_metrics = TRUE)
#> # A tibble: 22 × 2
#>    gof      value
#>    <chr>    <dbl>
#>  1 NSE      0.485
#>  2 log_NSE  0.823
#>  3 mNSE     0.309
#>  4 rNSE     0.560
#>  5 KGE      0.774
#>  6 KGElf    0.833
#>  7 KGEnp    0.815
#>  8 KGEkm    0.802
#>  9 RMSE    13.9  
#> 10 TRMSE    0.731
#> # ℹ 12 more rows
```
