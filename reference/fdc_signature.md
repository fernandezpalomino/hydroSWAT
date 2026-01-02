# Flow Duration Curve (FDC) signature

Computes `FDCsign`, an aggregated index combining four percent-bias
metrics for distinct flow regimes (peak, high, intermediate, and low),
summarizing model performance across the flow regime spectrum. See
Details for its calculation and component definitions.

## Usage

``` r
fdc_signature(sim, obs)
```

## Arguments

- sim:

  numeric. Simulated flow values.

- obs:

  numeric. Observed flow values.

## Value

A tibble with five columns: `Speak`, `Shigh`, `Smid`, `Slow` (all
expressed as percent bias), and `FDCsign` (aggregated index, unitless).

## Details

The index `FDCsign` is computed as: \$\$ FDCsign = 0.25 \times \big(
\|Speak/100\| + \|Shigh/100\| + \|Smid/100\| + \|Slow/100\| \big) \$\$
where each component is defined as:

- `Speak`: Percent bias in the FDC peak segment volume (EP: 0–2%),
  computed with
  [pbias_fdc_target_segment](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md).

- `Shigh`: Percent bias in the FDC high segment volume (EP: 2–20%),
  computed with
  [pbias_fdc_target_segment](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md).

- `Smid`: Percent bias in the FDC midsegment slope (EP: 20–70%),
  computed with
  [pbias_fdc_midsegment](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md).

- `Slow`: Percent bias in the FDC low segment volume (EP: 70–100%),
  computed with
  [pbias_fdc_target_segment](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md).

## See also

Other Performance evaluation:
[`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md),
[`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md),
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

# Compute FDC signature
fdc_signature(sim, obs)
#> # A tibble: 1 × 5
#>   Speak Shigh  Smid  Slow FDCsign
#>   <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1  29.6  23.2  27.8  9.23   0.225
```
