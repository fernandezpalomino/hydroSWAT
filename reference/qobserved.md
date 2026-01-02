# Observed streamflow (daily)

Daily discharge at the outlet gauge of the example basin.

## Usage

``` r
qobserved
```

## Format

Tibble/data.frame with:

- Date:

  Date (daily).

- Flow:

  Discharge (m^3/s).

## Source

National Meteorology and Hydrology Service of Peru (SENAMHI).

## See also

Other Data:
[`example_basin_datasets`](https://fernandezpalomino.github.io/hydroSWAT/reference/example_basin_datasets.md),
[`gridded_climate_netcdfs`](https://fernandezpalomino.github.io/hydroSWAT/reference/gridded_climate_netcdfs.md),
[`riv1`](https://fernandezpalomino.github.io/hydroSWAT/reference/riv1.md),
[`subs1`](https://fernandezpalomino.github.io/hydroSWAT/reference/subs1.md),
[`swat_txtinout_data`](https://fernandezpalomino.github.io/hydroSWAT/reference/swat_txtinout_data.md)

## Examples

``` r
head(qobserved)
#> # A tibble: 6 × 2
#>   Date        Flow
#>   <date>     <dbl>
#> 1 2010-01-01  13.8
#> 2 2010-01-02  14.0
#> 3 2010-01-03  14.3
#> 4 2010-01-04  16.7
#> 5 2010-01-05  20.7
#> 6 2010-01-06  15.9
# plot(qobserved$Date, qobserved$Flow, type = "l",
#      xlab = "Date", ylab = "Flow (m^3/s)")
```
