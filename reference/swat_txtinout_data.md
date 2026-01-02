# SWAT TxtInOut inputs

Named list that mirrors a SWAT `TxtInOut` directory from the example
project.

## Usage

``` r
swat_txtinout_data
```

## Format

Named list of character vectors. Each element is the content of one SWAT
input file (e.g., `file.cio`, `.gw`, `pcp*.pcp`).

## Source

Prepared by the hydroSWAT authors.

## See also

Other Data:
[`example_basin_datasets`](https://fernandezpalomino.github.io/hydroSWAT/reference/example_basin_datasets.md),
[`gridded_climate_netcdfs`](https://fernandezpalomino.github.io/hydroSWAT/reference/gridded_climate_netcdfs.md),
[`qobserved`](https://fernandezpalomino.github.io/hydroSWAT/reference/qobserved.md),
[`riv1`](https://fernandezpalomino.github.io/hydroSWAT/reference/riv1.md),
[`subs1`](https://fernandezpalomino.github.io/hydroSWAT/reference/subs1.md)

## Examples

``` r
str(swat_txtinout_data[1:3])
#> List of 3
#>  $ 000010000.pnd: chr [1:59] " .Pnd file Subbasin: 1 2025-07-28 12:00:00 AM ArcSWAT 2012.10_8.26" "Pond inputs:" "           0.000    | PND_FR : Fraction of subbasin area that drains into ponds. The value for PND_FR should be"| __truncated__ "           5.000    | PND_PSA: Surface area of ponds when filled to principal spillway [ha]" ...
#>  $ 000010000.rte: chr [1:30] " .rte file Subbasin: 1 2025-07-28 12:00:00 AM ArcSWAT 2012.10_8.26" "        86.829    | CHW2 : Main channel width [m]" "         2.151    | CHD : Main channel depth [m]" "       0.00098    | CH_S2 : Main channel slope [m/m]" ...
#>  $ 000010000.sub: chr [1:65] " .sub file Subbasin: 1 2025-07-28 12:00:00 AM ArcSWAT 2012.10_8.26" "     1113.756851    | SUB_KM : Subbasin area [km2]" "" "Climate in subbasin" ...
```
