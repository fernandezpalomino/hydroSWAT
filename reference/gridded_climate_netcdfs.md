# Gridded climate NetCDFs (precipitation and temperature)

Small NetCDFs with daily precipitation (RAIN4PE) and daily min/max
temperature (PISCO). Period: 7 days in Jan 2010. Resolution: 0.1 deg
(WGS84; EPSG:4326).

## Format

Three NetCDF files:

- pr.nc:

  Daily precipitation (mm/day), 7 layers.

- tasmax.nc:

  Daily maximum temperature (deg C), 7 layers.

- tasmin.nc:

  Daily minimum temperature (deg C), 7 layers.

## Source

- RAIN4PE:
  [doi:10.5880/pik.2020.010](https://doi.org/10.5880/pik.2020.010)

- PISCO temperature v1.1:
  <https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/>

## Details

Files are located under `inst/extdata` and should be accessed with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

## See also

Other Data:
[`example_basin_datasets`](https://fernandezpalomino.github.io/hydroSWAT/reference/example_basin_datasets.md),
[`qobserved`](https://fernandezpalomino.github.io/hydroSWAT/reference/qobserved.md),
[`riv1`](https://fernandezpalomino.github.io/hydroSWAT/reference/riv1.md),
[`subs1`](https://fernandezpalomino.github.io/hydroSWAT/reference/subs1.md),
[`swat_txtinout_data`](https://fernandezpalomino.github.io/hydroSWAT/reference/swat_txtinout_data.md)

## Examples

``` r
pr_nc     <- system.file("extdata", "pr.nc", package = "hydroSWAT")
tasmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
tasmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
# r <- terra::rast(pr_nc)
# terra::plot(r[[1]], main = "Precipitation (mm/day) 2010-01-01")
```
