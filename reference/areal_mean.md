# Compute areal means of raster data over polygons

Estimates mean values of raster cells for each polygon. Supports
parallel processing, allowing each core to independently load and
process raster layers from a NetCDF file. Used internally by
[pcp2swat_editor](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md)
and
[tmp2swat_editor](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)
for SWAT input preparation.

## Usage

``` r
areal_mean(
  grid,
  geom,
  start_date,
  time_step = "day",
  cores = 1,
  grid_file = NULL
)
```

## Arguments

- grid:

  A \`SpatRaster\` object for single-core processing.

- geom:

  A set of polygons in \`sf\` format to perform the extraction.

- start_date:

  The start date for the time series.

- time_step:

  Time step between raster layers. Either 'day' or 'month'. Default is
  'day'.

- cores:

  Number of cores for parallel processing. Default is 1 (single-core).

- grid_file:

  (Optional) Path to the NetCDF file for parallel processing.

## Value

A data frame with the mean values extracted for each polygon and each
raster layer.

## See also

[`pcp2swat_editor`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md),
[`tmp2swat_editor`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md),
[`pcp2swat`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md),
[`tmp2swat`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md)

Other Preprocessing tools:
[`pcp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md),
[`pcp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md),
[`tmp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md),
[`tmp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)

## Examples

``` r
# \donttest{
# Example with package NetCDF data
pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
data(subs1, package = "hydroSWAT")
grid <- terra::rast(pr_nc)
result <- areal_mean(grid, subs1, "2010-01-01", time_step = "day")
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
# }

if (FALSE) { # \dontrun{
# Example with external NetCDF and shapefile
grid <- terra::rast("path_to_raster_file.nc")
geom <- sf::st_read("path_to_shapefile.shp")
result <- areal_mean(grid, geom, "1981-01-01", time_step = "day",
                     cores = 2, grid_file = "path_to_netCDF.nc")
} # }
```
