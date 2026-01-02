# Generate SWAT Editor-compatible precipitation input files

Writes SWAT Editor-compatible precipitation input files for each
subbasin by estimating areal mean precipitation from raster or NetCDF
data using
[areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).

## Usage

``` r
pcp2swat_editor(
  grid,
  subs1,
  start_date,
  time_step = "day",
  unit = "mm",
  cores = 1,
  grid_file = NULL,
  output_dir = getwd()
)
```

## Arguments

- grid:

  A \`SpatRaster\` object representing precipitation data.

- subs1:

  An \`sf\` object representing the subbasins (e.g., shapefile from
  ArcSWAT or QSWAT).

- start_date:

  Start date of the record in "yyyy-mm-dd" format.

- time_step:

  Time step between raster layers. Either 'day' or 'month'. Default is
  'day'.

- unit:

  Precipitation unit. Either 'mm' or 'kg/m2/s'. Default is 'mm'.

- cores:

  Number of CPU cores for parallel processing. Default is 1.

- grid_file:

  Path to the NetCDF file (required for parallel processing).

- output_dir:

  Directory where SWAT input files will be written. Defaults to current
  working directory.

## Value

Writes SWAT Editor-compatible precipitation input files and a station
table in the specified directory.

## See also

[`areal_mean`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md)

Other Preprocessing tools:
[`areal_mean()`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md),
[`tmp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md),
[`tmp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)

## Examples

``` r
# \donttest{
# Example with package NetCDF data
pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
data(subs1, package = "hydroSWAT")
grid <- terra::rast(pr_nc)
pcp2swat_editor(grid, subs1, "2010-01-01", output_dir = tempdir())
#> Estimating areal mean precipitation for each subbasin...
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#> Writing SWAT precipitation input files...
#> Output files written to: /tmp/RtmpWxZ4hT/pcp_swat
# }

if (FALSE) { # \dontrun{
# Example with external NetCDF and shapefile
subs1 <- sf::st_read("path_to_subbasin_shapefile.shp")
grid <- terra::rast("path_to_precipitation_data.nc")
pcp2swat_editor(grid, subs1, "1981-01-01", cores = 3,
                grid_file = "path_to_precipitation_data.nc",
                output_dir = "C:/swat_inputs")
} # }
```
