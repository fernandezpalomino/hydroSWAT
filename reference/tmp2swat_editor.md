# Generate SWAT Editor-compatible temperature input files

Writes SWAT Editor-compatible temperature input files for each subbasin
by estimating areal mean maximum and minimum temperature from raster or
NetCDF data using
[areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).

## Usage

``` r
tmp2swat_editor(
  grid_tmax,
  grid_tmin,
  subs1,
  start_date,
  time_step = "day",
  unit = "C",
  cores = 1,
  grid_tmax_file = NULL,
  grid_tmin_file = NULL,
  output_dir = getwd()
)
```

## Arguments

- grid_tmax:

  A \`SpatRaster\` object representing maximum temperature.

- grid_tmin:

  A \`SpatRaster\` object representing minimum temperature.

- subs1:

  An \`sf\` object representing the subbasins (e.g., shapefile from
  ArcSWAT or QSWAT).

- start_date:

  Start date of the record in "yyyy-mm-dd" format.

- time_step:

  Time step between raster layers. Either 'day' or 'month'. Default is
  'day'.

- unit:

  Temperature unit. Either 'C' (Celsius) or 'K' (Kelvin). Default is
  'C'.

- cores:

  Number of CPU cores for parallel processing. Default is 1.

- grid_tmax_file:

  Path to the NetCDF file for Tmax (required if cores \>1).

- grid_tmin_file:

  Path to the NetCDF file for Tmin (required if cores \>1).

- output_dir:

  Directory where SWAT input files will be written. Defaults to current
  working directory.

## Value

Writes SWAT Editor-compatible temperature input files and a station
table in the specified directory.

## See also

[`areal_mean`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`tmp2swat`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md)

Other Preprocessing tools:
[`areal_mean()`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md),
[`pcp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md),
[`tmp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md)

## Examples

``` r
# \donttest{
# Example with package NetCDF data
tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
data(subs1, package = "hydroSWAT")
grid_tmax <- terra::rast(tmax_nc)
grid_tmin <- terra::rast(tmin_nc)
tmp2swat_editor(grid_tmax, grid_tmin, subs1, "2010-01-01", output_dir = tempdir())
#> Estimating areal mean temperature for each subbasin...
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#> Writing SWAT temperature input files...
#> Output files written to: /tmp/RtmpUu15Uv/tmp_swat
# }

if (FALSE) { # \dontrun{
# Example with external NetCDF and shapefile
subs1 <- sf::st_read("path_to_subbasin_shapefile.shp")
grid_tmax <- terra::rast("path_to_tmax_data.nc")
grid_tmin <- terra::rast("path_to_tmin_data.nc")
tmp2swat_editor(grid_tmax, grid_tmin, subs1, "1981-01-01", cores = 3,
                grid_tmax_file = "path_to_tmax_data.nc",
                grid_tmin_file = "path_to_tmin_data.nc",
                output_dir = "C:/swat_inputs")
} # }
```
