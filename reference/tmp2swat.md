# Update SWAT temperature input files (\`tmp\*.tmp\`)

Updates daily SWAT temperature files (\`tmp\*.tmp\`) using alternative
maximum and minimum temperature products provided as dataframes of
subbasin time series, typically estimated with
[areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).
This enables forcing SWAT with different gridded temperature datasets
instead of the original inputs.

## Usage

``` r
tmp2swat(tmax, tmin, tmp, output_dir = getwd())
```

## Arguments

- tmax:

  A dataframe where the first column ('Date') contains dates in
  "yyyy-mm-dd" format, and the remaining columns contain subbasin
  maximum temperature values. This dataframe is typically produced by
  [areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).

- tmin:

  A dataframe with the same structure as \`tmax\`, but containing
  subbasin minimum temperature values.

- tmp:

  A vector of file paths to existing \`tmp\*.tmp\` files, used as
  templates for writing the updated temperature data.

- output_dir:

  A character string specifying the directory where the updated files
  will be written. Defaults to the current working directory.

## See also

[`areal_mean`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`tmp2swat_editor`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)

Other Preprocessing tools:
[`areal_mean()`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md),
[`pcp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md),
[`tmp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)

## Examples

``` r
# \donttest{
# Prepare example SWAT TxtInOut and NetCDF inputs
tmpdir <- tempdir()
txtinout <- get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpzZaKls/TxtInOut
tmp_files <- list.files(file.path(txtinout), pattern = "^tmp.*\\.tmp$", full.names = TRUE)

tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")
tmax_grid <- terra::rast(tmax_nc)
tmin_grid <- terra::rast(tmin_nc)

# Example subbasin polygons from the package
data(subs1, package = "hydroSWAT")

# Compute areal means
tmax_data <- areal_mean(tmax_grid, subs1, "2010-01-01", time_step = "day")
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
tmin_data <- areal_mean(tmin_grid, subs1, "2010-01-01", time_step = "day")
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

# Update tmp*.tmp files
outdir <- file.path(tmpdir, "updated_tmp")
dir.create(outdir)
tmp2swat(tmax_data, tmin_data, tmp_files, output_dir = outdir)
#> tmp*.tmp files were written into: /tmp/RtmpzZaKls/updated_tmp
# }
```
