# Update SWAT precipitation input files (\`pcp\*.pcp\`)

Updates daily SWAT precipitation files (\`pcp\*.pcp\`) using an
alternative precipitation product provided as a dataframe of subbasin
time series, typically estimated with
[areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).
This enables forcing SWAT with different gridded precipitation datasets
instead of the original inputs.

## Usage

``` r
pcp2swat(x, pcp, output_dir = getwd())
```

## Arguments

- x:

  A dataframe where the first column ('Date') contains dates in
  "yyyy-mm-dd" format, and the remaining columns contain subbasin
  precipitation values. This dataframe is typically produced by
  [areal_mean](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md).

- pcp:

  A vector of file paths to existing \`pcp\*.pcp\` files, used as
  templates for writing the updated precipitation data.

- output_dir:

  A character string specifying the directory where the updated files
  will be written. Defaults to the current working directory.

## See also

[`areal_mean`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat_editor`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md)

Other Preprocessing tools:
[`areal_mean()`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md),
[`pcp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md),
[`tmp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md),
[`tmp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)

## Examples

``` r
# \donttest{
# Prepare example SWAT TxtInOut and NetCDF inputs
tmpdir <- tempdir()
txtinout <- get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpUu15Uv/TxtInOut
pcp_files <- list.files(file.path(txtinout), pattern = "^pcp.*\\.pcp$", full.names = TRUE)

pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
pr_grid <- terra::rast(pr_nc)

# Example subbasin polygons from the package
data(subs1, package = "hydroSWAT")

# Compute areal means
pr_data <- areal_mean(pr_grid, subs1, "2010-01-01", time_step = "day")
#> Data period is from: 2010-01-01 to 2010-01-07
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%

# Update pcp*.pcp files
outdir <- file.path(tmpdir, "updated_pcp")
dir.create(outdir)
pcp2swat(pr_data, pcp_files, output_dir = outdir)
#> pcp*.pcp files were written into: /tmp/RtmpUu15Uv/updated_pcp
# }
```
