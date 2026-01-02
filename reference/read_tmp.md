# Read temperature data from SWAT .tmp files

Reads one or more SWAT \`.tmp\` files containing daily maximum and
minimum temperature data and associated metadata for temperature gauges
such as station IDs, subbasins, and their latitude, longitude, and
elevation. These files use a fixed-width format: the first four lines
store station IDs and metadata, and the remaining lines contain daily
values (year, day of year, and maximum and minimum temperatures for each
station).

## Usage

``` r
read_tmp(tmp)
```

## Arguments

- tmp:

  Character vector with paths to one or more \`.tmp\` files in SWAT
  format.

## Value

A list with three tibbles:

- `gauges`: Metadata for each gauge (subbasin, station ID, latitude,
  longitude, elevation).

- `tmax`: Daily maximum temperature (year, day of year, values for each
  station).

- `tmin`: Daily minimum temperature (year, day of year, values for each
  station).

## See also

Other Output readers:
[`get_swat_vars()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md),
[`output_hru()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md),
[`output_rch()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[`output_sub()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
[`read_pcp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_pcp.md)

## Examples

``` r
# Example with package data
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpqhcGSX/TxtInOut
tmp_file <- file.path(tmpdir, "TxtInOut", "tmp1.tmp")
tmp_data <- read_tmp(tmp_file)
head(tmp_data$gauges)
#> # A tibble: 3 × 8
#>   Subbasin Station    Lati  Long  Elev ID_swat tmax           tmin          
#>   <chr>    <chr>     <dbl> <dbl> <dbl>   <int> <chr>          <chr>         
#> 1 1        tmp_00001 -15.2 -69.5  4133       1 tmp_00001_tmax tmp_00001_tmin
#> 2 2        tmp_00002 -14.8 -69.8  4312       2 tmp_00002_tmax tmp_00002_tmin
#> 3 3        tmp_00003 -15.1 -69.8  4001       3 tmp_00003_tmax tmp_00003_tmin
head(tmp_data$tmax)
#> # A tibble: 6 × 5
#>    year  yday tmp_00001_tmax tmp_00002_tmax tmp_00003_tmax
#>   <dbl> <dbl>          <dbl>          <dbl>          <dbl>
#> 1  2010     1           17.4           17.7           18  
#> 2  2010     2           17.2           17.4           17.7
#> 3  2010     3           15.9           16.3           16.5
#> 4  2010     4           14.1           14.2           14.9
#> 5  2010     5           16.5           16.8           16.8
#> 6  2010     6           16.3           16.4           16.7
head(tmp_data$tmin)
#> # A tibble: 6 × 5
#>    year  yday tmp_00001_tmin tmp_00002_tmin tmp_00003_tmin
#>   <dbl> <dbl>          <dbl>          <dbl>          <dbl>
#> 1  2010     1            2.4            2.2            2.6
#> 2  2010     2            3.4            3              3.5
#> 3  2010     3            4.9            4.6            4.8
#> 4  2010     4            4.9            4.6            4.9
#> 5  2010     5            5.4            5              5.4
#> 6  2010     6            5              4.7            5.1

if (FALSE) { # \dontrun{
# Example with external files
tmp_files <- list.files("path/to/TxtInOut", pattern = "\\.tmp$", full.names=TRUE)
tmp_data <- read_tmp(tmp_files)
} # }
```
