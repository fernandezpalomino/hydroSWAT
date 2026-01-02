# Read precipitation data from SWAT .pcp files

Reads one or more SWAT \`.pcp\` files containing daily precipitation
data and associated metadata for precipitation gauges such as station
IDs, subbasins, and their latitude, longitude, and elevation. These
files use a fixed-width format: the first four lines store station IDs
and metadata, and the remaining lines contain daily values (year, day of
year, and precipitation for each station).

## Usage

``` r
read_pcp(pcp)
```

## Arguments

- pcp:

  Character vector with paths to one or more \`.pcp\` files in SWAT
  format.

## Value

A list with two tibbles:

- `gauges`: Metadata for each gauge (subbasin, station ID, latitude,
  longitude, elevation).

- `pcp`: Daily precipitation (year, day of year, and values for each
  station).

## See also

Other Output readers:
[`get_swat_vars()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md),
[`output_hru()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md),
[`output_rch()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[`output_sub()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
[`read_tmp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_tmp.md)

## Examples

``` r
# Example with package data
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpqhcGSX/TxtInOut
pcp_file <- file.path(tmpdir, "TxtInOut", "pcp1.pcp")
pcp_data <- read_pcp(pcp_file)
head(pcp_data$gauges)
#> # A tibble: 3 × 6
#>   Subbasin Station    Lati  Long  Elev ID_swat
#>   <chr>    <chr>     <dbl> <dbl> <dbl>   <int>
#> 1 1        pcp_00001 -15.2 -69.5  4133       1
#> 2 2        pcp_00002 -14.8 -69.8  4312       2
#> 3 3        pcp_00003 -15.1 -69.8  4001       3
head(pcp_data$pcp)
#> # A tibble: 6 × 5
#>    year  yday pcp_00001 pcp_00002 pcp_00003
#>   <dbl> <dbl>     <dbl>     <dbl>     <dbl>
#> 1  2010     1       0.2       0.7       0.1
#> 2  2010     2       2.1       2.6       2.6
#> 3  2010     3      11.4       6.6       9.8
#> 4  2010     4       6.3       3         3.3
#> 5  2010     5       4.5       1.1       1.9
#> 6  2010     6       3.7       2.2       3.1

if (FALSE) { # \dontrun{
# Example with external files
pcp_files <- list.files("path/to/TxtInOut", pattern = "\\.pcp$", full.names=TRUE)
pcp_data <- read_pcp(pcp_files)
} # }
```
