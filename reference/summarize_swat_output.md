# Summarize SWAT output data

Processes SWAT model output data from reaches, subbasins, or HRUs and
aggregates them at different temporal and spatial scales.

\- If the input data are daily, they are aggregated to monthly and then
to annual scale.  
- If the input data are monthly, they are aggregated directly to annual
scale.

Additionally, the function computes climatological summaries:  
- Mean annual cycle and mean annual values for each simulated variable
at the HRU, subbasin, or reach level.  
- Mean annual values by land use/land cover (LULC) for HRU outputs.  
- Basin-wide climatological summaries, including mean annual cycle and
mean annual values, which are area-weighted averages across the entire
watershed (only for subbasin and HRU outputs).

## Usage

``` r
summarize_swat_output(swat_output, water_year = FALSE, water_year_start = 9)
```

## Arguments

- swat_output:

  A data frame containing SWAT output data, typically read using the
  [output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
  [output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
  or
  [output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md)
  functions.

- water_year:

  Logical. If \`TRUE\`, adjusts data to hydrological years, starting
  from the specified month.

- water_year_start:

  Integer. The month to start the hydrological year (default is 9 for
  September).

## Value

A list containing:

- \`daily\`: Original daily data (if the input is daily, otherwise
  \`NULL\`).

- \`monthly\`: Aggregated monthly data or original input if already
  monthly.

- \`annual\`: Aggregated annual data.

- \`mean_annual\`: Mean annual values for each HRU, subbasin, or reach.

- \`mean_annual_cycle\`: Mean monthly climatology for each HRU,
  subbasin, or reach.

- \`mean_annual_LULC\`: Mean annual values aggregated by land use/land
  cover (only for HRUs).

- \`mean_annual_basin\`: Area-weighted mean annual values across the
  entire basin (only for subbasins and HRUs).

- \`mean_annual_cycle_basin\`: Area-weighted mean monthly climatology
  across the entire basin (only for subbasins and HRU outputs).

- \`basin_area\`: Total area of the basin (only for subbasin and HRU
  outputs).

## See also

[output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
[output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md)

## Examples

``` r
# --- Example with Reach data ---
tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpqhcGSX/TxtInOut
rch_file <- file.path(tmpdir, "TxtInOut", "output.rch")
rch_data <- output_rch(file = rch_file, variable = c("FLOW_OUTcms"),
                       target_id = NULL, time_step = "daily",
                       output_start_date = "2011-01-01")
summary_rch <- summarize_swat_output(rch_data)
names(summary_rch)
#> [1] "daily"             "monthly"           "annual"           
#> [4] "mean_annual"       "mean_annual_cycle"

# --- Example with Subbasin data ---
sub_file <- file.path(tmpdir, "TxtInOut", "output.sub")
sub_data <- output_sub(file = sub_file,
                       variable = c("AREAkm2", "PRECIPmm", "ETmm", "WYLDmm"),
                       target_id = NULL, time_step = "daily",
                       output_start_date = "2011-01-01")
summary_sub <- summarize_swat_output(sub_data, water_year = TRUE)
head(summary_sub$mean_annual)
#> # A tibble: 3 × 6
#>     SUB   MON AREAkm2 PRECIPmm  ETmm WYLDmm
#>   <dbl> <dbl>   <dbl>    <dbl> <dbl>  <dbl>
#> 1     1 2012.   1114.     783.  563.   193.
#> 2     2 2012.   2072.     717.  555.   134.
#> 3     3 2012.    367.     694.  560.   114.

# --- Example with HRU data ---
hru_file <- file.path(tmpdir, "TxtInOut", "output.hru")
hru_data <- output_hru(file = hru_file,
                       variable = c("AREAkm2", "PRECIPmm", "ETmm", "WYLDmm"),
                       target_id = NULL, time_step = "daily",
                       output_start_date = "2011-01-01")
summary_hru <- summarize_swat_output(hru_data)
head(summary_hru$mean_annual_LULC)
#> # A tibble: 2 × 4
#>   LULC  PRECIPmm  ETmm WYLDmm
#>   <chr>    <dbl> <dbl>  <dbl>
#> 1 AGRL      721.  585.   113.
#> 2 PAST      724.  548.   155.

```
