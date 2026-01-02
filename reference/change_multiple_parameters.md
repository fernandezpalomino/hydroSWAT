# Change multiple SWAT model parameters

Applies several SWAT parameter updates by iterating over rows of
\`parameter_info\` and internally calling
[change_parameter](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md).
Commonly used in calibration or sensitivity workflows, and integrates
with
[run_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md)
and
[calibrate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md).

## Usage

``` r
change_multiple_parameters(parameter_info, subbasins = NULL)
```

## Arguments

- parameter_info:

  data.frame or tibble. Each row defines one parameter update and must
  include: \`component\`, \`parameter\`, \`value\`, \`method\`. Optional
  columns: \`version\` (defaults to "SWAT") and \`plant_type\` (required
  when \`component = "plant.dat"\`).

- subbasins:

  numeric. Vector of subbasin IDs where the changes should be applied.
  If \`NULL\` (default), changes are applied globally.

## Value

A list with one element per row of \`parameter_info\`. Each element is
the value returned by
[change_parameter](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md).

## See also

[run_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
[calibrate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)

Other Parameter management:
[`change_parameter()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md),
[`get_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md),
[`split_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/split_swat_parameters.md)

## Examples

``` r
# \donttest{
library(tibble)

tmpdir <- tempdir()
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpqhcGSX/TxtInOut
setwd(file.path(tmpdir, "TxtInOut"))

# Define multiple parameter changes
parameter_info <- tibble(
  component = c(".gw", ".mgt"),
  parameter = c("GW_DELAY", "CN2"),
  value     = c(30, 0.10),
  method    = c("v", "r")
)

# Apply changes to subbasins 1 and 2
results <- change_multiple_parameters(
  parameter_info, subbasins = c(1, 2)
)
#> Parameter change applied successfully.
#> Parameter change applied successfully.

# Apply changes globally (all subbasins)
results_global <- change_multiple_parameters(parameter_info)
#> Parameter change applied successfully.
#> Parameter change applied successfully.
# }
```
