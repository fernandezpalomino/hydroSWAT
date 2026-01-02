# Change a single SWAT model parameter

Modifies the value of a single SWAT model parameter in one or more input
files. Typically used for calibration or sensitivity analysis at the
subbasin or global level. This function is used by
[change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md)
and supports
[calibrate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)
for multi-objective calibration workflows.

## Usage

``` r
change_parameter(
  subbasins = NULL,
  component,
  parameter,
  value,
  method,
  version = "SWAT",
  plant_type = NULL
)
```

## Arguments

- subbasins:

  numeric. Vector of subbasin IDs where the parameter change should be
  applied. If \`NULL\` (default), changes are applied globally.

- component:

  character. SWAT input component (e.g., \`".gw"\`, \`".mgt"\`,
  \`"plant.dat"\`, \`"basins.bsn"\`) for the corresponding parameter to
  modify. See
  [get_swat_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md).

- parameter:

  character. Name of the SWAT parameter to modify within the defined
  \`component\`.

- value:

  numeric. New value or adjustment factor for the parameter.

- method:

  character. Method for applying the change:

  - \`"v"\`: Replace the value.

  - \`"r"\`: Relative change (e.g., \`0.1\` increases the current value
    by 10

- version:

  character. SWAT version (\`"SWAT"\` or \`"SWAT_T"\`). Default is
  \`"SWAT"\`.

- plant_type:

  character. Type of plant to modify. Required when \`component =
  "plant.dat"\`.

## Value

A list (class \`"changed_parameter"\`) summarizing the applied changes,
including all input arguments and the modified files.

## Details

For `component = ".wgn"` and `".pcp"`, only `method = "r"` is supported.
For `component = "plant.dat"`, `plant_type` is required. The function
expects to run inside the SWAT project's `TxtInOut` folder.

## See also

[calibrate_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)

Other Parameter management:
[`change_multiple_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md),
[`get_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md),
[`split_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/split_swat_parameters.md)

## Examples

``` r
# \donttest{
tmpdir <- tempdir()

# Get example SWAT project
get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/RtmpqhcGSX/TxtInOut
setwd(file.path(tmpdir, "TxtInOut"))

# Modify GW_DELAY in .gw globally
change_parameter(
  component = ".gw",
  parameter = "GW_DELAY",
  value = 30,
  method = "v"
)
#> Parameter change applied successfully.
#> $subbasins
#> NULL
#> 
#> $component
#> [1] ".gw"
#> 
#> $parameter
#> [1] "GW_DELAY"
#> 
#> $value
#> [1] 30
#> 
#> $method
#> [1] "v"
#> 
#> $files
#>  [1] "000010001.gw" "000010002.gw" "000010003.gw" "000010004.gw" "000020001.gw"
#>  [6] "000020002.gw" "000020003.gw" "000020004.gw" "000030001.gw" "000030002.gw"
#> 
#> attr(,"class")
#> [1] "list"              "changed_parameter"

# Apply a relative change to CN2 in .mgt for subbasins 1 and 2
change_parameter(
  subbasins = c(1, 2),
  component = ".mgt",
  parameter = "CN2",
  value = 0.1,
  method = "r"
)
#> Parameter change applied successfully.
#> $subbasins
#> [1] 1 2
#> 
#> $component
#> [1] ".mgt"
#> 
#> $parameter
#> [1] "CN2"
#> 
#> $value
#> [1] 0.1
#> 
#> $method
#> [1] "r"
#> 
#> $files
#> [1] "000010001.mgt" "000010002.mgt" "000010003.mgt" "000010004.mgt"
#> [5] "000020001.mgt" "000020002.mgt" "000020003.mgt" "000020004.mgt"
#> 
#> attr(,"class")
#> [1] "list"              "changed_parameter"
# }
```
