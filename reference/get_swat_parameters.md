# Retrieve SWAT model parameters

Returns the set of SWAT parameters available for calibration or
sensitivity analysis for a given SWAT version. The table helps define
valid \`component\`/\`parameter\` pairs for
[change_parameter](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md)
and
[change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md).

## Usage

``` r
get_swat_parameters(version = "SWAT")
```

## Arguments

- version:

  character. SWAT version ("SWAT" or "SWAT_T"). Default: "SWAT".

## Value

A tibble with:

- \`component\`: SWAT input component (e.g., ".gw", ".mgt").

- \`parameter\`: Parameter name.

- \`line\`: Line index in the file.

- \`start\`: Start position in the line.

- \`stop\`: End position in the line.

- \`format\`: Print format used when writing.

- \`description\`: Short description.

## See also

Other Parameter management:
[`change_multiple_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md),
[`change_parameter()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md),
[`split_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/split_swat_parameters.md)

## Examples

``` r
# Parameters for default SWAT
p <- get_swat_parameters()

# Parameters for SWAT_T
pt <- get_swat_parameters(version = "SWAT_T")
```
