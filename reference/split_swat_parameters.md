# Split SWAT parameters for parallel processing

Split a table of parameter sets into \`cores\` contiguous chunks for
parallel execution, distributing rows as evenly as possible.

## Usage

``` r
split_swat_parameters(cores, parameter_sample)
```

## Arguments

- cores:

  integer. Number of chunks to produce (e.g., CPU cores).

- parameter_sample:

  matrix or data.frame. Table of parameter sets to split (one row per
  set).

## Value

A list of length \`cores\` with subsets of \`parameter_sample\` (input
row order preserved).

## See also

Other Parameter management:
[`change_multiple_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md),
[`change_parameter()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md),
[`get_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md)

## Examples

``` r
# \donttest{
x <- data.frame(a = 1:10, b = 11:20)
split <- split_swat_parameters(cores = 3, parameter_sample = x)
vapply(split, nrow, integer(1))
#> [1] 4 3 3
# }
```
