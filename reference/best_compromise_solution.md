# Select a best-compromise Pareto solution by Euclidean distance

Selects the solution on a Pareto front with the smallest Euclidean
distance to the ideal point.

## Usage

``` r
best_compromise_solution(pareto_solutions, normalize = TRUE)
```

## Arguments

- pareto_solutions:

  matrix/data.frame/tibble. Each row is a solution (objective vector)
  and each column is an objective; values must be numeric (no NAs) and
  oriented for minimization.

- normalize:

  logical. Whether to min–max normalize objectives column-wise before
  distance calculation (default `TRUE`).

## Value

A list with:

- `objectives`: numeric vector with the objective values of the selected
  solution (original, unnormalized scale).

- `index`: integer row index of the selected solution in
  `pareto_solutions`.

## Details

The ideal point is the vector of column minima in the (normalized)
objective space. Euclidean distances are computed from each solution to
this ideal point; the closest solution is returned. If several solutions
have equal minimum distance, the first occurrence (by row order) is
returned.

## See also

Other Multiobjective calibration:
[`calculate_objectives()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calculate_objectives.md),
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md),
[`create_calibration_project()`](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md)

## Examples

``` r
pareto_solutions <- matrix(
  c(0.1554, 0.0282, 0.30,
    0.1309, 0.0608, 0.28,
    0.1418, 0.0460, 0.25),
  ncol = 3, byrow = TRUE
)
best     <- best_compromise_solution(pareto_solutions)
best_raw <- best_compromise_solution(pareto_solutions, normalize = FALSE)
```
