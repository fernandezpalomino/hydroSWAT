# Perform multiobjective calibration of a SWAT model with NSGA-II

Calibrates a SWAT model using the Non-dominated Sorting Genetic
Algorithm II (NSGA-II). The function builds on nsga2R and parallelizes
the evaluation of candidate parameter sets during initialization and
across generations.

## Usage

``` r
calibrate_swat(
  fn = calculate_objectives,
  varNo,
  objDim,
  lowerBounds,
  upperBounds,
  popSize = 100,
  tourSize = 2,
  generations = 10,
  cprob = 0.7,
  XoverDistIdx = 5,
  mprob = 0.2,
  MuDistIdx = 10,
  TxtInOut_dir = getwd(),
  cores = 1,
  fn_args = NULL,
  required_libraries = NULL
)
```

## Arguments

- fn:

  function. Multiobjective function to minimize (custom or
  [calculate_objectives](https://fernandezpalomino.github.io/hydroSWAT/reference/calculate_objectives.md)).
  For a custom `fn`, the user is responsible for validating `fn_args`,
  and the function must return a numeric vector of length `objDim`.

- varNo:

  integer. Number of decision variables (model parameters).

- objDim:

  integer. Number of objective functions.

- lowerBounds:

  numeric. Lower bounds for decision variables.

- upperBounds:

  numeric. Upper bounds for decision variables.

- popSize:

  integer. Population size (default `100`).

- tourSize:

  integer. Tournament size (default `2`).

- generations:

  integer. Number of generations (default `10`).

- cprob:

  numeric. Crossover probability (default `0.7`).

- XoverDistIdx:

  numeric (\\\geq\\ 0). Crossover distribution index (default `5`).

- mprob:

  numeric. Mutation probability (default `0.2`).

- MuDistIdx:

  numeric (\\\geq\\ 0). Mutation distribution index (default `10`).

- TxtInOut_dir:

  character. Path to the SWAT `TxtInOut` directory.

- cores:

  integer. Cores used for parallel evaluation (default `1`).

- fn_args:

  list. Inputs required by `fn` (e.g., parameter table, observed data,
  output-reading config, objectives, SWAT exe path). The required
  structure depends on `fn`. For `calculate_objectives` see
  [calculate_objectives](https://fernandezpalomino.github.io/hydroSWAT/reference/calculate_objectives.md).

- required_libraries:

  character. Extra packages to load on each worker for custom `fn`
  (default `NULL`).

## Value

An object of class `"nsga2R"` with the usual NSGA-II settings and:

- `parameters`: Non-dominated decision vectors.

- `objectives`: Objective values for those vectors.

- `paretoFrontRank`: Nondomination rank per solution.

- `crowdingDistance`: Crowding distance per solution.

For reproducibility, `TxtInOut_dir`, `cores`, and `fn_args` are
attached.

## Details

To calibrate a SWAT model, first create a calibration project from a
valid SWAT `TxtInOut` folder with
[create_calibration_project](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md),
set that folder as the working directory, and configure
simulation/output settings with
[setup_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md).

## References

Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. (2002). A fast and
elitist multiobjective genetic algorithm: NSGA-II. *IEEE Trans. Evol.
Comput.*, 6(2), 182–197.

## See also

[run_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
[run_swat_exe](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md),
[setup_swat](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md);
see the vignette:
`vignette("Calibrating-SWAT-with-hydroSWAT-A-complete-workflow", "hydroSWAT")`.

Other Multiobjective calibration:
[`best_compromise_solution()`](https://fernandezpalomino.github.io/hydroSWAT/reference/best_compromise_solution.md),
[`calculate_objectives()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calculate_objectives.md),
[`create_calibration_project()`](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md)

## Examples

``` r
# \donttest{
# Windows-only example (downloads SWAT EXE). On non-Windows, skip.
if (.Platform$OS.type == "windows") {

  tmpdir <- tempdir()
  swat_project <- get_swat_example(tmpdir)

  create_calibration_project(
    swat_TxtInOut   = swat_project,
    destination_dir = tmpdir,
    project_name    = "calibration_example",
    set_working_dir = TRUE
  )

  # SWAT executable (if download fails, set your local path below)
  swat_exe <- tryCatch(
    download_swat_exe(dest_dir = tmpdir, type = "release"),
    error = function(e) {
      message("download_swat_exe() failed: ", conditionMessage(e))
      "path/to/swat.exe"  # <-- set your local executable
    }
  )

  # Simulation setup (warm-up = 1 year)
  setup_swat_out <- setup_swat(
    sim_start_date = "2010-01-01",
    sim_end_date   = "2013-12-31",
    time_step      = "daily",
    NYSKIP         = 1,
    rch_vars       = c("FLOW_OUTcms")
  )

  # Objectives (NSE and log_NSE at reach 3)
  objectives <- list(
    list(
      element = "rch", variable = "FLOW_OUTcms", target_id = 3,
      metric  = "NSE", calib_time_step = "daily",
      calib_start_date = "2011-01-01", calib_end_date = "2013-12-31"
    ),
    list(
      element = "rch", variable = "FLOW_OUTcms", target_id = 3,
      metric  = "log_NSE", calib_time_step = "daily",
      calib_start_date = "2011-01-01", calib_end_date = "2013-12-31"
    )
  )

  # Observed flows (qobserved) included in the package
  observed_data <- list(
    daily = list(
      rch = list(
        "FLOW_OUTcms" = tibble::tibble(
          Date = qobserved$Date,
          value = qobserved$Flow,
          target_id = 3
        )
      )
    )
  )

  # What to read from SWAT outputs
  output_config <- list(
    rch = list(
      file = "output.rch",
      variable = c("FLOW_OUTcms"),
      target_id = c(3),
      time_step = "daily",
      output_start_date = setup_swat_out$output_start_date
    )
  )

  # Parameters and bounds
  parameter_info <- tibble::tibble(
    component  = c(".gw", ".gw", ".gw", ".gw", ".hru", ".hru", ".mgt"),
    parameter  = c("GW_DELAY", "ALPHA_BF", "GWQMN", "RCHRG_DP",
                   "SURLAG", "ESCO", "CN2"),
    value      = NA,
    method     = c("v", "v", "v", "v", "v", "v", "r"),
    version    = rep("SWAT", 7),
    plant_type = NA,
    min        = c(10, 0.50, 700, 0.05, 1.0, 0.90, -0.05),
    max        = c(50, 1.00, 750, 0.50, 2.0, 1.00,  0.05)
  )

  # Arguments for the objective function
  fn_args <- list(
    parameter_info = parameter_info,
    observed_data  = observed_data,
    output_config  = output_config,
    objectives     = objectives,
    subbasins      = NULL,
    swat_exe_path  = swat_exe
  )

  # Run NSGA-II calibration (small demo)
  set.seed(123)
  calibration_result <- calibrate_swat(
    fn           = calculate_objectives,
    varNo        = nrow(parameter_info),
    objDim       = length(objectives),
    lowerBounds  = parameter_info$min,
    upperBounds  = parameter_info$max,
    popSize      = 10,
    generations  = 2,
    TxtInOut_dir = getwd(),
    cores        = 2,
    fn_args      = fn_args
  )

  # Identify optimal parameters and Pareto solutions
  bcs <- best_compromise_solution(calibration_result$objectives)

  # Named best-parameter vector
  best_par <- as.numeric(
    calibration_result$parameters[bcs$index, , drop = TRUE]
  )
  names(best_par) <- parameter_info$parameter
  print(round(best_par, 3))

  best_obj <- calibration_result$objectives[bcs$index, ]

  plot(
    calibration_result$objectives,
    xlab = "1-log_NSE", ylab = "1-NSE",
    col = "gray50", pch = 19
  )
  points(
    calibration_result$objectives[
      calibration_result$paretoFrontRank == 1, ],
    col = "black", pch = 19
  )
  points(
    best_obj[1], best_obj[2],
    col = "blue", pch = 19, cex = 1.5
  )
  legend(
    "topright",
    legend = c(
      "Pareto Solutions (PS)",
      "Pareto Optimal Front (POF)",
      "Best Compromise Solution (BCS)"
    ),
    col = c("gray50", "black", "blue"),
    pch = 19, cex = 0.6, bty = "n"
  )
} else {
  message("Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.")
}
#> Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.
# }
```
