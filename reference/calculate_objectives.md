# Compute calibration objectives for the SWAT model

Computes objective metrics for multiobjective SWAT model calibration.
Users can define multiple objectives, each linked to spatial units such
as river reaches (`"rch"`), subbasins (`"sub"`), or HRUs (`"hru"`). Each
objective specifies target location, variable, performance metric, and
time step. This function is called by
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)
during optimization.

## Usage

``` r
calculate_objectives(parameter_values, fn_args)
```

## Arguments

- parameter_values:

  numeric. Vector of calibration parameter values.

- fn_args:

  list. Named list with inputs/configuration:

  - `objectives`: Calibration objectives.

  - `observed_data`: Observed data for comparison.

  - `output_config`: How to extract SWAT outputs.

  - `parameter_info`: Parameter specifications.

  - `subbasins`: Subbasin IDs to modify (`NULL` all).

  - `swat_exe_path`: Path to the SWAT executable.

## Value

Numeric vector of objective values (one per objective in `objectives`).

## Details

**objectives**: list of objectives. Each element contains:

- `element`: `"rch"`, `"sub"`, or `"hru"`.

- `variable`: e.g., `"FLOW_OUTcms"`.

- `target_id`: IDs to evaluate (type depends on element).

- `metric`: performance metric (see
  [gof_metrics](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md)).

- `calib_time_step`: `"daily"` or `"monthly"`.

- `calib_start_date`: Start date of the calibration period.

- `calib_end_date`: End date of the calibration period.

Example objectives (daily, reach 3, 2011-2013):

    objectives <- list(
      list(
        element          = "rch",
        variable         = "FLOW_OUTcms",
        target_id        = 3,
        metric           = "NSE",
        calib_time_step  = "daily",
        calib_start_date = "2011-01-01",
        calib_end_date   = "2013-12-31"
      ),
      list(
        element          = "rch",
        variable         = "FLOW_OUTcms",
        target_id        = 3,
        metric           = "log_NSE",
        calib_time_step  = "daily",
        calib_start_date = "2011-01-01",
        calib_end_date   = "2013-12-31"
      )
    )

**observed_data**: nested list by time step (`"daily"` or `"monthly"`),
spatial unit, and variable. Each terminal element is a tibble with
columns `Date`, `value`, `target_id`.

Example structure (daily, reach 3):

    observed_data <- list(
      daily = list(
        rch = list(
          "FLOW_OUTcms" = tibble::tibble(
            Date      = seq.Date(as.Date("2010-01-01"),
                                 as.Date("2013-12-31"), by = "day"),
            value     = runif(1461, 1, 50),
            target_id = 3
          )
        )
      )
    )

**output_config**: how to read SWAT outputs for each element. These
arguments are passed to
[output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
or
[output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md),
as appropriate.

- `file`: e.g., `"output.rch"`.

- `variable`: variables to extract.

- `target_id`: IDs to keep (`NULL` all).

- `time_step`: `"daily"` or `"monthly"`.

- `output_start_date`: date when SWAT begins writing outputs (typically
  after warm-up).

Example (daily reach output; with a 1-year warm-up, outputs begin on
2011-01-01):

    output_config <- list(
      rch = list(
        file              = "output.rch",
        variable          = c("FLOW_OUTcms"),
        target_id         = c(3),
        time_step         = "daily",
        output_start_date = "2011-01-01"
      )
    )

**parameter_info**: data frame/tibble describing parameter changes,
passed to
[change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md).
Columns:

- `component`: e.g., `".gw"`, `".mgt"`, `".hru"`.

- `parameter`: parameter name.

- `value`: value/adjustment factor (filled internally).

- `method`: `"v"` (value) or `"r"` (relative).

- `version` (opt.): `"SWAT"` or `"SWAT_T"`.

- `plant_type` (opt.): plant type for `plant.dat`.

See
[change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md)
and
[change_parameter](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md)
for details; list of available parameters:
[get_swat_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md).

**swat_exe_path**: path to the SWAT executable.

**subbasins** (optional): vector of subbasin IDs (`NULL` all).

## Note

When used via
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)
with `fn = calculate_objectives` (the package default), `fn_args` is
validated upstream to avoid per-iteration overhead. If you pass a custom
`fn`,
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)
does not validate its inputs; your function must validate `fn_args` (or
you must validate it before calling
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)).

## See also

[run_swat_exe](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md)

Other Multiobjective calibration:
[`best_compromise_solution()`](https://fernandezpalomino.github.io/hydroSWAT/reference/best_compromise_solution.md),
[`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md),
[`create_calibration_project()`](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md)

## Examples

``` r
# \donttest{
# Windows-only example (runs SWAT once to compute objectives).
# On non-Windows, skip or provide your own compiled swat.exe.
if (.Platform$OS.type == "windows") {

  tmpdir <- tempdir()
  swat_project <- get_swat_example(tmpdir)

  create_calibration_project(
    swat_TxtInOut   = swat_project,
    destination_dir = tmpdir,
    project_name    = "calc_obj_example",
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

  # Objectives (NSE and log_NSE at reach 3; 2011-01-01 to 2013-12-31)
  objectives <- list(
    list(
      element          = "rch",
      variable         = "FLOW_OUTcms",
      target_id        = 3,
      metric           = "NSE",
      calib_time_step  = "daily",
      calib_start_date = "2011-01-01",
      calib_end_date   = "2013-12-31"
    ),
    list(
      element          = "rch",
      variable         = "FLOW_OUTcms",
      target_id        = 3,
      metric           = "log_NSE",
      calib_time_step  = "daily",
      calib_start_date = "2011-01-01",
      calib_end_date   = "2013-12-31"
    )
  )

  # Observed flows (qobserved) provided with the package
  observed_data <- list(
    daily = list(
      rch = list(
        "FLOW_OUTcms" = tibble::tibble(
          Date      = qobserved$Date,
          value     = qobserved$Flow,
          target_id = 3
        )
      )
    )
  )

  # What to read from SWAT outputs
  output_config <- list(
    rch = list(
      file              = "output.rch",
      variable          = c("FLOW_OUTcms"),
      target_id         = c(3),
      time_step         = "daily",
      output_start_date = setup_swat_out$output_start_date
    )
  )

  # Parameters and bounds (values filled when evaluating objectives)
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

  # Assemble arguments for objective computation
  fn_args <- list(
    parameter_info = parameter_info,
    observed_data  = observed_data,
    output_config  = output_config,
    objectives     = objectives,
    subbasins      = NULL,
    swat_exe_path  = swat_exe
  )

  # Single evaluation for a candidate parameter vector
  obj_vals <- calculate_objectives(
    parameter_values = c(30, 0.8, 725, 0.2, 1.3, 0.95, 0.00),
    fn_args          = fn_args
  )
  print(round(obj_vals, 3))
} else {
  message("Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.")
}
#> Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.
# }
```
