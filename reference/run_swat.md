# Run SWAT model with or without parameter updates

Executes a SWAT project in one of three modes:

- **"exe"**: Run SWAT without modifying parameters (as-is).

- **"single_sample"**: Update one parameter set, run, extract outputs
  (e.g., apply calibrated params).

- **"multiple_sample"**: Iterate over many parameter sets; for each set,
  update inputs, run, extract outputs, then restore.

Internally, this function integrates:

1.  [change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md)
    to update inputs.

2.  [run_swat_exe](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md)
    to execute SWAT.

3.  [output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
    [output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
    [output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md)
    to extract outputs.

For batch runs,
[create_calibration_project](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md)
is recommended; it creates a safe copy with a `Backup` folder used to
restore inputs between runs.

**Note:** In `"multiple_sample"` mode you must pass at least one of
`output_rch_args`, `output_sub_args`, or `output_hru_args` to retrieve
model outputs; otherwise no outputs are extracted.

## Usage

``` r
run_swat(
  TxtInOut_dir = getwd(),
  swat_exe_path,
  execution_mode = "exe",
  parameter_info = NULL,
  subbasins = NULL,
  parameter_sample = NULL,
  output_rch_args = NULL,
  output_sub_args = NULL,
  output_hru_args = NULL,
  cores = 1
)
```

## Arguments

- TxtInOut_dir:

  character. Path to the SWAT `TxtInOut` folder. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- swat_exe_path:

  character. Path to the SWAT executable (e.g., `"swat.exe"`). On
  Windows, see
  [download_swat_exe](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md);
  on Linux/macOS compile from source.

- execution_mode:

  character. One of `"exe"`, `"single_sample"`, or `"multiple_sample"`.

- parameter_info:

  data.frame. Parameter definitions used to update SWAT inputs (required
  for `"single_sample"` and `"multiple_sample"`). Minimum columns:

  - `component` (e.g., `".gw"`, `".mgt"`)

  - `parameter` (name in SWAT input)

  - `value` (numeric; value or factor)

  - `method` (character; `"v"` or `"r"`)

  Optional: `version` (`"SWAT"` or `"SWAT_T"`), and `plant_type` (for
  `plant.dat`). Passed to
  [change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md).

- subbasins:

  numeric. Subbasin IDs where changes apply. `NULL` applies to all.
  Passed to
  [change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md).

- parameter_sample:

  data.frame. Parameter sets for `"multiple_sample"`; each row is one
  set whose column order matches `parameter_info`. Passed to
  [change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md).

- output_rch_args:

  list. Arguments for
  [output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md).

- output_sub_args:

  list. Arguments for
  [output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md).

- output_hru_args:

  list. Arguments for
  [output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md).

- cores:

  integer. CPU cores for `"multiple_sample"` (parallel). Default `1`
  (sequential).

## Value

- `"exe"`: Integer SWAT exit code (`0` = success).

- `"single_sample"`, `"multiple_sample"`: A list with any of `$rch`,
  `$sub`, `$hru` if the corresponding `*_args` were provided.

## See also

[create_calibration_project](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md),
[change_multiple_parameters](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md),
[output_rch](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[output_sub](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
[output_hru](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md)

Other Project setup and execution:
[`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md),
[`get_swat_example()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_example.md),
[`run_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md),
[`setup_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md)

## Examples

``` r
# \donttest{
if (.Platform$OS.type == "windows") {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Example SWAT project
  tmpdir <- tempdir()
  get_swat_example(tmpdir)

  # Create calibration project (adds Backup)
  create_calibration_project(
    swat_TxtInOut   = file.path(tmpdir, "TxtInOut"),
    destination_dir = tmpdir,
    project_name    = "calib_project",
    set_working_dir = TRUE
  )

  # Download SWAT executable
  exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")

  # Configure project
  setup_swat(
    sim_start_date = "2010-01-01",
    sim_end_date   = "2015-12-31",
    time_step      = "daily",
    NYSKIP         = 1,
    rch_vars       = c("FLOW_OUTcms", "SEDCONCmg/L"),
    sub_vars       = c("PRECIPmm", "ETmm"),
    hru_vars       = c("PRECIPmm"),
    hrus           = c(1, 2, 3)
  )

  # Parameter definitions
  parameter_info <- tibble::tibble(
    component = c(".gw", ".mgt"),
    parameter = c("GW_DELAY", "CN2"),
    value     = c(30, 0.1),
    method    = c("v", "r")
  )

  # Output extraction
  output_rch_args <- list(
    file = "output.rch",
    variable = c("FLOW_OUTcms", "SEDCONCmg/L"),
    target_id = 1,
    time_step = "daily",
    output_start_date = "2011-01-01"
  )

  # Run as-is (exe)
  run_swat(getwd(), exe_path, execution_mode = "exe")

  # Multiple sets (e.g., sensitivity)
  parameter_sample <- tibble::tibble(
    GW_DELAY = c(30, 35),
    CN2      = c(0.1, 0.2)
  )
  run_swat(getwd(), exe_path, "multiple_sample",
           parameter_info   = parameter_info,
           parameter_sample = parameter_sample,
           output_rch_args  = output_rch_args,
           cores = 1)

  # Single set (e.g., calibrated)
  run_swat(getwd(), exe_path, "single_sample",
           parameter_info  = parameter_info,
           output_rch_args = output_rch_args)
}
# }
```
