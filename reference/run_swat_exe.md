# Run SWAT executable

Executes the SWAT model in the current working directory, which must
contain a valid SWAT project (i.e., a `TxtInOut` folder with
`file.cio`).

## Usage

``` r
run_swat_exe(swat_exe_path = "swat.exe")
```

## Arguments

- swat_exe_path:

  character. Path to the SWAT executable (e.g., `"swat.exe"`). On
  Windows, see
  [download_swat_exe](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md);
  on Linux/macOS compile from source.

## Value

integer. Exit status of the SWAT execution:

- `0`: Successful execution.

- Non-zero: An error occurred during execution.

## See also

Other Project setup and execution:
[`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md),
[`get_swat_example()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_example.md),
[`run_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
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
  setwd(file.path(tmpdir, "TxtInOut"))

  # Download SWAT executable
  exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")

  # Run SWAT
  run_swat_exe(exe_path)
}
# }
```
