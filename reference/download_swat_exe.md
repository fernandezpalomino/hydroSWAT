# Download SWAT executable

Downloads and extracts the official SWAT executable (Windows only) to a
specified directory, avoiding manual download and extraction steps.

## Usage

``` r
download_swat_exe(
  dest_dir = ".",
  type = c("release", "debug"),
  url = "https://swat.tamu.edu/media/3bcg3zvo/rev695_executables.zip",
  overwrite = FALSE
)
```

## Arguments

- dest_dir:

  character. Directory where the executable will be extracted. Defaults
  to the current working directory.

- type:

  character. Type of executable: either `"release"` or `"debug"`.
  Default is `"release"`.

- url:

  character. URL to the ZIP archive with SWAT executables. Defaults to
  the official ZIP used by this package.

- overwrite:

  logical. Whether to overwrite an existing executable at `dest_dir`.
  Default is `FALSE`.

## Value

character. Full path to the extracted SWAT executable.

## Note

The default `url` targets the official SWAT executables. If it changes
upstream, supply a new URL via `url`.

## See also

Other Project setup and execution:
[`get_swat_example()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_example.md),
[`run_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
[`run_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md),
[`setup_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md)

## Examples

``` r
# \donttest{
if (.Platform$OS.type == "windows") {
  tmpdir <- tempdir()
  exe_path <- download_swat_exe(dest_dir = tmpdir, type = "release")
  exe_path
}
# }
```
