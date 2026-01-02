# Configure a SWAT project for simulation

Configures a SWAT project by editing the `file.cio` file. Sets the
simulation period, time step, warm-up years, and output variables for
reaches, subbasins, and HRUs.

## Usage

``` r
setup_swat(
  sim_start_date,
  sim_end_date,
  time_step,
  NYSKIP,
  rch_vars = NULL,
  sub_vars = NULL,
  hru_vars = NULL,
  hrus = NULL
)
```

## Arguments

- sim_start_date:

  character. Simulation start date ("YYYY-MM-DD").

- sim_end_date:

  character. Simulation end date ("YYYY-MM-DD").

- time_step:

  character. Simulation time step: `"daily"` or `"monthly"`.

- NYSKIP:

  integer. Years to skip as warm-up before outputs are written.

- rch_vars:

  character or NULL. Reach variables to output (see Details for limits
  and valid names). If `NULL`, zeros are written and SWAT prints all
  reach variables by default.

- sub_vars:

  character or NULL. Subbasin variables to output (see Details for
  limits and valid names). If `NULL`, zeros are written and SWAT prints
  all subbasin variables by default.

- hru_vars:

  character or NULL. HRU variables to output (see Details for limits and
  valid names). If `NULL`, zeros are written and SWAT prints all HRU
  variables by default.

- hrus:

  integer or NULL. HRU IDs to include. If `NULL`, zeros are written and
  SWAT prints all HRUs by default.

## Value

A list summarizing the applied configuration with fields:

- `output_start_date`: first date with outputs after warm-up.

- `time_step`: the selected time step.

- `rch_vars`, `sub_vars`, `hru_vars`: the user selections (or `NULL` if
  all by default).

- `hrus`: selected HRU IDs (or `NULL` if all by default).

## Details

The working directory must contain a valid SWAT `TxtInOut` project.

**Variable limits.** In custom mode SWAT allows up to 20 variables for
reaches, up to 15 for subbasins, and up to 20 for HRUs. Exceeding these
limits throws an error.

**Available variables.** Use
[get_swat_vars](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md)
to list valid names per element:

- Reaches: `get_swat_vars("rch")`

- Subbasins: `get_swat_vars("sub")`

- HRUs: `get_swat_vars("hru")`

Use exact SWAT variable names as returned by
[`get_swat_vars()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md).

Metadata columns (`COLNAME`, `SUB`, `RCH`, `GIS`, `MON`, `AREAkm2`,
`LULC`, `HRU`, `MGT`) are always printed and must not be included in the
selection.

## Note

This function overwrites `file.cio`. Back up your project if needed.

## See also

Other Project setup and execution:
[`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md),
[`get_swat_example()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_example.md),
[`run_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
[`run_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md)

## Examples

``` r
# \donttest{
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  tmpdir <- tempdir()
  get_swat_example(tmpdir)
#> SWAT TxtInOut files written to: /tmp/Rtmp9rCxra/TxtInOut
  setwd(file.path(tmpdir, "TxtInOut"))

  # List available variables
  get_swat_vars("rch")
#>  [1] "COLNAME"      "RCH"          "GIS"          "MON"          "AREAkm2"     
#>  [6] "FLOW_INcms"   "FLOW_OUTcms"  "EVAPcms"      "TLOSScms"     "SED_INtons"  
#> [11] "SED_OUTtons"  "SEDCONCmg/L"  "ORGN_INkg"    "ORGN_OUTkg"   "ORGP_INkg"   
#> [16] "ORGP_OUTkg"   "NO3_INkg"     "NO3_OUTkg"    "NH4_INkg"     "NH4_OUTkg"   
#> [21] "NO2_INkg"     "NO2_OUTkg"    "MINP_INkg"    "MINP_OUTkg"   "CHLA_INkg"   
#> [26] "CHLA_OUTkg"   "CBOD_INkg"    "CBOD_OUTkg"   "DISOX_INkg"   "DISOX_OUTkg" 
#> [31] "SOLPST_INmg"  "SOLPST_OUTmg" "SORPST_INmg"  "SORPST_OUTmg" "REACTPSTmg"  
#> [36] "VOLPSTmg"     "SETTLPSTmg"   "RESUSP_PSTmg" "DIFFUSEPSTmg" "REACBEDPSTmg"
#> [41] "BURYPSTmg"    "BED_PSTmg"    "BACTP_OUTct"  "BACTLP_OUTct" "CMETAL#1kg"  
#> [46] "CMETAL#2kg"   "CMETAL#3kg"   "TOT Nkg"      "TOT Pkg"      "NO3ConcMg/l" 
#> [51] "WTMPdegc"     "Salt1"        "Salt2"        "Salt3"        "Salt4"       
#> [56] "Salt5"        "Salt6"        "Salt7"        "Salt8"        "Salt9"       
#> [61] "Salt10"       "SAR"          "EC"          

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
#> $sim_start_date
#> [1] "2010-01-01"
#> 
#> $sim_end_date
#> [1] "2015-12-31"
#> 
#> $time_step
#> [1] "daily"
#> 
#> $NYSKIP
#> [1] 1
#> 
#> $rch_vars
#> [1] "FLOW_OUTcms" "SEDCONCmg/L"
#> 
#> $sub_vars
#> [1] "PRECIPmm" "ETmm"    
#> 
#> $hru_vars
#> [1] "PRECIPmm"
#> 
#> $hrus
#> [1] 1 2 3
#> 
#> $output_start_date
#> [1] "2011-01-01"
#> 
# }
```
