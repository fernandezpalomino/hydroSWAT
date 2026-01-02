# Write example SWAT TxtInOut files to a local folder

Extracts the example SWAT input files stored in
[swat_txtinout_data](https://fernandezpalomino.github.io/hydroSWAT/reference/swat_txtinout_data.md)
and writes them under the `TxtInOut` subfolder of `path`. This provides
a self-contained example SWAT project for testing and tutorials.

## Usage

``` r
get_swat_example(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  character. Destination directory where files will be written. Defaults
  to the current working directory.

- overwrite:

  logical. Whether to overwrite existing files. Defaults to `FALSE`.

## Value

Invisibly returns the full path to the created `TxtInOut` directory.

## See also

[swat_txtinout_data](https://fernandezpalomino.github.io/hydroSWAT/reference/swat_txtinout_data.md)

Other Project setup and execution:
[`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md),
[`run_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md),
[`run_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md),
[`setup_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md)

## Examples

``` r
# \donttest{
# Write files to a temporary directory
path <- tempdir()
get_swat_example(path)
#> SWAT TxtInOut files written to: /tmp/RtmpUu15Uv/TxtInOut

# Inspect files
list.files(file.path(path, "TxtInOut"))
#>   [1] "000010000.pnd"  "000010000.rte"  "000010000.sub"  "000010000.swq" 
#>   [5] "000010000.wgn"  "000010000.wus"  "000010001.chm"  "000010001.gw"  
#>   [9] "000010001.hru"  "000010001.mgt"  "000010001.sdr"  "000010001.sep" 
#>  [13] "000010001.sol"  "000010002.chm"  "000010002.gw"   "000010002.hru" 
#>  [17] "000010002.mgt"  "000010002.sdr"  "000010002.sep"  "000010002.sol" 
#>  [21] "000010003.chm"  "000010003.gw"   "000010003.hru"  "000010003.mgt" 
#>  [25] "000010003.sdr"  "000010003.sep"  "000010003.sol"  "000010004.chm" 
#>  [29] "000010004.gw"   "000010004.hru"  "000010004.mgt"  "000010004.sdr" 
#>  [33] "000010004.sep"  "000010004.sol"  "000020000.pnd"  "000020000.rte" 
#>  [37] "000020000.sub"  "000020000.swq"  "000020000.wgn"  "000020000.wus" 
#>  [41] "000020001.chm"  "000020001.gw"   "000020001.hru"  "000020001.mgt" 
#>  [45] "000020001.sdr"  "000020001.sep"  "000020001.sol"  "000020002.chm" 
#>  [49] "000020002.gw"   "000020002.hru"  "000020002.mgt"  "000020002.sdr" 
#>  [53] "000020002.sep"  "000020002.sol"  "000020003.chm"  "000020003.gw"  
#>  [57] "000020003.hru"  "000020003.mgt"  "000020003.sdr"  "000020003.sep" 
#>  [61] "000020003.sol"  "000020004.chm"  "000020004.gw"   "000020004.hru" 
#>  [65] "000020004.mgt"  "000020004.sdr"  "000020004.sep"  "000020004.sol" 
#>  [69] "000030000.pnd"  "000030000.rte"  "000030000.sub"  "000030000.swq" 
#>  [73] "000030000.wgn"  "000030000.wus"  "000030001.chm"  "000030001.gw"  
#>  [77] "000030001.hru"  "000030001.mgt"  "000030001.sdr"  "000030001.sep" 
#>  [81] "000030001.sol"  "000030002.chm"  "000030002.gw"   "000030002.hru" 
#>  [85] "000030002.mgt"  "000030002.sdr"  "000030002.sep"  "000030002.sol" 
#>  [89] "ATMO.ATM"       "basins.bsn"     "basins.wwq"     "bmp-ri.out"    
#>  [93] "bmp-sedfil.out" "chan.deg"       "cst.cst"        "fert.dat"      
#>  [97] "fig.fig"        "file.cio"       "fin.fin"        "hru.dat"       
#> [101] "hyd.out"        "input.std"      "lup.dat"        "output.hru"    
#> [105] "output.pes"     "output.rch"     "output.rsv"     "output.sub"    
#> [109] "pcp1.pcp"       "pest.dat"       "plant.dat"      "rch.dat"       
#> [113] "rsv.dat"        "schema.ini"     "septic.out"     "septwq.dat"    
#> [117] "sub.dat"        "swat.qst"       "till.dat"       "tmp1.tmp"      
#> [121] "urban.dat"     
# }
```
