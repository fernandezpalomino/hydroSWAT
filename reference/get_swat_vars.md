# Retrieve SWAT output variables

Returns a list of SWAT output variables by category (reach, subbasin, or
HRU), or all combined. Useful for filtering when reading SWAT output
files.

## Usage

``` r
get_swat_vars(type = c("all", "rch", "sub", "hru"))
```

## Arguments

- type:

  Character. Category of variables to return:

  - "all": Returns all available variables (default).

  - "rch": Returns variables related to reach output.

  - "sub": Returns variables related to subbasin output.

  - "hru": Returns variables related to hydrologic response units (HRU).

## Value

A character vector of variable names for the selected category. If
`"all"` is selected, a list of vectors for each category is returned.

## See also

Other Output readers:
[`output_hru()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md),
[`output_rch()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md),
[`output_sub()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md),
[`read_pcp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_pcp.md),
[`read_tmp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_tmp.md)

## Examples

``` r
# Get all SWAT variables (list by category)
get_swat_vars("all")
#> $rch
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
#> 
#> $sub
#>  [1] "COLNAME"    "SUB"        "GIS"        "MON"        "AREAkm2"   
#>  [6] "PRECIPmm"   "SNOMELTmm"  "PETmm"      "ETmm"       "SWmm"      
#> [11] "PERCmm"     "SURQmm"     "GW_Qmm"     "WYLDmm"     "SYLDt/ha"  
#> [16] "ORGNkg/ha"  "ORGPkg/ha"  "NSURQkg/ha" "SOLPkg/ha"  "SEDPkg/ha" 
#> [21] "LAT Q(mm)"  "LATNO3kg/h" "GWNO3kg/ha" "CHOLAmic/L" "CBODU mg/L"
#> [26] "DOXQ mg/L"  "TNO3kg/ha"  "QTILEmm"    "TVAPkg/ha" 
#> 
#> $hru
#>  [1] "LULC"       "HRU"        "GIS"        "SUB"        "MGT"       
#>  [6] "MON"        "AREAkm2"    "PRECIPmm"   "SNOFALLmm"  "SNOMELTmm" 
#> [11] "IRRmm"      "PETmm"      "ETmm"       "SW_INITmm"  "SW_ENDmm"  
#> [16] "PERCmm"     "GW_RCHGmm"  "DA_RCHGmm"  "REVAPmm"    "SA_IRRmm"  
#> [21] "DA_IRRmm"   "SA_STmm"    "DA_STmm"    "SURQ_GENmm" "SURQ_CNTmm"
#> [26] "TLOSSmm"    "LATQGENmm"  "GW_Qmm"     "WYLDmm"     "DAILYCN"   
#> [31] "TMP_AVdgC"  "TMP_MXdgC"  "TMP_MNdgC"  "SOL_TMPdgC" "SOLARMJ/m2"
#> [36] "SYLDt/ha"   "USLEt/ha"   "N_APPkg/ha" "P_APPkg/ha" "NAUTOkg/ha"
#> [41] "PAUTOkg/ha" "NGRZkg/ha"  "PGRZkg/ha"  "NCFRTkg/ha" "PCFRTkg/ha"
#> [46] "NRAINkg/ha" "NFIXkg/ha"  "F-MNkg/ha"  "A-MNkg/ha"  "A-SNkg/ha" 
#> [51] "F-MPkg/ha"  "AO-LPkg/ha" "L-APkg/ha"  "A-SPkg/ha"  "DNITkg/ha" 
#> [56] "NUPkg/ha"   "PUPkg/ha"   "ORGNkg/ha"  "ORGPkg/ha"  "SEDPkg/ha" 
#> [61] "NSURQkg/ha" "NLATQkg/ha" "NO3Lkg/ha"  "NO3GWkg/ha" "SOLPkg/ha" 
#> [66] "P_GWkg/ha"  "W_STRS"     "TMP_STRS"   "N_STRS"     "P_STRS"    
#> [71] "BIOMt/ha"   "LAI"        "YLDt/ha"    "BACTPct"    "BACTLPct"  
#> [76] "WTAB_CLIm"  "WTAB_SOLm"  "SNOmm"      "CMUPkg/ha"  "CMTOTkg/ha"
#> [81] "QTILEmm"    "TNO3kg/ha"  "LNO3kg/ha"  "GW_Q_Dmm"   "LATQCNTmm" 
#> [86] "TVAPkg/ha" 
#> 

# Get variables for Reach (output.rch)
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

# Get variables for Subbasin (output.sub)
get_swat_vars("sub")
#>  [1] "COLNAME"    "SUB"        "GIS"        "MON"        "AREAkm2"   
#>  [6] "PRECIPmm"   "SNOMELTmm"  "PETmm"      "ETmm"       "SWmm"      
#> [11] "PERCmm"     "SURQmm"     "GW_Qmm"     "WYLDmm"     "SYLDt/ha"  
#> [16] "ORGNkg/ha"  "ORGPkg/ha"  "NSURQkg/ha" "SOLPkg/ha"  "SEDPkg/ha" 
#> [21] "LAT Q(mm)"  "LATNO3kg/h" "GWNO3kg/ha" "CHOLAmic/L" "CBODU mg/L"
#> [26] "DOXQ mg/L"  "TNO3kg/ha"  "QTILEmm"    "TVAPkg/ha" 

# Get variables for HRU (output.hru)
get_swat_vars("hru")
#>  [1] "LULC"       "HRU"        "GIS"        "SUB"        "MGT"       
#>  [6] "MON"        "AREAkm2"    "PRECIPmm"   "SNOFALLmm"  "SNOMELTmm" 
#> [11] "IRRmm"      "PETmm"      "ETmm"       "SW_INITmm"  "SW_ENDmm"  
#> [16] "PERCmm"     "GW_RCHGmm"  "DA_RCHGmm"  "REVAPmm"    "SA_IRRmm"  
#> [21] "DA_IRRmm"   "SA_STmm"    "DA_STmm"    "SURQ_GENmm" "SURQ_CNTmm"
#> [26] "TLOSSmm"    "LATQGENmm"  "GW_Qmm"     "WYLDmm"     "DAILYCN"   
#> [31] "TMP_AVdgC"  "TMP_MXdgC"  "TMP_MNdgC"  "SOL_TMPdgC" "SOLARMJ/m2"
#> [36] "SYLDt/ha"   "USLEt/ha"   "N_APPkg/ha" "P_APPkg/ha" "NAUTOkg/ha"
#> [41] "PAUTOkg/ha" "NGRZkg/ha"  "PGRZkg/ha"  "NCFRTkg/ha" "PCFRTkg/ha"
#> [46] "NRAINkg/ha" "NFIXkg/ha"  "F-MNkg/ha"  "A-MNkg/ha"  "A-SNkg/ha" 
#> [51] "F-MPkg/ha"  "AO-LPkg/ha" "L-APkg/ha"  "A-SPkg/ha"  "DNITkg/ha" 
#> [56] "NUPkg/ha"   "PUPkg/ha"   "ORGNkg/ha"  "ORGPkg/ha"  "SEDPkg/ha" 
#> [61] "NSURQkg/ha" "NLATQkg/ha" "NO3Lkg/ha"  "NO3GWkg/ha" "SOLPkg/ha" 
#> [66] "P_GWkg/ha"  "W_STRS"     "TMP_STRS"   "N_STRS"     "P_STRS"    
#> [71] "BIOMt/ha"   "LAI"        "YLDt/ha"    "BACTPct"    "BACTLPct"  
#> [76] "WTAB_CLIm"  "WTAB_SOLm"  "SNOmm"      "CMUPkg/ha"  "CMTOTkg/ha"
#> [81] "QTILEmm"    "TNO3kg/ha"  "LNO3kg/ha"  "GW_Q_Dmm"   "LATQCNTmm" 
#> [86] "TVAPkg/ha" 
```
