# Package index

## Preprocessing tools

Functions for preparing SWAT model input data, including climate
forcings, parameter files, and TxtInOut inputs.

- [`areal_mean()`](https://fernandezpalomino.github.io/hydroSWAT/reference/areal_mean.md)
  : Compute areal means of raster data over polygons
- [`pcp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat_editor.md)
  : Generate SWAT Editor-compatible precipitation input files
- [`tmp2swat_editor()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat_editor.md)
  : Generate SWAT Editor-compatible temperature input files
- [`pcp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pcp2swat.md)
  : Update SWAT precipitation input files (\`pcp\*.pcp\`)
- [`tmp2swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/tmp2swat.md)
  : Update SWAT temperature input files (\`tmp\*.tmp\`)

## Project setup and execution

Tools for configuring, initializing, and running SWAT projects,
including example datasets and model executables.

- [`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md)
  : Download SWAT executable
- [`get_swat_example()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_example.md)
  : Write example SWAT TxtInOut files to a local folder
- [`setup_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/setup_swat.md)
  : Configure a SWAT project for simulation
- [`run_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat.md)
  : Run SWAT model with or without parameter updates
- [`run_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/run_swat_exe.md)
  : Run SWAT executable

## Parameter management

Functions for retrieving, modifying, and organizing SWAT model
parameters, including support for parallel calibration workflows.

- [`get_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_parameters.md)
  : Retrieve SWAT model parameters
- [`change_parameter()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_parameter.md)
  : Change a single SWAT model parameter
- [`change_multiple_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/change_multiple_parameters.md)
  : Change multiple SWAT model parameters
- [`split_swat_parameters()`](https://fernandezpalomino.github.io/hydroSWAT/reference/split_swat_parameters.md)
  : Split SWAT parameters for parallel processing

## Multiobjective calibration

Multiobjective calibration tools for SWAT based on the NSGA-II
evolutionary algorithm.

- [`create_calibration_project()`](https://fernandezpalomino.github.io/hydroSWAT/reference/create_calibration_project.md)
  : Create a SWAT calibration project
- [`calibrate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calibrate_swat.md)
  : Perform multiobjective calibration of a SWAT model with NSGA-II
- [`calculate_objectives()`](https://fernandezpalomino.github.io/hydroSWAT/reference/calculate_objectives.md)
  : Compute calibration objectives for the SWAT model
- [`best_compromise_solution()`](https://fernandezpalomino.github.io/hydroSWAT/reference/best_compromise_solution.md)
  : Select a best-compromise Pareto solution by Euclidean distance

## Performance evaluation

Model performance assessment using classical goodness-of-fit metrics and
hydrological signatures, including Flow Duration Curve (FDC)–based
diagnostics.

- [`evaluate_swat()`](https://fernandezpalomino.github.io/hydroSWAT/reference/evaluate_swat.md)
  : Evaluate SWAT model performance
- [`gof_metrics()`](https://fernandezpalomino.github.io/hydroSWAT/reference/gof_metrics.md)
  : Goodness-of-fit (GOF) metrics and FDC-based signatures
- [`fdc_signature()`](https://fernandezpalomino.github.io/hydroSWAT/reference/fdc_signature.md)
  : Flow Duration Curve (FDC) signature
- [`pbias_fdc_midsegment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_midsegment.md)
  : Percent bias in the midsegment slope of a Flow Duration Curve (FDC)
- [`pbias_fdc_target_segment()`](https://fernandezpalomino.github.io/hydroSWAT/reference/pbias_fdc_target_segment.md)
  : Percent bias in a Flow Duration Curve (FDC) segment
- [`trmse()`](https://fernandezpalomino.github.io/hydroSWAT/reference/trmse.md)
  : Transformed root mean squared error (TRMSE)
- [`plot_timeseries()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_timeseries.md)
  : Plot time series of simulated and observed data
- [`plot_mean_annual_cycle()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_mean_annual_cycle.md)
  : Plot mean annual cycle by evaluation periods
- [`plot_fdc()`](https://fernandezpalomino.github.io/hydroSWAT/reference/plot_fdc.md)
  : Plot Flow Duration Curve (FDC) by evaluation periods

## Output readers

Functions for reading and parsing SWAT output files at different spatial
levels (HRU, subbasin, and reach).

- [`get_swat_vars()`](https://fernandezpalomino.github.io/hydroSWAT/reference/get_swat_vars.md)
  : Retrieve SWAT output variables
- [`output_rch()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_rch.md)
  : Read SWAT reach output
- [`output_sub()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_sub.md)
  : Read SWAT subbasin output
- [`output_hru()`](https://fernandezpalomino.github.io/hydroSWAT/reference/output_hru.md)
  : Read SWAT HRU output
- [`read_pcp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_pcp.md)
  : Read precipitation data from SWAT .pcp files
- [`read_tmp()`](https://fernandezpalomino.github.io/hydroSWAT/reference/read_tmp.md)
  : Read temperature data from SWAT .tmp files

## Output analysis

Tools for aggregating, summarizing, and analyzing SWAT simulation
outputs for further interpretation and visualization.

- [`summarize_swat_output()`](https://fernandezpalomino.github.io/hydroSWAT/reference/summarize_swat_output.md)
  : Summarize SWAT output data

## Data

Example datasets bundled with the package for demonstrations, testing,
and reproducible vignettes.

- [`example_basin_datasets`](https://fernandezpalomino.github.io/hydroSWAT/reference/example_basin_datasets.md)
  : Datasets included with the package for demos, vignettes, and tests.
  Unless otherwise noted, they correspond to the \*\*Huancane River
  basin (Peru)\*\*.
- [`gridded_climate_netcdfs`](https://fernandezpalomino.github.io/hydroSWAT/reference/gridded_climate_netcdfs.md)
  : Gridded climate NetCDFs (precipitation and temperature)
- [`subs1`](https://fernandezpalomino.github.io/hydroSWAT/reference/subs1.md)
  : Subbasins (sf)
- [`riv1`](https://fernandezpalomino.github.io/hydroSWAT/reference/riv1.md)
  : River network (sf)
- [`swat_txtinout_data`](https://fernandezpalomino.github.io/hydroSWAT/reference/swat_txtinout_data.md)
  : SWAT TxtInOut inputs
- [`qobserved`](https://fernandezpalomino.github.io/hydroSWAT/reference/qobserved.md)
  : Observed streamflow (daily)
