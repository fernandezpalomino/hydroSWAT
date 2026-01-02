# Calibrating SWAT with hydroSWAT: A complete workflow

``` r
# Load required packages
library(hydroSWAT)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
```

## 1. Introduction

This vignette presents a complete and reproducible workflow for
calibrating a SWAT model using the `hydroSWAT` R package. The example
dataset corresponds to the Huancane River Basin in southern Peru,
modeled using QSWAT.

The workflow includes setting up a calibration project, configuring
simulation periods and outputs, defining multi-objective criteria,
running the NSGA-II algorithm, evaluating performance, and analyzing
hydrological outputs.

> **Important note on execution**  
> Due to external dependencies, parallel execution, and computational
> cost, this vignette is not run automatically during package checks and
> is intended for interactive use by the user.

## 2. Set up the environment and load the data

Prepare the SWAT executable and load the example dataset, including
spatial inputs and observed streamflow data.

> **Note:**  
> `hydroSWAT` requires a compiled SWAT executable (`swat.exe`).  
> - On **Windows**, a pre‑compiled executable can be downloaded
> automatically with
> [`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md).  
> - On **Linux** and **macOS**, users must **compile the SWAT source
> code** for their system and provide the executable path.  
> Source code is available at <https://swat.tamu.edu/software/swat/>.  
> Without a valid executable, model simulations and calibration cannot
> be performed.

``` r
# Define working directory for the example
wd <- tempdir()

# Download the SWAT executable
swat_exe <- download_swat_exe(dest_dir = wd, type = "release")

# Extract the example SWAT project
swat_project <- get_swat_example(path = wd)

# Load spatial data
plot(subs1["Subbasin"])
plot(riv1["Subbasin"])

# Inspect observed flow data
head(qobserved)
```

## 3. Create calibration project

Initialize a new calibration project by copying the input files to a
working directory and setting it as the current workspace.

``` r
create_calibration_project(
  swat_TxtInOut = swat_project,
  destination_dir = wd,
  project_name = "calibration_example",
  set_working_dir = TRUE
)
```

## 4. Set up the simulation

Define the simulation period, time step, and which output variables to
print in SWAT model runs.

``` r
setup_swat_out <- setup_swat(
  sim_start_date = "2010-01-01",
  sim_end_date = "2013-12-31",
  time_step = "daily",
  NYSKIP = 1,
  rch_vars = c("FLOW_OUTcms"),
  sub_vars = c("PRECIPmm"),
  hru_vars = c("PRECIPmm"),
  hrus = c(1)
)
```

## 5. Define calibration objectives

Define the objective functions, target variables, and time periods used
to evaluate model performance during calibration.

``` r
# Define the objective functions, target variables, and evaluation periods
objectives <- list(
  list(
    element = "rch",
    variable = "FLOW_OUTcms",
    target_id = 3,
    metric = "NSE",
    calib_time_step = "daily",
    calib_start_date = "2011-01-01",
    calib_end_date = "2013-12-31"
  ),
  list(
    element = "rch",
    variable = "FLOW_OUTcms",
    target_id = 3,
    metric = "log_NSE",
    calib_time_step = "daily",
    calib_start_date = "2011-01-01",
    calib_end_date = "2013-12-31"
  )
)

# Prepare observed data in the format required by hydroSWAT for calibration
observed_data <- list(
  daily = list(
    rch = list(
      "FLOW_OUTcms" = tibble(
        Date = qobserved$Date,
        value = qobserved$Flow,
        target_id = 3
      )
    )
  )
)

# Configure the simulated variable to be extracted from SWAT outputs.
# This ensures hydroSWAT can compare the simulated output with 
# the observed data defined above.
output_config <- list(
  rch = list(
    file = "output.rch",
    variable = c("FLOW_OUTcms"),
    target_id = c(3),
    time_step = "daily",
    output_start_date = setup_swat_out$output_start_date
  )
)
```

## 6. Define model parameters

List the SWAT model parameters to be calibrated, including the
calibration method (e.g., absolute value or relative change), acceptable
ranges, and target model components.

``` r
parameter_info <- tibble(
  component = c(
    ".gw", ".gw", ".gw", ".gw",
    ".hru", ".hru", ".mgt"
  ),
  parameter = c(
    "GW_DELAY", "ALPHA_BF", "GWQMN", "RCHRG_DP",
    "SURLAG", "ESCO", "CN2"
  ),
  value = NA,
  method = c("v", "v", "v", "v", "v", "v", "r"),
  version = rep("SWAT", 7),
  plant_type = NA,
  min = c(10, 0.5, 700, 0.05, 1, 0.9, -0.05),
  max = c(50, 1, 750, 0.5, 2, 1, 0.05)
)
```

## 7. Run multi-objective calibration

This step performs the core calibration procedure using the NSGA-II
evolutionary algorithm. The algorithm explores combinations of model
parameters and evaluates them against multiple performance objectives
(e.g., NSE, log-NSE). The result is a Pareto-optimal set of solutions
that represent trade-offs between all objectives.

In this example, the calibration uses 8 parallel cores, with a
population size of 50 over 5 generations (i.e., 250 total model
evaluations). On a Dell XPS 15 9510 laptop (Intel Core i9-11900H, 8
cores, 2.50 GHz, 64 GB RAM), this configuration required approximately 2
minutes.

``` r
# Arguments for objective function evaluation
fn_args <- list(
  parameter_info = parameter_info,
  observed_data = observed_data,
  output_config = output_config,
  objectives = objectives,
  subbasins = NULL,
  swat_exe_path = swat_exe
)

# Run NSGA-II calibration
t_start <- Sys.time()

set.seed(123)
calibration_result <- calibrate_swat(
  fn = hydroSWAT:::calculate_objectives,
  varNo = nrow(parameter_info),
  objDim = length(objectives),
  lowerBounds = parameter_info$min,
  upperBounds = parameter_info$max,
  popSize = 50,
  generations = 5,
  TxtInOut_dir = getwd(),
  cores = 8,
  fn_args = fn_args
)

t_end <- Sys.time()
print(t_end - t_start)
```

## 8. Identify best simulation and visualize Pareto front

Extract the best compromise solution from the Pareto front and visualize
the performance trade-offs across objectives.

``` r
best_solution <- best_compromise_solution(calibration_result$objectives)
best_parameters <- calibration_result$parameters[best_solution$index, ]
parameter_info$value <- best_parameters

# Plot Pareto solutions and best compromise solution
best_objectives <- calibration_result$objectives[best_solution$index, ]
plot(
  calibration_result$objectives,
  xlab = "1-log_NSE", ylab = "1-NSE",
  col = "gray50", pch = 19
)
points(
  calibration_result$objectives[calibration_result$paretoFrontRank == 1, ],
  col = "black", pch = 19
)
points(
  best_objectives[1], best_objectives[2],
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
```

## 9. Run the model with optimal parameters for evaluation

Run the SWAT model once using the best-performing parameter set obtained
from the multi-objective calibration.

``` r
setup_swat_out <- setup_swat(
  sim_start_date = "2010-01-01",
  sim_end_date = "2015-12-31",
  time_step = "daily",
  NYSKIP = 1,
  rch_vars = c("FLOW_OUTcms"),
  sub_vars = c("PRECIPmm", "ETmm", "WYLDmm"),
  hru_vars = c("PRECIPmm", "ETmm", "WYLDmm")
)

output_rch_args <- list(
  file = "output.rch",
  variable = c("FLOW_OUTcms"),
  target_id = c(3),
  time_step = "daily",
  output_start_date = setup_swat_out$output_start_date
)

sim_best <- run_swat(
  TxtInOut_dir = getwd(),
  swat_exe_path = fn_args$swat_exe_path,
  execution_mode = "single_sample",
  parameter_info = parameter_info,
  output_rch_args = output_rch_args
)
```

## 10. Evaluate model performance

Evaluate the calibrated model using up to 22 hydrological performance
metrics, automatically applied to the user-defined calibration and
validation periods. These metrics include widely used criteria such as
the Nash–Sutcliffe Efficiency (NSE), Kling-Gupta Efficiency (KGE), Mean
Absolute Error (MAE), and Percent Bias (PBIAS), and are complemented
with diagnostic plots, such as hydrographs, flow duration curves, and
seasonal cycles, for comprehensive model evaluation.

``` r
evaluation_results <- evaluate_swat(
  rch_output = sim_best$rch,
  observed_data = observed_data$daily$rch$FLOW_OUTcms,
  target_id = 3,
  variable = "FLOW_OUTcms",
  metrics = c("NSE", "KGE", "MAE", "PBIAS"),
  start_dates = c("2011-01-01", "2014-01-01"),
  end_dates = c("2013-12-31", "2015-12-31")
)
print(evaluation_results$evaluation_metric)

plot_timeseries(evaluation_results$daily_data)
plot_timeseries(evaluation_results$monthly_data)
plot_fdc(evaluation_results$daily_fdc_data)
plot_mean_annual_cycle(evaluation_results$mean_annual_cycle)
```

## 11. Analyze hydrological outputs

This section illustrates how to read, summarize, and visualize mean
annual outputs from the SWAT model at three spatial levels: HRU,
subbasin, and river reach.

### HRU-level analysis

Read and summarize hydrological outputs at the Hydrological Response
Unit (HRU) level. Results are aggregated to basin and land use types.

``` r
# Read and summarize HRU-level outputs
hru_daily_data <- output_hru(
  file = "output.hru",
  time_step = "daily",
  output_start_date = "2012-01-01"
)

summary_hru <- summarize_swat_output(hru_daily_data)

# Print mean annual values aggregated at the basin level and by land use
print(summary_hru$mean_annual_basin)
print(summary_hru$mean_annual_LULC)
```

### Subbasin-level analysis

Summarize outputs at the subbasin level and join results with spatial
data to create maps of annual averages.

``` r
# Read and summarize subbasin-level outputs
subs_daily_data <- output_sub(
  file = "output.sub",
  time_step = "daily",
  output_start_date = "2012-01-01"
)

summary_sub <- summarize_swat_output(subs_daily_data)

# Join subbasin results to shapefile
sub_data <- subs1 %>%
  left_join(summary_sub$mean_annual, by = c("Subbasin" = "SUB")) %>%
  dplyr::select(Subbasin, PRECIPmm, ETmm, WYLDmm) %>%
  tidyr::pivot_longer(
    cols = -c(Subbasin, geometry),
    names_to = "Variable", values_to = "Value"
  )

# Visualize subbasin-level results
ggplot(sub_data) +
  geom_sf(aes(fill = Value)) +
  facet_wrap(~Variable) +
  scale_fill_viridis_c(name = "mm/year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

### River reach-level analysis

Summarize mean annual discharge per river segment and display the
results on a map using the river network shapefile.

``` r
# Read and summarize reach-level (river) outputs
riv_daily_data <- output_rch(
  file = "output.rch",
  time_step = "daily",
  output_start_date = "2012-01-01"
)

summary_rch <- summarize_swat_output(riv_daily_data)

# Join discharge results to river shapefile
riv_data <- riv1 %>%
  dplyr::select(Subbasin) %>%
  left_join(summary_rch$mean_annual, by = c("Subbasin" = "RCH"))

# Visualize mean annual discharge by river segment
ggplot() +
  geom_sf(data = subs1, fill = NA, color = "black",
          linetype = "dashed", size = 0.3) +
  geom_sf(data = riv_data, aes(geometry = geometry, color = FLOW_OUTcms)) +
  scale_color_viridis_c(name = "Discharge (m³/s)") +
  theme_minimal()
```

## 12. Final notes

- The SWAT executable is downloaded using
  [`download_swat_exe()`](https://fernandezpalomino.github.io/hydroSWAT/reference/download_swat_exe.md),
  but users may also provide the path to their preferred version.
- This workflow is fully reproducible with the example dataset.
- A fully commented interactive script is provided in:
  inst/scripts/calibrating_swat.R
- Cite `hydroSWAT` if used in scientific publications.
