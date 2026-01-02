# -------------------------------------------------------------------------
# calibrate_swat
# -------------------------------------------------------------------------
#' @title Perform multiobjective calibration of a SWAT model with NSGA-II
#'
#' @description
#' Calibrates a SWAT model using the Non-dominated Sorting Genetic Algorithm
#' II (NSGA-II). The function builds on \pkg{nsga2R} and parallelizes the
#' evaluation of candidate parameter sets during initialization and across
#' generations.
#'
#' @param fn function. Multiobjective function to minimize (custom or
#'   \link{calculate_objectives}). For a custom \code{fn}, the user is
#'   responsible for validating \code{fn_args}, and the function must
#'   return a numeric vector of length \code{objDim}.
#' @param varNo integer. Number of decision variables (model parameters).
#' @param objDim integer. Number of objective functions.
#' @param lowerBounds numeric. Lower bounds for decision variables.
#' @param upperBounds numeric. Upper bounds for decision variables.
#' @param popSize integer. Population size (default \code{100}).
#' @param tourSize integer. Tournament size (default \code{2}).
#' @param generations integer. Number of generations (default \code{10}).
#' @param cprob numeric. Crossover probability (default \code{0.7}).
#' @param XoverDistIdx numeric (\eqn{\geq} 0). Crossover distribution index
#'   (default \code{5}).
#' @param mprob numeric. Mutation probability (default \code{0.2}).
#' @param MuDistIdx numeric (\eqn{\geq} 0). Mutation distribution index
#'   (default \code{10}).
#' @param TxtInOut_dir character. Path to the SWAT \code{TxtInOut} directory.
#' @param cores integer. Cores used for parallel evaluation (default \code{1}).
#' @param fn_args list. Inputs required by \code{fn} (e.g., parameter table,
#'   observed data, output-reading config, objectives, SWAT exe path).
#'   The required structure depends on \code{fn}. For
#'   \code{calculate_objectives} see \link{calculate_objectives}.
#' @param required_libraries character. Extra packages to load on each worker
#'   for custom \code{fn} (default \code{NULL}).
#'
#' @details
#' To calibrate a SWAT model, first create a calibration project from a valid
#' SWAT \code{TxtInOut} folder with \link{create_calibration_project}, set that
#' folder as the working directory, and configure simulation/output settings
#' with \link{setup_swat}.
#'
#' @return
#' An object of class \code{"nsga2R"} with the usual NSGA-II settings and:
#' \itemize{
#'   \item \code{parameters}: Non-dominated decision vectors.
#'   \item \code{objectives}: Objective values for those vectors.
#'   \item \code{paretoFrontRank}: Nondomination rank per solution.
#'   \item \code{crowdingDistance}: Crowding distance per solution.
#' }
#' For reproducibility, \code{TxtInOut_dir}, \code{cores}, and \code{fn_args}
#' are attached.
#'
#' @family Multiobjective calibration
#' @seealso \link{run_swat}, \link{run_swat_exe}, \link{setup_swat};
#'   see the vignette: \code{vignette("Calibrating-SWAT-with-hydroSWAT-A-complete-workflow", "hydroSWAT")}.
#'
#' @references
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. (2002).
#' A fast and elitist multiobjective genetic algorithm: NSGA-II.
#' \emph{IEEE Trans. Evol. Comput.}, 6(2), 182–197.
#'
#' @examples
#' \donttest{
#' # Windows-only example (downloads SWAT EXE). On non-Windows, skip.
#' if (.Platform$OS.type == "windows") {
#'
#'   tmpdir <- tempdir()
#'   swat_project <- get_swat_example(tmpdir)
#'
#'   create_calibration_project(
#'     swat_TxtInOut   = swat_project,
#'     destination_dir = tmpdir,
#'     project_name    = "calibration_example",
#'     set_working_dir = TRUE
#'   )
#'
#'   # SWAT executable (if download fails, set your local path below)
#'   swat_exe <- tryCatch(
#'     download_swat_exe(dest_dir = tmpdir, type = "release"),
#'     error = function(e) {
#'       message("download_swat_exe() failed: ", conditionMessage(e))
#'       "path/to/swat.exe"  # <-- set your local executable
#'     }
#'   )
#'
#'   # Simulation setup (warm-up = 1 year)
#'   setup_swat_out <- setup_swat(
#'     sim_start_date = "2010-01-01",
#'     sim_end_date   = "2013-12-31",
#'     time_step      = "daily",
#'     NYSKIP         = 1,
#'     rch_vars       = c("FLOW_OUTcms")
#'   )
#'
#'   # Objectives (NSE and log_NSE at reach 3)
#'   objectives <- list(
#'     list(
#'       element = "rch", variable = "FLOW_OUTcms", target_id = 3,
#'       metric  = "NSE", calib_time_step = "daily",
#'       calib_start_date = "2011-01-01", calib_end_date = "2013-12-31"
#'     ),
#'     list(
#'       element = "rch", variable = "FLOW_OUTcms", target_id = 3,
#'       metric  = "log_NSE", calib_time_step = "daily",
#'       calib_start_date = "2011-01-01", calib_end_date = "2013-12-31"
#'     )
#'   )
#'
#'   # Observed flows (qobserved) included in the package
#'   observed_data <- list(
#'     daily = list(
#'       rch = list(
#'         "FLOW_OUTcms" = tibble::tibble(
#'           Date = qobserved$Date,
#'           value = qobserved$Flow,
#'           target_id = 3
#'         )
#'       )
#'     )
#'   )
#'
#'   # What to read from SWAT outputs
#'   output_config <- list(
#'     rch = list(
#'       file = "output.rch",
#'       variable = c("FLOW_OUTcms"),
#'       target_id = c(3),
#'       time_step = "daily",
#'       output_start_date = setup_swat_out$output_start_date
#'     )
#'   )
#'
#'   # Parameters and bounds
#'   parameter_info <- tibble::tibble(
#'     component  = c(".gw", ".gw", ".gw", ".gw", ".hru", ".hru", ".mgt"),
#'     parameter  = c("GW_DELAY", "ALPHA_BF", "GWQMN", "RCHRG_DP",
#'                    "SURLAG", "ESCO", "CN2"),
#'     value      = NA,
#'     method     = c("v", "v", "v", "v", "v", "v", "r"),
#'     version    = rep("SWAT", 7),
#'     plant_type = NA,
#'     min        = c(10, 0.50, 700, 0.05, 1.0, 0.90, -0.05),
#'     max        = c(50, 1.00, 750, 0.50, 2.0, 1.00,  0.05)
#'   )
#'
#'   # Arguments for the objective function
#'   fn_args <- list(
#'     parameter_info = parameter_info,
#'     observed_data  = observed_data,
#'     output_config  = output_config,
#'     objectives     = objectives,
#'     subbasins      = NULL,
#'     swat_exe_path  = swat_exe
#'   )
#'
#'   # Run NSGA-II calibration (small demo)
#'   set.seed(123)
#'   calibration_result <- calibrate_swat(
#'     fn           = calculate_objectives,
#'     varNo        = nrow(parameter_info),
#'     objDim       = length(objectives),
#'     lowerBounds  = parameter_info$min,
#'     upperBounds  = parameter_info$max,
#'     popSize      = 10,
#'     generations  = 2,
#'     TxtInOut_dir = getwd(),
#'     cores        = 2,
#'     fn_args      = fn_args
#'   )
#'
#'   # Identify optimal parameters and Pareto solutions
#'   bcs <- best_compromise_solution(calibration_result$objectives)
#'
#'   # Named best-parameter vector
#'   best_par <- as.numeric(
#'     calibration_result$parameters[bcs$index, , drop = TRUE]
#'   )
#'   names(best_par) <- parameter_info$parameter
#'   print(round(best_par, 3))
#'
#'   best_obj <- calibration_result$objectives[bcs$index, ]
#'
#'   plot(
#'     calibration_result$objectives,
#'     xlab = "1-log_NSE", ylab = "1-NSE",
#'     col = "gray50", pch = 19
#'   )
#'   points(
#'     calibration_result$objectives[
#'       calibration_result$paretoFrontRank == 1, ],
#'     col = "black", pch = 19
#'   )
#'   points(
#'     best_obj[1], best_obj[2],
#'     col = "blue", pch = 19, cex = 1.5
#'   )
#'   legend(
#'     "topright",
#'     legend = c(
#'       "Pareto Solutions (PS)",
#'       "Pareto Optimal Front (POF)",
#'       "Best Compromise Solution (BCS)"
#'     ),
#'     col = c("gray50", "black", "blue"),
#'     pch = 19, cex = 0.6, bty = "n"
#'   )
#' } else {
#'   message("Windows-only example. On non-Windows, compile SWAT and set 'swat_exe'.")
#' }
#' }
#'
#' @importFrom nsga2R nsga2R fastNonDominatedSorting crowdingDist4frnt
#'   tournamentSelection boundedSBXover boundedPolyMutation
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @export

calibrate_swat <- function(fn = calculate_objectives, varNo, objDim,
                           lowerBounds, upperBounds, popSize = 100,
                           tourSize = 2, generations = 10, cprob = 0.7,
                           XoverDistIdx = 5, mprob = 0.2, MuDistIdx = 10,
                           TxtInOut_dir = getwd(), cores = 1, fn_args = NULL,
                           required_libraries = NULL) {

  # Ensure the SWAT project is available
  validate_swat_directory(TxtInOut_dir, check_backup = TRUE)

  setwd(TxtInOut_dir)

  # Validate fn_args only if fn is the calculate_objectives from hydroSWAT
  if (identical(fn, get("calculate_objectives",
                        envir = asNamespace("hydroSWAT")))) {
    fn_args <- validate_fn_args(fn_args)

    message("fn_args successfully validated for 'calculate_objectives' function.")
  }

  if (cores > popSize) {
    stop(paste(
      "Error: 'cores' cannot be greater than 'popSize'.",
      "Please adjust the values and try again."
    ))
  }

  start_time <- Sys.time()
  print(start_time)

  # Run sequentially if cores = 1
  if (cores == 1) {
    cat("Running in sequential mode (1 core)...\n")

    # Wrapper for 'fn' with calibration data and working directory adjustment.
    fn_modified_seq <- function(parameter_values) {
      fn(parameter_values, fn_args)
    }

    result <- nsga2R(fn = fn_modified_seq, varNo, objDim, lowerBounds, upperBounds,
                     popSize, tourSize, generations, cprob, XoverDistIdx, mprob,
                     MuDistIdx)
  } else {

    # Run in parallel if cores > 1
    cat("Running in parallel mode with", cores, "cores...\n")

    # Validate required libraries
    required_libraries <- check_libraries(union(required_libraries, "hydroSWAT"))
    check_par_libs <- check_libraries(c("foreach", "doParallel"))

    # Wrapper for 'fn' with calibration data and working directory adjustment.
    fn_modified <- function(parameter_values, core_id, fn_args) {
      setwd(core_working_dirs[core_id])
      objectives <- fn(parameter_values, fn_args)
      return(objectives)
    }

    available_cores <- parallel::detectCores()
    cores_to_use <- min(cores, available_cores)
    cl <- parallel::makeCluster(cores_to_use)
    doParallel::registerDoParallel(cl)

    parallel::clusterExport(cl, varlist = "required_libraries", envir = environment())
    parallel::clusterEvalQ(cl, lapply(required_libraries, library, character.only = TRUE))

    # Create temporary directories for each core
    core_working_dirs <- create_core_working_dirs (cores_to_use, TxtInOut_dir)
    core_working_dirs_remove <- suppressWarnings(
      normalizePath(file.path(core_working_dirs, "..")))

    # Stop cluster and clean temp directories
    on.exit({
      parallel::stopCluster(cl)
      lapply(core_working_dirs_remove, unlink, recursive = TRUE)
    }, add = TRUE)

    # Initialize the population
    cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
    cat("\n")
    cat("initializing the population")
    cat("\n")

    # Initialize the population with random parameter values
    parent <- t(sapply(1:popSize, function(u) array(stats::runif(length(lowerBounds),
                                                                 lowerBounds, upperBounds))))
    parent_splits <- split_swat_parameters(cores = cores_to_use, parameter_sample = parent)

    # Export necessary functions/data to the cluster
    parallel::clusterExport(cl, varlist = c("fn", "fn_modified", "fn_args",
                                            "parent_splits", "core_working_dirs"),
                            envir = environment())

    # Process parameters in parallel
    parent_objs <- foreach::foreach(i = 1:cores_to_use, .combine = 'rbind') %dopar% {
      params_for_core <- parent_splits[[i]]
      results_for_core <- lapply(1:nrow(params_for_core), function(j) {
        fn_modified(params_for_core[j, ], i, fn_args)
      })
      do.call(rbind, results_for_core)
    }
    parent <- cbind(parent, parent_objs)

    # Rank the initial population
    cat("ranking the initial population")
    cat("\n")
    ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + objDim)])

    # Rank index for each chromosome
    rnkIndex <- integer(popSize)
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parent <- cbind(parent, rnkIndex)

    # Calculate crowding distance
    cat("crowding distance calculation")
    cat("\n")
    objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 2, max) -
      apply(parent[, (varNo + 1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parent, ranking, objRange)
    parent <- cbind(parent, apply(cd, 1, sum))

    # Begin generations loop
    for (iter in 1:generations) {
      cat("---------------generation---------------", iter, "starts")
      cat("\n")
      cat("tournament selection")
      cat("\n")
      matingPool <- tournamentSelection(parent, popSize, tourSize)

      cat("crossover operator")
      cat("\n")
      childAfterX <- boundedSBXover(matingPool[, 1:varNo], lowerBounds,
                                    upperBounds, cprob, XoverDistIdx)

      cat("mutation operator")
      cat("\n")
      childAfterM <- boundedPolyMutation(childAfterX, lowerBounds,
                                         upperBounds, mprob, MuDistIdx)

      cat("evaluate the objective fns of childAfterM")
      cat("\n")
      # Process each split of parameters in parallel
      childAfterM_splits <- split_swat_parameters(cores = cores_to_use,
                                                  parameter_sample = childAfterM)
      parallel::clusterExport(cl, varlist = c("childAfterM_splits"),
                              envir = environment())
      childAfterM_objs <- foreach::foreach(i = 1:cores_to_use,
                                           .combine = 'rbind') %dopar% {
                                             params_for_core <- childAfterM_splits[[i]]
                                             results_for_core <- lapply(1:nrow(params_for_core), function(j) {
                                               fn_modified(params_for_core[j, ], i, fn_args)
                                             })
                                             do.call(rbind, results_for_core)
                                           }
      childAfterM <- cbind(childAfterM,  childAfterM_objs)

      # Consider use child again and again ...
      cat("Rt = Pt + Qt")
      cat("\n")

      # Combine the parent with the childAfterM
      # (No need to retain the rnkIndex and cd of parent)
      parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)

      cat("ranking again")
      cat("\n")
      ranking <- fastNonDominatedSorting(parentNext[, (varNo +
                                                         1):(varNo + objDim)])
      i <- 1
      while (i <= length(ranking)) {
        rnkIndex[ranking[[i]]] <- i
        i <- i + 1
      }
      parentNext <- cbind(parentNext, rnkIndex)

      cat("crowded comparison again")
      cat("\n")
      objRange <- apply(parentNext[, (varNo + 1):(varNo + objDim)],2, max) -
        apply(parentNext[, (varNo + 1):(varNo + objDim)], 2, min)
      cd <- crowdingDist4frnt(parentNext, ranking, objRange)
      parentNext <- cbind(parentNext, apply(cd, 1, sum))
      parentNext.sort <- parentNext[order(parentNext[, varNo + objDim + 1],
                                          -parentNext[, varNo + objDim + 2]), ]

      cat("environmental selection")
      cat("\n")

      # choose the first 'popSize' rows for next generation
      parent <- parentNext.sort[1:popSize, ]

      cat("---------------generation---------------", iter, "ends")
      cat("\n")
      if (iter != generations) {
        cat("\n")
        cat("********** new iteration *********")
        cat("\n")

      } else {
        cat("********** stop the evolution *********")
        cat("\n")
      }
    }

    # report on nsga2 settings and results
    result = list(
      functions = fn,
      parameterDim = varNo,
      objectiveDim = objDim,
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      popSize = popSize,
      tournamentSize = tourSize,
      generations = generations,
      XoverProb = cprob,
      XoverDistIndex = XoverDistIdx,
      mutationProb = mprob,
      mutationDistIndex = MuDistIdx,
      parameters = parent[,1:varNo],
      objectives = parent[, (varNo + 1):(varNo + objDim)],
      paretoFrontRank = parent[, varNo + objDim + 1],
      crowdingDistance = parent[, varNo + objDim + 2]
    )
  }

  result$TxtInOut_dir <- TxtInOut_dir
  result$cores <- cores
  result$fn_args <- fn_args

  class(result) = "nsga2R"

  # Execution time
  end_time <- Sys.time()
  print(end_time)
  formatted_time <- format_execution_time(start_time, end_time)
  cat("Execution time:", formatted_time, "\n")

  return(result)
}



# -------------------------------------------------------------------------
# create_core_working_dirs
# -------------------------------------------------------------------------

#' @title Create temporary directories for parallel processing
#'
#' @description Internal helper for creating temporary directories for
#' parallel processing. Each directory is a copy of the SWAT `TxtInOut` folder.
#'
#' @param cores Number of cores to create directories for.
#' @param TxtInOut_dir Path to the original `TxtInOut` directory.
#'
#' @return A character vector of paths to the temporary `TxtInOut` directories.
#' @noRd
create_core_working_dirs  <- function(cores, TxtInOut_dir) {
  # Input validation
  if (!dir.exists(TxtInOut_dir)) {
    stop("TxtInOut_dir does not exist. Please provide a valid directory.")
  }
  if (!is.numeric(cores) || cores < 1 || cores %% 1 != 0) {
    stop("cores must be a positive integer.")
  }

  # Create temporary directories
  core_working_dirs  <- vapply(1:cores, function(i) {
    temp_dir <- suppressWarnings(
      normalizePath(file.path(TxtInOut_dir, "..", paste0("dir_core_", i)))
    )
    dir.create(temp_dir, recursive = TRUE)
    file.copy(TxtInOut_dir, temp_dir, recursive = TRUE)
    temp_dir <- suppressWarnings(
      normalizePath(file.path(temp_dir, "TxtInOut"))
    )
    return(temp_dir)
  }, character(1))

  return(core_working_dirs )
}


# -------------------------------------------------------------------------
# format_execution_time
# -------------------------------------------------------------------------
format_execution_time <- function(start_time, end_time) {
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Convert seconds to hh:mm:ss
  hours <- execution_time %/% 3600
  minutes <- (execution_time %% 3600) %/% 60
  seconds <- round(execution_time %% 60, 2)

  formatted_time <- sprintf("%02d:%02d:%05.2f", hours, minutes, seconds)

  return(formatted_time)
}


# -------------------------------------------------------------------------
# validate_swat_directory
# -------------------------------------------------------------------------
# Ensure the SWAT project is available
validate_swat_directory <- function(TxtInOut_dir, check_backup = FALSE) {
  if (!dir.exists(TxtInOut_dir)) {
    stop("The specified 'TxtInOut_dir' directory was not found, ",
         "please provide a valid path.")
  }
  if (!file.access(TxtInOut_dir, 2) == 0) {
    stop("TxtInOut_dir is not writable.")
  }
  if (!file.exists(file.path(TxtInOut_dir, "file.cio"))) {
    stop("'file.cio' not found, ensure the directory is a SWAT project.")
  }
  if (check_backup && !dir.exists(file.path(TxtInOut_dir, "Backup"))) {
    stop("The 'Backup' folder is missing, ensure 'TxtInOut_dir' contains it.")
  }
}



# -------------------------------------------------------------------------
# check_libraries
# -------------------------------------------------------------------------
check_libraries <- function(target_libraries) {
  # Ensure the input is not NULL or empty
  if (is.null(target_libraries) || length(target_libraries) == 0) {
    stop("No target libraries specified for validation.")
  }

  # Check for missing packages
  missing_libs <- target_libraries[!sapply(
    target_libraries,
    function(lib) require(lib, character.only = TRUE, quietly = TRUE)
  )]

  # Stop execution if any required package is missing
  if (length(missing_libs) > 0) {
    stop("Missing package(s): ",
         paste(missing_libs, collapse = ", "),
         ", install them and try again.")
  }

  return(target_libraries)
}

