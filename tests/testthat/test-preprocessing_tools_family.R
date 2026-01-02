# =============================================================================
# Family: Preprocessing tools
#
# Tests cover:
#  - spatial preprocessing of gridded climate data (areal_mean)
#  - generation of SWAT Editor-compatible inputs (pcp2swat_editor, tmp2swat_editor)
#  - updating existing SWAT climate files (pcp2swat, tmp2swat)
#
# Tests rely on package example NetCDF files and the example SWAT project.
# File-writing operations are skipped on CRAN.
# =============================================================================

skip_on_cran()
skip_if_not_installed("terra")
skip_if_not_installed("sf")
skip_if_not_installed("exactextractr")

# -----------------------------------------------------------------------------
# areal_mean — Spatial aggregation of gridded data
# -----------------------------------------------------------------------------

test_that("areal_mean computes areal means using package example data", {

  pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  grid <- terra::rast(pr_nc)
  geom <- subs1

  result <- areal_mean(
    grid = grid,
    geom = geom,
    start_date = "2010-01-01",
    time_step = "day"
  )

  # Output structure: Date + one column per polygon
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), nrow(geom) + 1)
  expect_true(all(!is.na(result$Date)))
})

test_that("areal_mean validates input arguments", {

  pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  grid <- terra::rast(pr_nc)

  expect_error(
    areal_mean("not_a_raster", subs1, "2010-01-01"),
    "SpatRaster"
  )

  expect_error(
    areal_mean(grid, "not_sf", "2010-01-01"),
    "sf"
  )

  expect_error(
    areal_mean(grid, subs1, "2010-01-01", time_step = "year"),
    "day.*month"
  )
})

# -----------------------------------------------------------------------------
# pcp2swat_editor — Precipitation inputs for SWAT Editor
# -----------------------------------------------------------------------------

test_that("pcp2swat_editor creates SWAT precipitation input files", {

  pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  grid <- terra::rast(pr_nc)
  geom <- subs1
  tmpdir <- tempdir()

  suppressMessages(
    pcp2swat_editor(
      grid = grid,
      subs1 = geom,
      start_date = "2010-01-01",
      output_dir = tmpdir
    )
  )

  out_dir <- file.path(tmpdir, "pcp_swat")

  expect_true(dir.exists(out_dir))
  expect_true(any(grepl("^pcp_", list.files(out_dir))))
  expect_true(file.exists(file.path(out_dir, "pcp_stations.txt")))
})

test_that("pcp2swat_editor validates precipitation unit", {

  pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  grid <- terra::rast(pr_nc)

  expect_error(
    pcp2swat_editor(
      grid = grid,
      subs1 = subs1,
      start_date = "2010-01-01",
      unit = "invalid"
    ),
    "unit"
  )
})

# -----------------------------------------------------------------------------
# tmp2swat_editor — Temperature inputs for SWAT Editor
# -----------------------------------------------------------------------------

test_that("tmp2swat_editor creates SWAT temperature input files", {

  tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
  tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")

  grid_tmax <- terra::rast(tmax_nc)
  grid_tmin <- terra::rast(tmin_nc)
  geom <- subs1
  tmpdir <- tempdir()

  suppressMessages(
    tmp2swat_editor(
      grid_tmax = grid_tmax,
      grid_tmin = grid_tmin,
      subs1 = geom,
      start_date = "2010-01-01",
      output_dir = tmpdir
    )
  )

  out_dir <- file.path(tmpdir, "tmp_swat")

  expect_true(dir.exists(out_dir))
  expect_true(any(grepl("^tmp_", list.files(out_dir))))
  expect_true(file.exists(file.path(out_dir, "tmp_stations.txt")))
})

test_that("tmp2swat_editor errors when Tmax and Tmin layers do not match", {

  tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
  grid_tmax <- terra::rast(tmax_nc)[[1]]   # single layer
  grid_tmin <- terra::rast(tmax_nc)        # multiple layers

  expect_error(
    tmp2swat_editor(
      grid_tmax = grid_tmax,
      grid_tmin = grid_tmin,
      subs1 = subs1,
      start_date = "2010-01-01"
    ),
    "same number of layers"
  )
})

# -----------------------------------------------------------------------------
# pcp2swat — Update existing SWAT precipitation files
# -----------------------------------------------------------------------------

test_that("pcp2swat updates SWAT precipitation files consistently", {

  # Prepare example SWAT project
  tmpdir <- tempdir()
  txtinout <- get_swat_example(tmpdir)

  pcp_files <- list.files(
    file.path(txtinout),
    pattern = "^pcp.*\\.pcp$",
    full.names = TRUE
  )
  expect_true(length(pcp_files) > 0)

  # Prepare new precipitation forcing
  pr_nc <- system.file("extdata", "pr.nc", package = "hydroSWAT")
  pr_grid <- terra::rast(pr_nc)
  pr_data <- areal_mean(pr_grid, subs1, "2010-01-01", time_step = "day")

  # Update files
  outdir <- file.path(tmpdir, "updated_pcp")
  dir.create(outdir)

  pcp2swat(pr_data, pcp_files, output_dir = outdir)

  new_files <- list.files(outdir, pattern = "^pcp.*\\.pcp$", full.names = TRUE)

  expect_equal(length(new_files), length(pcp_files))

  # Metadata (first 4 lines) must remain unchanged
  expect_equal(
    readLines(pcp_files[1], n = 4),
    readLines(new_files[1], n = 4)
  )

  # Number of lines = metadata + data
  expect_equal(
    length(readLines(new_files[1])),
    nrow(pr_data) + 4
  )
})

# -----------------------------------------------------------------------------
# tmp2swat — Update existing SWAT temperature files
# -----------------------------------------------------------------------------

test_that("tmp2swat updates SWAT temperature files consistently", {

  # Prepare example SWAT project
  tmpdir <- tempdir()
  txtinout <- get_swat_example(tmpdir)

  tmp_files <- list.files(
    file.path(txtinout),
    pattern = "^tmp.*\\.tmp$",
    full.names = TRUE
  )
  expect_true(length(tmp_files) > 0)

  # Prepare new temperature forcing
  tmax_nc <- system.file("extdata", "tasmax.nc", package = "hydroSWAT")
  tmin_nc <- system.file("extdata", "tasmin.nc", package = "hydroSWAT")

  tmax_grid <- terra::rast(tmax_nc)
  tmin_grid <- terra::rast(tmin_nc)

  tmax_data <- areal_mean(tmax_grid, subs1, "2010-01-01", time_step = "day")
  tmin_data <- areal_mean(tmin_grid, subs1, "2010-01-01", time_step = "day")

  # Update files
  outdir <- file.path(tmpdir, "updated_tmp")
  dir.create(outdir)

  tmp2swat(tmax_data, tmin_data, tmp_files, output_dir = outdir)

  new_files <- list.files(outdir, pattern = "^tmp.*\\.tmp$", full.names = TRUE)

  expect_equal(length(new_files), length(tmp_files))

  # Metadata must remain unchanged
  expect_equal(
    readLines(tmp_files[1], n = 4),
    readLines(new_files[1], n = 4)
  )

  # Number of lines = metadata + data
  expect_equal(
    length(readLines(new_files[1])),
    nrow(tmax_data) + 4
  )
})
