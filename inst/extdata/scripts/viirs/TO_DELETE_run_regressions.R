##############################################################################
# RUN REGRESSIONS
# Run regression over the aggregated data.
##############################################################################

library(dplyr)
library(future)
library(rlog)
library(purrr)
library(stringr)
library(terra)
library(tidyr)

library(seasonmetrics)



#---- Setup ----
rlog::log_info("Start new process 02_run_regressions.R -----------------------")

out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs"
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Load results from previous scripts.
files_df_rds <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs/files_df.rds"
stopifnot("`files_df_rds` file not found!" = file.exists(files_df_rds))

# double sigmoid fitting attempts.
n_runs_min <- 12
n_runs_max <- 48

# Load grid parameters
xy_min <- xy_max <- grid_cells <- grid_crs <- NULL
source(
  system.file(
    "extdata", "scripts", "VIIRS", "grid_parameters.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("xy_min", "xy_max", "grid_cells", "grid_crs") %in% ls())
)

# Number of cores to use in parallel to run regressions.
cores_compute_season <- 4



#---- Utility ----
rlog::log_info("Loading utility functions...")



#' Compute the peak seaon using a double sigmoidal function
#'
#' @description
#' Utility funcion for computing the peak season using a double sigmoidal
#' function fit.
#'
#' @param x a data frame.
#' @param id_group a character. Name of the group column in x.
#' @param id_col a character. Name of the ID column in x.
#' @param val_col a character. Name of the column with a value.
#' @param month_col a character. Name of the column with month numbers.
#'
#' @return a data frame (tibble).
#'
season_peak_dsig_helper <- function(
    x, id_group, id_col, val_col,
    month_col, n_runs_min, n_runs_max) {
  g_id <- unique(x[[id_group]])
  stopifnot("Only one group ID allowed!" = length(g_id) == 1)
  c_id <- unique(x[[id_col]])
  stopifnot("Only one ID allowed!" = length(c_id) == 1)
  # Fill in values for the missing months.
  if (!all(1:12 %in% x[[month_col]])) {
    complement_df <-
      tibble::tibble(
        "{id_group}" := g_id,
        "{id_col}" := c_id,
        "{month_col}" := (1:12)[!(1:12 %in% x[[month_col]])],
        "{val_col}" := 0
      )
    x <-
      x |>
      dplyr::bind_rows(complement_df)
  }
  # Compute the season.
  res <-
    x |>
    dplyr::arrange(.data[[month_col]]) |>
    dplyr::pull(tidyselect::all_of(val_col)) |>
    compute_season_double_sig(
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    ) |>
    dplyr::mutate(
      "{id_group}" := g_id,
      "{id_col}" := c_id
    )

  return(res)
}




#---- Script ----

rlog::log_info("Loading data...")
files_df <- readRDS(files_df_rds)

rlog::log_info("Getting cell centers...")
unique_cent <-
  files_df %>%
  dplyr::pull(data) %>%
  dplyr::bind_rows() %>%
  dplyr::select(cell_id, x_cent, y_cent) %>%
  dplyr::distinct(cell_id, x_cent, y_cent)



rlog::log_info("Aggregating data from all years...")
all_years_dsig <-
  files_df |>
  tidyr::unnest(data) |>
  dplyr::group_by(cell_id, month) |>
  dplyr::summarize(n_points = sum(n_points, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(cell_id, month, n_points) |>
  dplyr::mutate(
    month = stringr::str_pad(month, pad = 0, width = 2),
    group = "all"
  )

rlog::log_info("Splitting data by pixel...")
all_years_dsig <-
  all_years_dsig |>
  dplyr::select(group, cell_id, month, n_points) |>
  dplyr::mutate(month = as.integer(month)) |>
  dplyr::arrange(cell_id, month) |>
  dplyr::group_by(cell_id) |>
  dplyr::group_split()

rlog::log_info("Computing season using double sigmoidal function...")

if (cores_compute_season > 1) {
  future::plan(
    strategy = future::multisession,
    workers = cores_compute_season
  )
  options <- furrr::furrr_options(seed = 123)
}

all_years_dsig <-
  all_years_dsig |>
  furrr::future_map(
    purrr::possibly(.f = season_peak_dsig_helper, otherwise = "ERROR"),
    id_group = "group",
    id_col = "cell_id",
    val_col = "n_points",
    month_col = "month",
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max
  )

future::plan(future::sequential)

saveRDS(
  object = all_years_dsig,
  file = file.path(out_dir, "all_years_dsig.rds")
)

# Remove failed fits.
all_years_dsig <- all_years_dsig[sapply(all_years_dsig, is.data.frame)]

rlog::log_info("Converting results to spatial vectors...")
all_years_dsig <-
  all_years_dsig |>
  dplyr::bind_rows() |>
  tibble::as_tibble() |>
  tidyr::separate(
    col = cell_id, into = c("x", "y"),
    sep = "_", remove = FALSE
  ) |>
  dplyr::mutate(x = as.double(x), y = as.double(y)) |>
  sf::st_as_sf(coords = c("x", "y"), crs = grid_crs)

rlog::log_info("Rasterizing vectors...")

var_names <-
  colnames(all_years_dsig)[!colnames(all_years_dsig) %in%
    c(
      "geometry", "pos_min", "val_min",
      "val_mean", "val_sd", "cell_id",
      "group"
    )]

var_r <- lapply(
  X = var_names,
  FUN = rasterize_points,
  data_sf = all_years_dsig,
  grid_r = blank_raster(
    grid_cells = grid_cells, xy_min = xy_min,
    xy_max = xy_max, grid_crs = grid_crs
  )
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

terra::writeRaster(
  var_r,
  filename = file.path(out_dir, "all_years_dsig_r.tif")
)



rlog::log_info("Aggregating data from every 2 years...")
n_years <- 2
two_years_dsig <-
  files_df |>
  tidyr::unnest(data) |>
  # Add a column indicating the first year of the grouping period.
  dplyr::mutate(
    year_group = year - (year %% n_years),
    year_group = paste("p", year_group, sep = "_")
  ) |>
  dplyr::group_by(cell_id, month, year_group) |>
  dplyr::summarize(n_points = sum(n_points, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(cell_id, month, year_group, n_points) |>
  dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2))

group_names <-
  two_years_dsig |>
  dplyr::pull(year_group) |>
  unique() |>
  sort()

rlog::log_info("Splitting data by year and pixel...")
two_years_dsig <-
  two_years_dsig |>
  dplyr::select(year_group, cell_id, month, n_points) |>
  dplyr::mutate(month = as.integer(month)) |>
  dplyr::arrange(year_group, cell_id, month) |>
  dplyr::group_by(year_group, cell_id) |>
  dplyr::group_split()

rlog::log_info("Computing season using double sigmoidal function...")

if (cores_compute_season > 1) {
  future::plan(
    strategy = future::multisession,
    workers = cores_compute_season
  )
  options <- furrr::furrr_options(seed = 123)
}

two_years_dsig <-
  two_years_dsig |>
  furrr::future_map(
    purrr::possibly(.f = season_peak_dsig_helper, otherwise = "ERROR"),
    id_group = "year_group",
    id_col = "cell_id",
    val_col = "n_points",
    month_col = "month",
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max
  )

future::plan(future::sequential)

saveRDS(
  object = two_years_dsig,
  file = file.path(out_dir, "all_years_dsig.rds")
)

# Remove failed fits.
two_years_dsig <- two_years_dsig[sapply(two_years_dsig, is.data.frame)]

rlog::log_info("Converting results to spatial vectors...")
two_years_dsig <-
  two_years_dsig |>
  dplyr::bind_rows() |>
  tibble::as_tibble() |>
  tidyr::separate(
    col = cell_id, into = c("x", "y"),
    sep = "_", remove = FALSE
  ) |>
  dplyr::mutate(x = as.double(x), y = as.double(y)) |>
  sf::st_as_sf(coords = c("x", "y"), crs = grid_crs)

rlog::log_info("Rasterizing vectors...")

var_names <-
  colnames(two_years_dsig)[!colnames(two_years_dsig) %in%
    c(
      "geometry", "pos_min", "val_min",
      "val_mean", "val_sd", "cell_id",
      "year_group"
    )]

var_r <- lapply(
  X = var_names,
  FUN = rasterize_points,
  data_sf = two_years_dsig,
  grid_r = blank_raster(
    grid_cells = grid_cells, xy_min = xy_min,
    xy_max = xy_max, grid_crs = grid_crs
  )
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

terra::writeRaster(
  var_r,
  filename = file.path(out_dir, "two_years_dsig_r.tif")
)
