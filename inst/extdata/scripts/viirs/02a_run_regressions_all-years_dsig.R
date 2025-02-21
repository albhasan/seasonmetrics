##############################################################################
# RUN REGRESSIONS
# Run regression over the aggregated data using DOUBLE SIGMOIDAL FUNCTION.
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
rlog::log_info("Start new process -----------------------")


# # double sigmoid fitting attempts.
out_dir <- files_df_rds <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Script parameter not found!" =
    all(c("out_dir", "files_df_rds") %in% ls())
)

# # double sigmoid fitting attempts.
n_runs_min <- n_runs_max <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters_dsig.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "double sigmoid parameters not found!" =
    all(c("n_run_min", "n_run_max") %in% ls())
)

# Load grid parameters.
xy_min <- xy_max <- grid_cells <- grid_crs <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters_grid.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("xy_min", "xy_max", "grid_cells", "grid_crs") %in% ls())
)

# Load parallel computing parameters.
cores_compute_season <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters_computing.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("cores_compute_season") %in% ls())
)



#---- Utility ----
rlog::log_info("Loading utility functions...")

source(
  system.file(
    "extdata", "scripts", "viirs", "util_dsig.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "double sigmoid utilitary functions not found!" =
    all(c("season_peak_dsig_helper") %in% ls())
)






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
