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


# double sigmoid fitting attempts.
out_dir <- files_df_rds <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Script parameter not found!" =
    all(c("out_dir", "files_df_rds") %in% ls())
)

# double sigmoid fitting attempts.
n_runs_min <- n_runs_max <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters_dsig.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Double sigmoid parameters not found!" =
    all(c("n_runs_min", "n_runs_max") %in% ls())
)

# Load grid parameters.
xy_min <- xy_max <- grid_cells <- grid_crs <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters_grid.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("xy_min", "xy_max", "grid_cells", "grid_crs") %in% ls())
)

# Load parallel computing parameters.
cores_process_season <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters_computing.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("cores_process_season") %in% ls())
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
  dplyr::mutate(
    month = as.integer(month),
    group = "all"
  ) |>
  dplyr::arrange(cell_id, month)

# #NOTE: I guess I don't need to filter cells. I would need empty cell for
# #building a raster.
# rlog::log_info("Estimating number of time steps of each cell.")
# stats_tb <-
#   all_years_dsig |>
#   dplyr::group_by(cell_id) |>
#   dplyr::summarize(
#     n_months = dplyr::n(),
#     total_ponts = sum(n_points)
#   )
# stopifnot(
#   "Not cells found with enough data for regression!" =
#   sum(stats_tb[["n_months"]] >= min_number_of_months) > 0
# )
# rlog::log_info(
#   "Found ",
#   sum(stats_tb[["n_months"]] >= min_number_of_months),
#   "/",
#   nrow(stats_tb),
#   " cells that meet the mininum number of time steps."
# )

rlog::log_info("Splitting data by pixel...")
all_years_dsig_ls <-
  all_years_dsig |>
  dplyr::select(group, cell_id, month, n_points) |>
  dplyr::group_by(cell_id) |>
  dplyr::group_split()

cores_process_season <- min(cores_process_season, parallel::detectCores())
rlog::log_info(
  sprintf("Setting up processing with %s cores...", cores_process_season)
)

if (cores_process_season > 1) {
  future::plan(
    strategy = future::multisession,
    workers = cores_process_season
  )
  options <- furrr::furrr_options(seed = 123)
}

rlog::log_info("Computing season using double sigmoidal function...")
all_years_dsig_ls <-
  all_years_dsig_ls |>
  furrr::future_map(
    purrr::possibly(
      .f = season_peak_dsig_helper
    ),
    id_group = "group",
    id_col = "cell_id",
    val_col = "n_points",
    month_col = "month",
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    n_cycles = 1,
    f = mean
  )

rlog::log_info("Setting up sequential processing...")
future::plan(future::sequential)

all_years_dsig_ls_file <- file.path(out_dir, "all_years_dsig_ls.rds")
rlog::log_info(
  sprintf("Writing regressions of all years to to %s ", all_years_dsig_ls_file)
)
saveRDS(
  object = all_years_dsig_ls,
  file = all_years_dsig_ls_file
)

rlog::log_info("Removing failed fits...")
all_years_dsig_ls <- all_years_dsig_ls[sapply(all_years_dsig_ls, is.data.frame)]

rlog::log_info("Converting results to spatial vector...")
all_years_dsig_tb <-
  all_years_dsig_ls |>
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
  colnames(all_years_dsig_tb)[!colnames(all_years_dsig_tb) %in%
    c(
      "geometry", "pos_min", "val_min",
      "val_mean", "val_sd", "cell_id",
      "group"
    )]

var_r <- lapply(
  X = var_names,
  FUN = rasterize_points,
  data_sf = all_years_dsig_tb,
  grid_r = blank_raster(
    grid_cells = grid_cells, xy_min = xy_min,
    xy_max = xy_max, grid_crs = grid_crs
  )
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

all_years_dsig_r_file <- file.path(out_dir, "all_years_dsig_r.tif")
rlog::log_info("Writing raster to:", all_years_dsig_r_file)
terra::writeRaster(
  var_r,
  filename = all_years_dsig_r_file
)
