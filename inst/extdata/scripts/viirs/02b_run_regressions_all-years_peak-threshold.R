##############################################################################
# RUN REGRESSIONS
# Run regression over the aggregated data using PEAK AND THRESHOLD function.
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

# Load grid parameters
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

# Peak and threshold patameters.
threshold_cons <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters_peak-threshold.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Peak & Threshold parameters not found!" =
    all(c("threshold_cons") %in% ls())
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
    "extdata", "scripts", "util", "util_peak-threshold.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Peak & Threshold utilitary functions not found!" =
    all(c("season_peak_thres_helper") %in% ls())
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
all_years_pthres <-
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
all_years_pthres <-
  all_years_pthres |>
  dplyr::select(group, cell_id, month, n_points) |>
  dplyr::mutate(month = as.integer(month)) |>
  dplyr::arrange(cell_id, month) |>
  dplyr::group_by(cell_id) |>
  dplyr::group_split()

rlog::log_info("Computing season using peak & threshold function...")

if (cores_compute_season > 1) {
  future::plan(
    strategy = future::multisession,
    workers = cores_compute_season
  )
  options <- furrr::furrr_options(seed = 123)
}

all_years_pthres <-
  all_years_pthres |>
  furrr::future_map(
    purrr::possibly(
      .f = season_peak_thres_helper,
      otherwise = "ERROR"
    ),
    id_group = "group",
    id_col = "cell_id",
    val_col = "n_points",
    month_col = "month",
    threshold_cons = threshold_cons
  )

future::plan(future::sequential)

saveRDS(
  object = all_years_pthres,
  file = file.path(out_dir, "all_years_pthres.rds")
)

# Remove failed fits.
all_years_pthres <- all_years_pthres[sapply(all_years_pthres, is.data.frame)]

rlog::log_info("Converting results to spatial vectors...")
all_years_pthres <-
  all_years_pthres |>
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
  colnames(all_years_pthres)[!colnames(all_years_pthres) %in%
    c(
      "geometry", "pos_min", "val_min",
      "val_mean", "val_sd", "cell_id",
      "group"
    )]

var_r <- lapply(
  X = var_names,
  FUN = rasterize_points,
  data_sf = all_years_pthres,
  grid_r = blank_raster(
    grid_cells = grid_cells, xy_min = xy_min,
    xy_max = xy_max, grid_crs = grid_crs
  )
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

terra::writeRaster(
  var_r,
  filename = file.path(out_dir, "all_years_pthres_r.tif")
)
