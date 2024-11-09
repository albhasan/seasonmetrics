library(dplyr)
library(furrr)
library(lubridate)
library(purrr)
library(readr)
library(rlang)
library(rlog)
library(sf)
library(stringr)
library(terra)
library(tidyr)

library(seasonmetrics)



#---- Setup ----

rlog::log_info("Start new process -------------------------------------------")

# VIIRS data.
csv_files <- "/home/alber/Documents/data/r_packages/seasonmetrics/VIIRS"
stopifnot("CSV directory not found!" = dir.exists(csv_files))

out_dir <- "/home/alber/Documents/results/r_packages/seasonmetrics/results_04"
stopifnot("Output directory not found!" = dir.exists(out_dir))

files_df_rds <- file.path(out_dir, "files_df.rds")
stopifnot("Previous `files_df_rds` file found!" = !file.exists(files_df_rds))

# Grid.
xy_min <- c(-180, -90)
xy_max <- c(180, 90)
grid_crs <- 4326
id_col <- "cell_id"

# Double sigmoid fitting attempts.
n_runs_min <- 12
n_runs_max <- 48

grid_cells <- c(cols = 1440, rows = 720)

# Machine: Ubuntu 22 running on WSL2.
# Model name: Intel(R) Xeon(R) CPU E5-2640 v3 @ 2.60GHz
# CPU(s):     32
# RAM:        32GB
#
# Number of cores used when processing in parallel.
# 1 cores. Reached up to 27GB while processing (no data.table).
# real    112m43.654s
# user    119m12.030s
# sys     2m16.657s
#--------------------------------------------------------
#
# Compute seasons 2 cores
# 2024-08-23 11:30:54.933611 [INFO] Computing month_df...
# 2024-08-23 11:48:25.315343 [INFO] Computing season using double sigmoidal...
# 2024-08-25 20:54:03.729327 [INFO] Finished!


cores_process_csv <- 1L
cores_compute_season <- 2L



#---- Utility ----

rlog::log_info("Loading utility functions...")



#' Process CSV files
#'
#' @description
#' Read a CSV file representing points and aggregate them into a regular grid.
#'
#' @param file_path a character. Path to a CSV file.
#' @param grid_origin a numeric. Origin of the aggregation grid.
#' @param grid_size a numeric. Size of the aggregation grid.
#'
#' @return a data frame (tibble).
#'
process_csv_fast <- function(file_path, grid_origin, grid_size) {

  longitude <- latitude <- acq_date <- NULL

  # Read data.
  data_tb <-
    file_path %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::select(longitude, latitude, acq_date) %>%
    dplyr::mutate(acq_date = lubridate::as_date(acq_date),
                  year = lubridate::year(acq_date),
                  month = lubridate::month(acq_date)) %>%
    dplyr::select(longitude, latitude, year, month)

  # Estimate the grid cell of each point.
  #NOTE: Taken whuber's answer at from:
  #https://gis.stackexchange.com/questions/48416/aggregating-points-to-grid-using-r
  ji <- function(xy, origin = grid_origin, cellsize = grid_size) {
    t(apply(xy, 1,
      function(z) {
        cellsize / 2 + origin + cellsize * (floor((z - origin) / cellsize))
      }
    ))
  }
  JI <- ji(cbind(data_tb$longitude, data_tb$latitude))
  data_tb["x_cent"] <- JI[, 1]
  data_tb["y_cent"] <- JI[, 2]
  data_tb[id_col] <- paste(JI[, 1], JI[, 2], sep = "_")
  rm(JI)
  gc()

  data_tb %>%
    dplyr::count(.data[[id_col]], .data[["x_cent"]], .data[["y_cent"]],
                 year, month, name = "n_points") %>%
    return()
}



#' Raster template for rasterization
#'
#' @param grid_cells a numeric. Number of cells for the raster.
#' @param xy_min,xy_max a numeric. Minimum and maximum XY coordinates.
#' @param grid_crs a numeric. EPSG code.
#'
#' @return a terra object (raster).
#'
blank_raster <- function(grid_cells, xy_min, xy_max, grid_crs) {
  terra::rast(
    nrows = grid_cells[["rows"]],
    ncols = grid_cells[["cols"]],
    xmin = xy_min[1],
    xmax = xy_max[1],
    ymin = xy_min[2],
    ymax = xy_max[2],
    crs = paste0("EPSG:", grid_crs)
  )
}



#' Rasterize points
#'
#' @descrition
#' Rasterize points given a variable name. This is a utility function.
#'
#' @param vname a character. Name of a column in data_sf.
#' @param data_sf an sf object (points).
#' @param grid_r a terra object (raster).
#'
#' @return a terra object (raster).
#'
rasterize_helper <- function(vname, data_sf, grid_r) {
  terra::rasterize(x = terra::vect(data_sf[vname]),
                   y = grid_r,
                   field = vname)
}



#' Compute the peak seaon using a threshold
#'
#' @description
#' Utility funcion for computing the peak threshold.
#'
#' @param x a data frame.
#' @param id_col a character. Name of the ID column in x.
#'
#' @return a data frame (tibble).
#'
season_peak_thres_helper <- function(x, id_col) {
  c_id <- unique(x[[id_col]])[1]
  stopifnot("Only one ID allowed!" = length(c_id) == 1)
  # Fill in values for the missing months.
  if (!all(1:12 %in% x[["month"]])) {
    complement_df <-
      tibble::tibble(
        "{id_col}" := c_id,
        month = (1:12)[!(1:12 %in% x[["month"]])],
        max_n = 0
      )
    x <- dplyr::bind_rows(x, complement_df)
  }
  # Compute the season.
  max_n <- NULL
  x %>%
    dplyr::arrange(month) %>%
    dplyr::pull(max_n) %>%
    seasonmetrics::compute_season_peak_threshold(threshold_cons = 0.6) %>%
    dplyr::mutate("{id_col}" := c_id) %>%
    return()
}



#' Compute the peak seaon using a double sigmoidal function
#'
#' @description
#' Utility funcion for computing the peak season using a double sigmoidal
#' function fit.
#'
#' @param x a data frame.
#' @param id_col a character. Name of the ID column in x.
#'
#' @return a data frame (tibble).
#'
season_peak_dsig_helper <- function(x, id_col) {
  c_id <- unique(x[[id_col]])[1]
  stopifnot("Only one ID allowed!" = length(c_id) == 1)
  # Fill in values for the missing months.
  if (!all(1:12 %in% x[["month"]])) {
    complement_df <-
      tibble::tibble(
        "{id_col}" := c_id,
        month = (1:12)[!(1:12 %in% x[["month"]])],
        max_n = 0
      )
    x <-
      x %>%
      dplyr::bind_rows(complement_df)
  }
  # Compute the season.
  max_n <- NULL
  x %>%
    dplyr::arrange(month) %>%
    dplyr::pull(max_n) %>%
    compute_season_double_sig(n_runs_min = n_runs_min,
                              n_runs_max = n_runs_max) %>%
    dplyr::mutate("{id_col}" := c_id) %>%
    return()
}



#---- Script ----

rlog::log_info("Processing CSV files...")

if (cores_process_csv > 1) {
  future::plan(multisession, workers = cores_process_csv)
  options <- furrr::furrr_options(seed = 123)
}

# List and pre-process CSV files.
files_df <-
  csv_files %>%
  list.files(pattern = "*.csv", full.names = TRUE) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  #NOTE: Use less files when debugging.
  dplyr::slice(1) %>%
  dplyr::mutate(
    data = furrr::future_map(file_path,
                             process_csv_fast,
                             grid_origin = xy_min,
                             grid_size = (xy_max - xy_min) / grid_cells,
                             .options = furrr::furrr_options(seed = 123))
  )

future::plan(sequential)
gc()

saveRDS(object = files_df, file = files_df_rds)

rlog::log_info("Computing month_sum...")

# Unique cell centers.
unique_cent <-
  files_df %>%
  dplyr::pull(data) %>%
  dplyr::bind_rows() %>%
  dplyr::select(cell_id, x_cent, y_cent) %>%
  dplyr::distinct(cell_id, x_cent, y_cent)

rlog::log_info("Computing monthly statistics...")
month_df <-
  files_df %>%
  dplyr::pull(data) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(.data[[id_col]], month) %>%
  dplyr::summarize(
    min_n = min(n_points),
    mean_n = mean(n_points),
    median_n = stats::median(n_points),
    max_n = max(n_points),
    sd_n = sd(n_points),
    sum_n = sum(n_points)
  ) %>%
  dplyr::arrange(cell_id, month) %>%
  dplyr::mutate(month = as.integer(month)) %>%
  dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2))

rlog::log_info("Rasterizing monthly totals...")
month_sum <-
  month_df %>%
  dplyr::select(cell_id, month, sum_n) %>%
  tidyr::pivot_wider(names_prefix = "m", names_from = month,
                     values_from = sum_n, values_fill = 0) %>%
  dplyr::select(sort(colnames(.))) %>%
  tibble::as_tibble() %>%
  dplyr::left_join(y = unique_cent, by = "cell_id")

vars <- colnames(month_sum)[!colnames(month_sum) %in%
                              c(id_col, "x_cent", "y_cent")]
month_sum_r <- lapply(vars,
  function(v, df, xcol, ycol) {
    terra::rasterize(
      x = as.matrix(df[c(xcol, ycol)]),
      y = blank_raster(grid_cells = grid_cells, xy_min = xy_min,
                       xy_max = xy_max, grid_crs = grid_crs),
      values = df[[v]]
    )
  },
  df = month_sum,
  xcol = "x_cent",
  ycol = "y_cent"
)
names(month_sum_r) <- vars
month_sum_r <- terra::rast(month_sum_r)

rlog::log_info("Writing monthly totals to a file...")
terra::writeRaster(month_sum_r, 
                   filename = file.path(out_dir, "month_sum_r.tif"),
                   overwrite = TRUE)

rm(month_sum)
rm(month_sum_r)
gc()

if (cores_compute_season > 1) {
  future::plan(multisession, workers = cores_compute_season)
  options <- furrr::furrr_options(seed = 123)
}

rlog::log_info("Computing season using peak and threshold...")

# Estimate seasons' parameters.
season_peak_thres_sf <-
  month_df %>%
  dplyr::select({{id_col}}, month, max_n) %>%
  dplyr::mutate(month = as.integer(month)) %>%
  dplyr::arrange({{id_col}}, month) %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::group_split() %>%
  furrr::future_map(season_peak_thres_helper, id_col = id_col) %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble() %>%
  tidyr::separate(col = cell_id, into = c("x", "y"),
                  sep = "_", remove = FALSE) %>%
  dplyr::mutate(x = as.double(x), y = as.double(y)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = grid_crs)

rlog::log_info("Rasterizing peak and threshold results...")

# Rasterize season parameters.
var_names <-
  colnames(season_peak_thres_sf)[!colnames(season_peak_thres_sf) %in%
                                 "geometry"]
var_r <- lapply(
  X = var_names,
  FUN = rasterize_helper,
  data_sf = season_peak_thres_sf,
  grid_r = blank_raster(grid_cells = grid_cells, xy_min = xy_min,
                        xy_max = xy_max, grid_crs = grid_crs)
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

terra::writeRaster(
  var_r,
  filename = file.path(out_dir, "season_peak_thres.tif")
)

rm(season_peak_thres_sf)
gc()



rlog::log_info("Computing season using double sigmoidal...")

season_dsig_df <-
  month_df %>%
  dplyr::select({{id_col}}, month, max_n) %>%
  dplyr::mutate(month = as.integer(month)) %>%
  dplyr::arrange({{id_col}}, month) %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::group_split() %>%
  furrr::future_map(
    purrr::possibly(.f = season_peak_dsig_helper, otherwise = "ERROR"),
    id_col = id_col
  )

# Remove failed fits.
season_dsig_df <- season_dsig_df[sapply(season_dsig_df, is.data.frame)]
season_dsig_df <- dplyr::bind_rows(season_dsig_df)

season_dsig_sf <-
  season_dsig_df %>%
  tibble::as_tibble() %>%
  tidyr::separate(col = cell_id, into = c("x", "y"),
                  sep = "_", remove = FALSE) %>%
  dplyr::mutate(x = as.double(x), y = as.double(y)) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = grid_crs)

rlog::log_info("Rasterizing double sigmoidal results...")

var_names <-
  colnames(season_dsig_sf)[!colnames(season_dsig_sf) %in%
                           c("geometry", "pos_min", "val_min", "val_len",
                             "val_mean", "val_sd", "cell_id")]
var_r <- lapply(
  X = var_names,
  FUN = rasterize_helper,
  data_sf = season_dsig_sf,
  grid_r = blank_raster(grid_cells = grid_cells, xy_min = xy_min,
                        xy_max = xy_max, grid_crs = grid_crs)
)
names(var_r) <- var_names
var_r <- terra::rast(var_r)

terra::writeRaster(
  var_r,
  filename = file.path(out_dir, "season_dsig.tif")
)

future::plan(sequential)
rm(season_dsig_df)
gc()

rlog::log_info("Finished!")
