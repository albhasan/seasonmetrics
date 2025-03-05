##############################################################################
# 01 USE THE AGGREGATED DATA TO CREATE THE RASTERS FOR ANALYSIS.
##############################################################################

library(dplyr)
library(rlog)
library(purrr)
library(stringr)
library(terra)
library(tidyr)

library(seasonmetrics)



rlog::log_info("Start new process 01_create_rasters.R -----------------------")

#---- Setup ----

rlog::log_info("Reading parameters...")

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

all_years_r_rds <- file.path(out_dir, "all_years_r.rds")
stopifnot("Previous `all_years_r_rds` file found!" = !file.exists(all_years_r_rds))

two_years_r_rds <- file.path(out_dir, "two_years_r.rds")
stopifnot("Previous `two_years_r_rds` file found!" = !file.exists(two_years_r_rds))

# Load grid parameters
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



#---- Utility ----

#' Helper function for rasterizing data frames
#'
#' @description
#' Wrapper function around the function rasterize of the terra package.
#'
#' @param v a character(1). Name of a column in df.
#' @param df a data frame.
#' @param xcol,ycol a character(1). Name of columns in df with coordinates.
#' @param xy_min,xy_max a numeric(2). Minimum and maximum grid coordinate
#' values.
#' @oaran grid_cells a numeric(2). Number of cells in the grid.
#' @param grid_crs a character. Coordinate reference system of the resulting
#' grid.
#'
#' @return a list of rasters (terra).
#'
rasterize_df <- function(
    v, df, xcol, ycol,
    xy_min, xy_max,
    grid_cells, grid_crs) {
  terra::rasterize(
    x = terra::vect(
      x = df[c(xcol, ycol, v)],
      geom = c(xcol, ycol),
      crs = paste0("EPSG:", grid_crs)
    ),
    y = blank_raster(
      grid_cells = grid_cells,
      xy_min = xy_min,
      xy_max = xy_max,
      grid_crs = grid_crs
    ),
    field = v
  )
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
all_years_df <-
  files_df |>
  tidyr::unnest(data) |>
  dplyr::group_by(cell_id, month) |>
  dplyr::summarize(n_points = sum(n_points, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(cell_id, month, n_points) |>
  dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2)) |>
  tidyr::pivot_wider(
    names_prefix = "m",
    names_from = month,
    values_from = n_points,
    values_fill = 0
  ) |>
  dplyr::left_join(y = unique_cent, by = "cell_id") |>
  sort_cols()

var_names <- colnames(all_years_df)[!colnames(all_years_df) %in%
  c("cell_id", "x_cent", "y_cent")]

rlog::log_info("Rasterizing...")

all_years_r <- lapply(
  X = var_names,
  FUN = rasterize_df,
  df = as.data.frame(all_years_df),
  xcol = "x_cent",
  ycol = "y_cent",
  xy_min = xy_min,
  xy_max = xy_max,
  grid_cells = grid_cells,
  grid_crs = grid_crs
)
names(all_years_r) <- var_names
all_years_r <- terra::rast(all_years_r)

rlog::log_info("Saving to disk...")
saveRDS(object = all_years_r, file = all_years_r_rds)



rlog::log_info("Aggregating data from every 2 years...")
n_years <- 2
two_years_df <-
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
  dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2)) |>
  tidyr::pivot_wider(
    names_prefix = "m",
    names_from = month,
    values_from = n_points,
    values_fill = 0
  ) |>
  dplyr::left_join(y = unique_cent, by = "cell_id") |>
  sort_cols()

year_group_names <-
  two_years_df |>
  dplyr::pull(year_group) |>
  unique() |>
  sort()



rlog::log_info("Rasterizing...")
two_years_r_ls <-
  two_years_df |>
  dplyr::arrange(year_group) |>
  dplyr::group_by(year_group) |>
  dplyr::group_split() |>
  stats::setNames(year_group_names) |>
  purrr::map(
    .f = function(x, var_names) {
      r <- lapply(
        X = var_names,
        FUN = rasterize_df,
        df = as.data.frame(x),
        xcol = "x_cent",
        ycol = "y_cent",
        xy_min = xy_min,
        xy_max = xy_max,
        grid_cells = grid_cells,
        grid_crs = grid_crs
      )
      names(r) <- var_names
      r <- terra::rast(r)
    },
    var_names = var_names
  )


# NOTE: terra objects need to be serialized before saving and
# unserialized after reading (terra::unserialize).
rlog::log_info("Saving to disk...")
two_years_r_ls <- lapply(
  X = two_years_r_ls,
  FUN = terra::serialize,
  connection = NULL
)
saveRDS(object = two_years_r_ls, file = two_years_r_rds)

rlog::log_info("Finished!")
