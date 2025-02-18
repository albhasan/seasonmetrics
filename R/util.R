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



#' Compute statistics over the column of a CSV file
#'
#' @description
#' Read the given CSV file and compute statistics over its columns.
#'
#' @param x A character. Path to a CSV file.
#'
#' @return a data frame with statistics.
#'
get_field_stats <- function(x) {
  longitude <- latitude <- acq_date <- NULL
  min_max_len <- list(
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    len = ~ length(.x)
  )
  res <-
    x |>
    readr::read_csv() |>
    dplyr::select(longitude, latitude, acq_date) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), min_max_len))
  return(res)
}
