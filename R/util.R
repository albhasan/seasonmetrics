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
