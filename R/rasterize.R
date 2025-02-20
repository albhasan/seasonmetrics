#' Rasterize points
#'
#' @description
#' Rasterize points given a variable name. This is a utility function.
#'
#' @param vname a character. Name of a column in data_sf.
#' @param data_sf an sf object (points).
#' @param grid_r a terra object (raster).
#'
#' @return a terra object (raster).
#'
#' @export
#'
rasterize_points <- function(vname, data_sf, grid_r) {
  return(terra::rasterize(
    x = terra::vect(data_sf[vname]),
    y = grid_r,
    field = vname
  ))
}
