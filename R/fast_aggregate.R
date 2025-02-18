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
#' @export
#'
process_csv_fast <- function(file_path, grid_origin, grid_size) {
  .data <- NULL
  longitude <- latitude <- NULL
  acq_date <- month <- year <- NULL

  # Read data.
  data_tb <-
    file_path |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::select(longitude, latitude, acq_date) |>
    dplyr::mutate(
      acq_date = lubridate::as_date(acq_date),
      year = lubridate::year(acq_date),
      month = lubridate::month(acq_date)
    ) |>
    dplyr::select(longitude, latitude, year, month)

  data_tb <-
    add_cell_ids(
      data_tb = data_tb,
      grid_origin = grid_origin,
      grid_size = grid_size
    )

  data_tb <-
    data_tb |>
    dplyr::count(.data[["cell_id"]], .data[["x_cent"]], .data[["y_cent"]],
      year, month,
      name = "n_points"
    )

  return(data_tb)
}


#' Add an cell id to each point
#'
#' @description
#' Given a data frame with point coordiantes and a grid, add a cell id to each
#' point
#'
#' @param data_tb a data frame with longitude and latitude columns.
#' @param grid_origin a numeric. Origin of the grid.
#' @param grid_size a numeric. Size of the grid.
#'
#' @return a data frame with cell centroids (x_cent, y_cent) and a ID built
#' using the ceentroids (cell_id).
#'
add_cell_ids <- function(data_tb, grid_origin, grid_size) {
  stopifnot(
    "Longitude and latitude columns not found!" =
      all(c("longitude", "latitude") %in% colnames(data_tb))
  )
  ji_mt <- ji(
    xy = cbind(data_tb[["longitude"]], data_tb[["latitude"]]),
    origin = grid_origin,
    cellsize = grid_size
  )
  data_tb["x_cent"] <- ji_mt[, 1]
  data_tb["y_cent"] <- ji_mt[, 2]
  data_tb["cell_id"] <- paste(ji_mt[, 1], ji_mt[, 2], sep = "_")
  return(data_tb)
}



#' Estimate the grid cell of each point.
#'
#' @description
#' Estimate the grid cell of each point. Taken from whuber's answer at
# <https://gis.stackexchange.com/questions/48416/aggregating-points-to-grid-using-r>
#'
#' @param xy a matrix with two columns; The X and Y coordinates.
#' @param origin a numeric(2). The origin of the grid.
#' @param cellsize a numeric(2). The size of a cell in the grid.
#'
#' @return a numeric matrix with 2 columns representing the longitude and
#' latitude of thecenter point of the cell corresponding to each point in the
#' given data frame.
#'
ji <- function(xy, origin, cellsize) {
  stopifnot("Only two columns expected!" = ncol(xy) == 2)
  t(apply(
    X = xy,
    MARGIN = 1,
    FUN = function(z) {
      cellsize / 2 + origin + cellsize * (floor((z - origin) / cellsize))
    }
  ))
}
