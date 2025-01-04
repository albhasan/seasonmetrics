grid_helper <- function(o, min, max, res) {
  sort(c(
    seq(from = o, to = max, by = res),
    seq(from = o, to = min, by = -res)[-1]
  ))
}

make_grid_origin_res <- function(xy_origin,
                                 xy_min,
                                 xy_max,
                                 cell_size,
                                 crs = 4326,
                                 id_col = "id") {
  stopifnot("The origin must fall in the given ranges!" = all(
    xy_min[1] <= xy_origin[1], xy_origin[1] <= xy_max[1],
    xy_min[2] <= xy_origin[2], xy_origin[2] <= xy_max[2]
  ))
  stopifnot(
    "`cell_sizse` is too large!" =
      all((xy_max - xy_min) >= cell_size)
  )

  lon_grid <- grid_helper(
    o = xy_origin[1],
    min = xy_min[1],
    max = xy_max[1],
    res = cell_size[1]
  )
  lat_grid <- grid_helper(
    o = xy_origin[2],
    min = xy_min[2],
    max = xy_max[2],
    res = cell_size[2]
  )

  stopifnot(
    "Not enough room to fit a grid!" =
      all(length(lon_grid) > 1, length(lat_grid) > 1)
  )

  aoi_grid <- sf::st_as_sf(sf::st_make_grid(
    x = sf::st_bbox(
      c(
        xmin = min(lon_grid), xmax = max(lon_grid),
        ymin = min(lat_grid), ymax = max(lat_grid)
      ),
      crs = sf::st_crs(crs)
    ),
    cellsize = cell_size
  ))
  aoi_grid[id_col] <- seq_len(nrow(aoi_grid))

  return(aoi_grid)
}



test_that("ji works", {
  # Parameters.
  n_points <- 50L
  grid_size <- c(2, 2)
  grid_origin <- c(0, 0)

  suppressMessages(
    sf::sf_use_s2(use_s2 = FALSE)
  )

  # Create random points.
  data_df <- data.frame(
    longitude = runif(min = -179, max = 179, n = n_points),
    latitude = runif(min = -89, max = 89, n = n_points),
    id_point = seq(n_points)
  )
  points_sf <- sf::st_as_sf(
    x = data_df,
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  )

  # Create a grid.
  grid_sf <- make_grid_origin_res(
    xy_origin = grid_origin,
    xy_min = c(-180, -90),
    xy_max = c(180, 90),
    cell_size = grid_size,
    crs = 4326,
    id_col = "id_grid"
  )
  # NOTE: Avoid warning about centroids for longitude/latitude data.
  suppressWarnings(
    grid_cent <- sf::st_centroid(grid_sf)
  )
  grid_cent <- sf::st_coordinates(grid_cent)
  grid_cent <- as.data.frame(floor(grid_cent))
  grid_cent <- as.data.frame(lapply(grid_cent, as.integer))
  grid_sf["id_grid"] <- paste(grid_cent[["X"]], grid_cent[["Y"]], sep = "_")

  # Get the cell id correspoinding to each point.
  # NOTE: Avoid warning about spatially constant variables.
  sf::st_agr(points_sf) <- "constant"
  # NOTE: Avoid warnings about assuming planar coordinates during intersection.
  suppressWarnings(
    points_sf <- sf::st_intersection(
      x = points_sf,
      y = grid_sf
    )
  )
  expect_true(nrow(points_sf) == n_points)
  points_df <- sf::st_drop_geometry(points_sf)
  points_df <- points_df[order(points_df[["id_point"]]), ]

  # Use the function in this package.
  ji_df <- as.data.frame(ji(
    xy = data_df[, c("longitude", "latitude")],
    origin = grid_origin,
    cellsize = grid_size
  ))
  expect_equal(nrow(ji_df), expected = n_points)
  ji_df["id_point"] <- data_df[["id_point"]]
  ji_df["id_grid"] <- paste(
    ji_df[["longitude"]],
    ji_df[["latitude"]],
    sep = "_"
  )
  ji_df <- ji_df[c("id_point", "id_grid")]
  ji_df <- ji_df[order(ji_df[["id_point"]]), ]

  # Test.
  expect_equal(
    object = ji_df,
    expected = points_df
  )
})



test_that("add_cell_ids works", {
  # Parameters.
  n_points <- 50L
  grid_size <- c(4, 2)
  grid_origin <- c(0, 0)

  suppressMessages(
    sf::sf_use_s2(use_s2 = FALSE)
  )

  # Create random points.
  data_df <- data.frame(
    longitude = runif(min = -179, max = 179, n = n_points),
    latitude = runif(min = -89, max = 89, n = n_points),
    id_point = seq(n_points)
  )
  points_sf <- sf::st_as_sf(
    x = data_df,
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  )

  # Create a grid.
  grid_sf <- make_grid_origin_res(
    xy_origin = grid_origin,
    xy_min = c(-180, -90),
    xy_max = c(180, 90),
    cell_size = grid_size,
    crs = 4326,
    id_col = "id_grid"
  )
  # NOTE: Avoid warning about centroids for longitude/latitude data.
  suppressWarnings(
    grid_cent <- sf::st_centroid(grid_sf)
  )
  grid_cent <- sf::st_coordinates(grid_cent)
  grid_cent <- as.data.frame(floor(grid_cent))
  grid_cent <- as.data.frame(lapply(grid_cent, as.integer))
  grid_sf["id_grid"] <- paste(grid_cent[["X"]], grid_cent[["Y"]], sep = "_")

  # Get the cell id correspoinding to each point.
  # NOTE: Avoid warning about spatially constant variables.
  sf::st_agr(points_sf) <- "constant"
  # NOTE: Avoid warnings about assuming planar coordinates during intersection.
  suppressWarnings(
    points_sf <- sf::st_intersection(
      x = points_sf,
      y = grid_sf
    )
  )
  expect_true(nrow(points_sf) == n_points)
  points_df <- sf::st_drop_geometry(points_sf)
  points_df <- points_df[order(points_df[["id_point"]]), ]

  # Use the function in this package.
  cells_df <- add_cell_ids(
    data_tb = data_df[, c("longitude", "latitude")],
    grid_origin = grid_origin,
    grid_size = grid_size
  )
  cells_df["id_point"] <- seq_len(nrow(cells_df))
  cells_df <- cells_df[c("id_point", "cell_id")]
  colnames(cells_df) <- c("id_point", "id_grid")
  expect_equal(nrow(cells_df), expected = n_points)

  # Test.
  expect_equal(
    object = cells_df,
    expected = points_df
  )
})
