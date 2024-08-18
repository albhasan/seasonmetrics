#' Build a grid that can be used later for aggregating geographic data
#'
#' @description
#' Build a grid around spatial data (vector or raster) or from scratch.
#'
#' `make_grid` returns a grid around spatial data (vector or raster).
#'
#' @param x An sf or terra object.
#' @param n integer of lenght 1 or 2. Number of cells (columns, rows).
#' @param cellsize numeric of length 1 or 2 (width, length). It defaults to the
#'   width and heigth of the bounding box of the given object divided by the
#'   number of cells.
#' @param id_col character of length 1. Name of the column for identifying grid
#'   cells.
#' @param add_row_col Either logical(1) or character(2). Should columns
#'   identifying the row and column of each cell be added? If so, provide
#'   their names; otherwise default names are used
#' @param add_centroids Either logical(1) or character(2). Should columns with
#'   the coordinates of the cells' centroids be added? If so, provide their
#'   names (X & Y); otherwise default names are used.
#' @param add_area Either logical(1) or character(1). Should a column with the
#' cell's area be added? If so, provide its name; otherwise a default name is
#' used.
#' @param drop_area_units A logical. Drop the area units?
#'
#' @return An `sf` object with polygons.
#'
#' @seealso [sf::st_make_grid()] which this function wraps.
#'
#' @export
#'
make_grid <- function(x, n,
                      cellsize = c(diff(sf::st_bbox(x)[c(1, 3)]),
                                   diff(sf::st_bbox(x)[c(2, 4)]))/n,
                      id_col = "id",
                      add_row_col = FALSE, 
                      add_centroids = FALSE, 
                      add_area = FALSE,
                      drop_area_units = FALSE) {

    stopifnot("Invalid n" = all(n > 0))
    stopifnot("Invalid n" = length(n) %in% 1:2)
    stopifnot("Invalid add_row_col Expected logical(1) or character(2)" = 
        all(
            (is.logical(add_row_col) & length(add_row_col) == 1) | 
            (is.character(add_row_col)) & length(add_row_col == 2)
        )
    )
    stopifnot("Invalid add_centroids. Expected logical(1) or character(2)" = 
        all(
            (is.logical(add_centroids) & length(add_centroids) == 1) | 
            (is.character(add_centroids)) & length(add_centroids == 2)
        )
    )

    if (inherits(x, what = "SpatVector"))
        x <- sf::st_as_sf(x)

    if (inherits(x, what = "SpatRaster")) {
        xy_mat <- matrix(as.vector(terra::ext(x)), ncol = 2, byrow = FALSE)
        xy_line <- sf::st_linestring(xy_mat, dim = "XY")
        x <- sf::st_sf(sf::st_sfc( xy_line, crs = terra::crs(x)))
    }

    if (is.na(sf::st_crs(x)))
        stop("CRS is missing!")

    if (min(cellsize) != max(cellsize))
        warning("The grid's cells aren't squares!")

    grid <- sf::st_make_grid(
        x,
        cellsize = cellsize,
        n = n,
        what = "polygons",
        square = TRUE,
        flat_topped = FALSE
    )

    # Handle the id column.
    data_df <- data.frame(1:length(grid))
    colnames(data_df) <- id_col

    # Handle the row and column columns.
    rc_df <- expand.grid(grid_col = 1:n[1], grid_row = 1:n[length(n)])
    if (all(is.logical(add_row_col), add_row_col))
        data_df <- cbind(data_df, rc_df)
    if (is.character(add_row_col)) {
        colnames(rc_df) <- c(add_row_col[2], add_row_col[1])
        data_df <- cbind(data_df, rc_df)
    }

    # Handle the geometry column.
    data_df["geometry"] <- list(grid)
    data_sf <- sf::st_sf(data_df)

    # Handle the centroids' columns.
    if (all(is.logical(add_centroids), add_centroids))
        data_sf <- add_centroids(data_sf)
    if (is.character(add_centroids))
        data_sf <- add_centroids(data_sf, col_names = add_centroids)

    # Handle the area column.
    if (all(is.logical(add_area), add_area))
        data_sf <- add_area(data_sf)
    if (is.character(add_area))
        data_sf <- add_area(data_sf, col_name = add_area,
                            drop_units = drop_area_units)

    return(data_sf)

}

#' @rdname make_grid
#' 
#' @description
#' `make_grid_min_max_cells` returns a grid using the minimum & maximum 
#' coordinates, and the number of cells in the grid.
#'
#' @param xy_min,xy_max A pair of numeric vectors with the minimum and maximum
#'   coordiantes of the grid.
#' @param crs Numeric or character representing a Coordinate Reference System.
#'
#' @export
#'
make_grid_min_max_cells <- function(xy_min, xy_max, n, crs = 4326, 
                                    id_col = "id", add_row_col = FALSE, 
                                    add_centroids = FALSE, add_area = FALSE, 
                                    drop_area_units = FALSE) {

    stopifnot("Invalid xy_min. A numeric is expected." = all(
        is.numeric(xy_min),
        length(xy_min) %in% 1:2
    ))
    stopifnot("Invalid xy_max. A numeric is expected." = all(
        is.numeric(xy_max),
        length(xy_max) %in% 1:2
    ))

    xy_mat <- matrix(c(xy_min, xy_max), ncol = 2, byrow = TRUE)
    xy_line <- sf::st_linestring(xy_mat, dim = "XY")
    x <- sf::st_sf(sf::st_sfc( xy_line, crs = crs))

    return(make_grid(
        x, 
        n = n,
        id_col = id_col,
        add_row_col = add_row_col,
        add_centroids = add_centroids,
        add_area = add_area,
        drop_area_units = drop_area_units
    ))

}


#' @rdname make_grid
#' 
#' @description
#' `make_grid_origin_dist` returns a grid using the coordinates of an origin 
#'  point (minimum XY), the number of cells, and the number of cells in the 
#'  grid.
#'
#' @param cell_size A numeric of length 1 or 2. The width and heigth o a cell.
#'   The units are those of the `crs`.
#'
#' @export
#'
make_grid_origin_dist <- function(xy_min, n, cell_size, crs, id_col = "id",
                                  add_row_col = FALSE, add_centroids = FALSE,
                                  add_area = FALSE, drop_area_units = FALSE) {

    stopifnot("Invalid xy_min. A numeric(1 or 2) is expected." = all(
        is.numeric(xy_min),
        length(xy_min) %in% 1:2
    ))
    stopifnot("Invalid cell_size A numeric(1 or 2) is expected." = all(
        is.numeric(cell_size),
        length(cell_size) %in% 1:2
    ))

    if (length(n) == 1)
        n <- c(n, n)
    if (length(cell_size) == 1)
        cell_size <- c(cell_size, cell_size)

    xy_max <- xy_min + (n * cell_size)
    xy_mat <- matrix(c(xy_min, xy_max), ncol = 2, byrow = TRUE)
    xy_line <- sf::st_linestring(xy_mat, dim = "XY")
    x <- sf::st_sf(sf::st_sfc( xy_line, crs = crs))

    return(make_grid(
        x,
        n = n,
        id_col = id_col,
        add_row_col = add_row_col,
        add_centroids = add_centroids,
        add_area = add_area,
        drop_area_units = drop_area_units
    ))

}



#' Build a vector from the origin approximating the minimum and maximum values
#'
#' @description
#' Build a vector between the minimum and maximum ensuring that the given
#' origin value is part of the returned vector.
#'
#' @param o   a numeric(1). Origin.
#' @param min a numeric(1). Mininum value.
#' @param max a numeric(1). Maximum value.
#' @param res a numeric(1). Resultuion.
#'
#' @return    A numeric.
#'
grid_helper <- function(o, min, max, res) {
  sort(c(seq(from = o, to = max, by = res),
         seq(from = o, to = min, by = -res)[-1]))
}



#' @rdname make_grid
#' 
#' @description
#' `make_grid_origin_res` returns a grid using the coordinates of an origin
#'  point inside an area and a cell size. The new grid will have a vertex on
#'  the origin and will fit as many cells as possible in the area.
#'
#' @param xy_origin A numeric(2) representing an XY point.
#'
#' @export
#'
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
    stopifnot("`cell_sizse` is too large!" =
              all((xy_max - xy_min) >= cell_size))

    lon_grid <- grid_helper(o = xy_origin[1], min = xy_min[1], max = xy_max[1],
                            res = cell_size)
    lat_grid <- grid_helper(o = xy_origin[2], min = xy_min[2], max = xy_max[2],
                            res = cell_size)

    stopifnot("Not enough room to fit a grid!" = 
        all(length(lon_grid) > 1, length(lat_grid) > 1))

    aoi_grid <- sf::st_as_sf(sf::st_make_grid(
        x = sf::st_bbox(c(xmin = min(lon_grid), xmax = max(lon_grid),
            ymin = min(lat_grid), ymax = max(lat_grid)),
            crs = sf::st_crs(crs)),
        cellsize = cell_size
    ))
    aoi_grid[id_col] <- seq(nrow(aoi_grid))

    return(aoi_grid)

}


#' Are grid attributes NAs?
#'
#' @description
#' Test whether all the values in the given grid are NA.
#'
#' @param x a grid (sf object).
#'
#' @return a logical
#'
is_grid_na <- function(x) {
    stopifnot("Expected an `sf` object!" = inherits(x, what = "sf"))
    stopifnot("Expected polygon geometry!" = 
        unique(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))
    return(all(vapply(
        X = colnames(sf::st_drop_geometry(x)),
        FUN = function(v, x) {all(is.na(x[[v]]))},
        FUN.VALUE = logical(1), x = x
    )))
}

