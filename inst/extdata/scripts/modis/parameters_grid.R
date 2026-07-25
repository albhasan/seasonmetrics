# Grid parameters.
xy_min <- c(xmin = -180, ymin = -90)
xy_max <- c(xmax = 180, ymax = 90)
grid_cells <- c(cols = 1440, rows = 720)
grid_crs <- 4326
rlog::log_info("Grid parameters: ", xy_min, xy_max, grid_cells, grid_crs)
