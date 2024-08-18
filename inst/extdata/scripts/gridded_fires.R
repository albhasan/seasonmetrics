library(data.table)
library(dplyr)
library(dtplyr)
library(furrr)
library(lubridate)
library(purrr)
library(raster)
library(readr)
library(rlang)
library(sf)
library(stringr)
library(tidyr)

## Install seasonmetrics
# library(devtools)
# load_all()
# install()
library(seasonmetrics)



#---- Setup ----

# VIIRS data.
csv_files <- "/home/alber/Documents/github/seasonmetrics/inst/extdata/VIIRS"
stopifnot("CSV directory not found!" = dir.exists(csv_files))

out_dir <- "/home/alber/Downloads/tmp"
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Grid.
xy_min <- c(-180, -90)
xy_max <- c(180, 90)
grid_crs <- 4326
id_col = "cell_id"

#NOTE: Use coarser resolution when debugging.
#grid_cells <- c(cols = 144, rows = 72)
grid_cells <- c(cols = 1440, rows = 720)

# Number of cores used when processing in parallel.
# 16GB RAM ~ 1 core.
# 32GB RAM ~ 2 cores.
cores <- 1L


#---- Utility ----

process_csv <- function(x, xy_min, xy_max, grid_cells, grid_crs, id_col) {

    x <- rlang::eval_tidy(x)

    fire_grid <- make_grid_min_max_cells(xy_min = xy_min, xy_max = xy_max,
        n = grid_cells, crs = grid_crs, id_col = id_col)

    x %>%
       #NOTE: Use a subsample when debugging.
       #dplyr::sample_n(1000) %>%
        data.table::as.data.table() %>%
        dplyr::mutate(ac_date = lubridate::as_date(acq_date),
                      year = lubridate::year(ac_date),
                      month = lubridate::month(ac_date)) %>%
        dplyr::select(longitude, latitude, year, month) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = grid_crs) %>%
        sf::st_join(y = fire_grid, join = sf::st_within) %>%
        sf::st_drop_geometry() %>%
        dplyr::count(cell_id, year, month) %>%
        tibble::as_tibble() %>%
        return()

}



#---- Script ----

if (cores > 1) {
    future::plan(multisession, workers = cores)
    options <- furrr::furrr_options(seed = 123)
}

# List files.
files_df <-
    csv_files %>%
    list.files(pattern = "*.csv", full.names = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(file_path = "value") %>%
    #NOTE: Use less files when debugging.
    #dplyr::slice(1:2) %>%
    dplyr::mutate(
        raw_data = purrr::map(file_path, ~rlang::quo(readr::read_csv(.,
                              show_col_types = FALSE))),
        data = furrr::future_map(raw_data, process_csv,
            xy_min = xy_min, xy_max = xy_max, grid_cells = grid_cells,
            grid_crs = grid_crs, id_col = id_col, 
            .options = furrr::furrr_options(seed = 123))
    )

month_sum <-
    files_df %>%
    dplyr::pull(data) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(cell_id, month) %>%
    dplyr::summarize(monthly_sum = sum(n)) %>%
    dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2)) %>%
    tidyr::pivot_wider(names_prefix = "m", names_from = month,
                       values_from = monthly_sum, values_fill = 0) %>%
    dplyr::select(sort(colnames(.)))

fire_grid <- make_grid_min_max_cells(xy_min = xy_min, xy_max = xy_max,
    n = grid_cells, crs = grid_crs, id_col = id_col)

# Export the merged sf as a shapefile.
sf::st_write(
    merge(fire_grid, month_sum, by = id_col),
    file.path(out_dir, "month_sum.shp"),
    delete_layer = TRUE
)

# Average count for each month.
month_df <- 
    files_df %>%
    dplyr::pull(data) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(cell_id, month) %>%
    dplyr::summarize(
        min_n = min(n),
        mean_n = mean(n),
        max_n = max(n),
        sd_n = sd(n)
    )

season_df <-
    month_df %>%
    dplyr::select(cell_id, month, max_n) %>%
    dplyr::arrange(cell_id, month) %>%
    dplyr::group_by(cell_id) %>%
    dplyr::group_split() %>%
    purrr::map(function(x){
        cell_id <- unique(x[["cell_id"]])[1]
        # Fill in values for the missing months.
        if (!all(1:12 %in% x[["month"]])) {
            complement_df <-
                tibble::tibble(
                    cell_id = cell_id,
                    month = (1:12)[!(1:12 %in% x[["month"]])],
                    max_n = 0
                )
            x <-
                x %>%
                dplyr::bind_rows(complement_df)
        }
        # Compute the season.
        season_pos <-
            x %>%
            dplyr::arrange(month) %>%
            dplyr::pull(max_n) %>%
            compute_season(threshold_cons = 0.6)
        # Return a tibble with season statistics.
        return(
            tibble::tibble(
                cell_id = cell_id,
                season_start = season_pos[1],
                season_end   = season_pos[length(season_pos)],
                season_len   = length(season_pos)
            )
        )
    }) %>%
    dplyr::bind_rows()

# Export the merged sf as a shapefile.
sf::st_write(
    merge(fire_grid, season_df, by = "cell_id"),
    file.path(out_dir, "season.shp"),
    delete_layer = TRUE
)

