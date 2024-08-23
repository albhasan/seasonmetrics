#library(data.table)
library(dplyr)
#library(dtplyr)
library(furrr)
library(lubridate)
library(purrr)
library(readr)
library(rlang)
library(rlog)
library(sf)
library(stringr)
library(tidyr)

## Install seasonmetrics
# library(devtools)
# load_all()
# document()
# install()
library(seasonmetrics)



#---- Setup ----
rlog::log_info("Start new process -------------------------------------------")

# VIIRS data.
csv_files <- "/home/alber/Documents/github/seasonmetrics/inst/extdata/VIIRS"
stopifnot("CSV directory not found!" = dir.exists(csv_files))

out_dir <- "/home/alber/Documents/github/seasonmetrics/inst/extdata/results"
stopifnot("Output directory not found!" = dir.exists(out_dir))

files_df_rds <- file.path(out_dir, "files_df.rds")
stopifnot("Previous `files_df_rds` file found!" = !file.exists(files_df_rds))

# Grid.
xy_min <- c(-180, -90)
xy_max <- c(180, 90)
grid_crs <- 4326
id_col = "cell_id"


#NOTE: Use coarser resolution when debugging.
grid_cells <- c(cols = 1440, rows = 720)
#grid_cells <- c(cols = 144, rows = 72)

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
cores_process_csv <- 1L
cores_compute_season <- 2L


#---- Utility ----
rlog::log_info("Loading utility functions...")

process_csv <- function(x, xy_min, xy_max, grid_cells, grid_crs, id_col) {

    fire_grid <- make_grid_min_max_cells(xy_min = xy_min, xy_max = xy_max,
        n = grid_cells, crs = grid_crs, id_col = id_col)

    data_tb <- 
        x %>%
        readr::read_csv(show_col_types = FALSE) %>%
        #data.table::as.data.table() %>%
        dplyr::select(longitude, latitude, acq_date) %>%
        #NOTE: Use a subsample when debugging.
        #dplyr::sample_n(1000) %>%
        dplyr::mutate(ac_date = lubridate::as_date(acq_date),
                      year = lubridate::year(ac_date),
                      month = lubridate::month(ac_date)) %>%
        dplyr::select(longitude, latitude, year, month)

    data_sf <-
        data_tb %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = grid_crs) %>%
        sf::st_join(y = fire_grid, join = sf::st_within)
    rm(data_tb)

    res <-
        data_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::count(.data[[id_col]], year, month, name = "n_points") %>%
        tibble::as_tibble()

    rm(data_sf)

    gc()
    return(res)

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
    #dplyr::slice(1:2) %>%
    dplyr::mutate(
        data = furrr::future_map(file_path, process_csv,
            xy_min = xy_min, xy_max = xy_max, grid_cells = grid_cells,
            grid_crs = grid_crs, id_col = id_col, 
            .options = furrr::furrr_options(seed = 123))
    )

future::plan(sequential)

saveRDS(
    object = files_df, 
    file = files_df_rds
)

rlog::log_info("Computing month_sum...")
month_sum <-
    files_df %>%
    dplyr::pull(data) %>%
    dplyr::bind_rows() %>%
    #data.table::as.data.table() %>%
    dplyr::group_by(.data[[id_col]], month) %>%
    dplyr::summarize(monthly_sum = sum(n_points)) %>%
    dplyr::mutate(month = stringr::str_pad(month, pad = 0, width = 2)) %>%
    tidyr::pivot_wider(names_prefix = "m", names_from = month,
                       values_from = monthly_sum, values_fill = 0) %>%
    dplyr::select(sort(colnames(.))) %>%
    tibble::as_tibble()

fire_grid <- make_grid_min_max_cells(xy_min = xy_min, xy_max = xy_max,
    n = grid_cells, crs = grid_crs, id_col = id_col)

# Export the merged sf object.
sf::st_write(
    merge(fire_grid, month_sum, by = id_col),
    file.path(out_dir, "month_sum.gpkg"),
    delete_layer = TRUE
)

# Average count for each month.
rlog::log_info("Computing month_df...")
month_df <-
    files_df %>%
    dplyr::pull(data) %>%
    dplyr::bind_rows() %>%
    #data.table::as.data.table()
    dplyr::group_by(.data[[id_col]], month) %>%
    dplyr::summarize(
        min_n = min(n_points),
        mean_n = mean(n_points),
        max_n = max(n_points),
        sd_n = sd(n_points)
    ) %>%
    tibble::as_tibble()

if (cores_compute_season > 1) {
    future::plan(multisession, workers = cores_compute_season)
    options <- furrr::furrr_options(seed = 123)
}

rlog::log_info("Computing season using peak and threshold...")
season_peak_thres_df <-
    month_df %>%
    dplyr::select({{id_col}}, month, max_n) %>%
    dplyr::arrange({{id_col}}, month) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::group_split() %>%
    furrr::future_map(function(x, id_col) {
        c_id <- unique(x[[id_col]])[1]
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
        x %>%
            dplyr::arrange(month) %>%
            dplyr::pull(max_n) %>%
            compute_season_peak_threshold(threshold_cons = 0.6) %>%
            dplyr::mutate("{id_col}" := c_id) %>%
            return()
    }, id_col = id_col) %>%
    dplyr::bind_rows()

# Export the merged sf.
sf::st_write(
    merge(fire_grid, season_peak_thres_df, by = id_col),
    file.path(out_dir, "season_peak_thres_df.gpkg"),
    delete_layer = TRUE
)

rlog::log_info("Computing season using double sigmoidal...")
season_dsig_df <-
    month_df %>%
    dplyr::select({{id_col}}, month, max_n) %>%
    dplyr::arrange({{id_col}}, month) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::group_split() %>%
    furrr::future_map(function(x, id_col) {
        c_id <- unique(x[[id_col]])[1]
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
        x %>%
            dplyr::arrange(month) %>%
            dplyr::pull(max_n) %>%
            compute_season_double_sig() %>%
            dplyr::mutate("{id_col}" := c_id) %>%
            return()
    }, id_col = id_col) %>%
    dplyr::bind_rows()

future::plan(sequential)

# Export the merged sf.
sf::st_write(
    merge(fire_grid, season_dsig_df, by = id_col),
    file.path(out_dir, "season_dsig_df.gpkg"),
    delete_layer = TRUE
)

rlog::log_info("Finished!")

