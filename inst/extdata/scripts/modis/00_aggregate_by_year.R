###############################################################################
# 00 AGGREGATE MODIS DATA
###############################################################################

library(dplyr)
library(furrr)
library(future)
library(rlog)

library(seasonmetrics)


rlog::log_info("Start new process -------------------------------------------")

#---- Setup ----

rlog::log_info("Reading parameters...")

csv_dir <- out_dir <- files_df_rds <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Script parameter not found!" =
    all(c("csv_dir", "out_dir", "files_df_rds") %in% ls())
)

# Load grid parameters
xy_min <- xy_max <- grid_cells <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters_grid.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Grid parameters not found!" =
    all(c("xy_min", "xy_max", "grid_cells") %in% ls())
)

# Load parallel processing parameters.
cores_process_csv <- cores_compute_season <- NULL
source(
  system.file(
    "extdata", "scripts", "modis", "parameters_computing.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Computing parameters not found!" =
    all(c("cores_process_csv", "cores_compute_season") %in% ls())
)



#---- Script ----

rlog::log_info("Listing CSV files...")

files_df <-
  csv_dir |>
  list.files(pattern = "*.csv$", full.names = TRUE) |>
  dplyr::as_tibble() |>
  dplyr::rename(file_path = "value")

rlog::log_info("Setting parallel processing...")
if (cores_process_csv > 1) {
  future::plan(multisession, workers = cores_process_csv)
  options <- furrr::furrr_options(seed = 123)
}

rlog::log_info("Aggratating points into a grid of cells...")
files_df <-
  files_df |>
  dplyr::mutate(
    data = furrr::future_map(
      .x = file_path,
      .f = seasonmetrics::process_csv_fast,
      grid_origin = xy_min,
      grid_size = (xy_max - xy_min) / grid_cells,
      .options = furrr::furrr_options(seed = 123)
    )
  )

future::plan(sequential)
gc()

rlog::log_info(sprintf("Saving results to %s", files_df_rds))
saveRDS(
  object = files_df,
  file = files_df_rds
)

rlog::log_info("Finished!")
