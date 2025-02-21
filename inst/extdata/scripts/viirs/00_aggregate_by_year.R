###############################################################################
# 00 AGGREGATE VIIRS DATA
# NOTE: The results are 14M. The maximum allowed in a package is 5M
###############################################################################

library(dplyr)
library(furrr)
library(future)
library(rlog)

library(seasonmetrics)


rlog::log_info("Start new process -------------------------------------------")

#---- Setup ----

rlog::log_info("Reading parameters...")

csv_files <- out_dir <- files_df_rds <- NULL
source(
  system.file(
    "extdata", "scripts", "viirs", "parameters.R",
    package = "seasonmetrics"
  )
)
stopifnot(
  "Script parameter not found!" =
    all(c("csv_files", "out_dir", "files_df_rds") %in% ls())
)

# Load grid parameters
xy_min <- xy_max <- grid_cells <- NULL
source(
  system.file(
    "extdata", "scripts", "VIIRS", "grid_parameters.R",
    package = "seasonmetrics"
  )
)

stopifnot(
  "Grid parameters not found!" =
    all(c("xy_min", "xy_max", "grid_cells") %in% ls())
)

# Parallel processing.
cores_process_csv <- 2L



#---- Script ----


rlog::log_info("Setting parallel processing...")
if (cores_process_csv > 1) {
  future::plan(multisession, workers = cores_process_csv)
  options <- furrr::furrr_options(seed = 123)
}

rlog::log_info("Listing CSV files...")
files_df <-
  csv_files |>
  list.files(pattern = "*.csv$", full.names = TRUE) |>
  dplyr::as_tibble() |>
  dplyr::rename(file_path = "value")

rlog::log_info("Processing CSV files...")
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

rlog::log_info("Saving results to disk...")
saveRDS(object = files_df, file = files_df_rds)

future::plan(sequential)
gc()

rlog::log_info("Finished!")
