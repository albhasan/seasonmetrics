###############################################################################
# 00 AGGREGATE VIIRS DATA
# NOTE: The results are 19M. The maximum allowed in a package is 5M
###############################################################################

library(dplyr)
library(furrr)
library(future)
library(rlog)

library(seasonmetrics)


rlog::log_info("Start new process 00_aggregate_by_year ----------------------")

#---- Setup ----

rlog::log_info("Reading parameters...")

# VIIRS data.
csv_files <- "/home/alber/Documents/data/r_packages/seasonmetrics/VIIRS"
stopifnot("CSV directory not found!" = dir.exists(csv_files))

out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs"
stopifnot("Output directory not found!" = dir.exists(out_dir))

files_df_rds <- file.path(out_dir, "files_df.rds")
stopifnot("Previous `files_df_rds` file found!" = !file.exists(files_df_rds))

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
