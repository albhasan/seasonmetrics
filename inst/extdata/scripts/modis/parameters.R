# Input fire data.
csv_dir <- "/home/alber/Documents/data/r_packages/seasonmetrics/modis"
rlog::log_info("CSV directory: ", csv_dir)
stopifnot("Input directory not found!" = dir.exists(csv_dir))

# Directory for storing results.
out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_12/modis"
rlog::log_info("Output directory: ", out_dir)
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Load previous results.
files_df_rds <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_12/modis/files_df.rds"
rlog::log_info("Looking for a file of points aggregated into a grid...")
if (file.exists(files_df_rds)) {
  rlog::log_info(sprintf("File found at %s", files_df_rds))
} else {
  rlog::log_info("File not found. This probably is a new run.")
}

rlog::log_info("Loading previous results, if available: ", out_dir)
stopifnot(
  "Previous results' directory not found!" = dir.exists(dirname(files_df_rds))
)

# Number of years for aggregating data.
n_years <- 5
rlog::log_info("Number of years of aggregation: ", n_years)

# Minimum number of montly observations in a cell before trying a regression.
min_number_of_months <- 7
rlog::log_info(
  "Mininum number of time steps before trying a regression: ",
  min_number_of_months
)
