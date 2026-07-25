# Input fire data.
csv_dir <- "/home/alber/Documents/data/r_packages/seasonmetrics/modis"
rlog::log_info("CSV directory: ", csv_dir)
stopifnot("Input directory not found!" = dir.exists(csv_dir))

# Directory for storing results.
out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_11/modis"
rlog::log_info("Output directory: ", out_dir)
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Load previous results.
files_df_rds <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_11/modis/files_df.rds"
rlog::log_info("Loading previous results, if available: ", out_dir)
stopifnot(
  "Previous results' directory not found!" = dir.exists(dirname(files_df_rds))
)
