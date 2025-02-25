# Input fire data.
csv_files <- "/home/alber/Documents/data/r_packages/seasonmetrics/modis"
stopifnot("Input directory not found!" = dir.exists(csv_files))

# Directory for storing results.
out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/modis"
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Load previous results.
files_df_rds <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/modis/files_df.rds"
