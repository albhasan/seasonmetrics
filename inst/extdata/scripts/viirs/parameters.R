# Input fire data.
csv_files <- "/home/alber/documents/data/r_packages/seasonmetrics/viirs"
stopifnot("Input directory not found!" = dir.exists(csv_files))

# Directory for storing results.
out_dir <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs"
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Load previous results.
files_df_rds <-
  "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs/files_df.rds"
stopifnot("`files_df_rds` file not found!" = file.exists(files_df_rds))
