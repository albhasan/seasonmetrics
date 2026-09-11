# Number of cores to use in parallel when processing CSV files.
cores_process_csv <- 8L
rlog::log_info("Number of cores for processing CSVs: ", cores_process_csv)

# Number of cores to use in parallel to run regressions.
cores_process_season <- 32
rlog::log_info("Number of cores for computing seasons: ", cores_process_season)
