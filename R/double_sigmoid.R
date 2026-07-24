# TODO: Remove this function and file.
# Prepare for double sigmoidal regression
#
# @description
# Transform the input data frame into one compatible with the regression
# functions available in the `sicegar` package.
#
# @param x a data frame resulting from a call to [center_peak].
# @param n_cycles an integer(1). Number of cycles contained in `x`.
#
# @return An object of
#
# fit_ds <- function(x, n_cycles) {
#   # Prepare data for regression.
#   # NOTE: sicegar always uses two columns: intensity and time.
#   sicegar_df <- data.frame(
#     intensity = unlist(x[, 1:n_cycles]),
#     time = rep(x[["center_pos"]], times = n_cycles)
#   )
#
#   sic_norm_df <- sicegar::normalizeData(sicegar_df)
#
#   # Do the double-sigmoidal fit
#   model_fit <- sicegar::multipleFitFunction(
#     dataInput = sic_norm_df,
#     model = "doublesigmoidal",
#     n_runs_min = n_runs_min,
#     n_runs_max = n_runs_max
#   )
# }
