#' Compute the peak seaon using a double sigmoidal function
#'
#' @description
#' Utility funcion for computing the peak season using a double sigmoidal
#' function fit.
#'
#' @param x a data frame.
#' @param id_group a character. Name of the group column in x.
#' @param id_col a character. Name of the ID column in x.
#' @param val_col a character. Name of the column with a value.
#' @param month_col a character. Name of the column with month numbers.
#'
#' @return a data frame (tibble).
#'
season_peak_dsig_helper <- function(
    x, id_group, id_col, val_col,
    month_col, n_runs_min, n_runs_max) {
  g_id <- unique(x[[id_group]])
  stopifnot("Only one group ID allowed!" = length(g_id) == 1)
  c_id <- unique(x[[id_col]])
  stopifnot("Only one ID allowed!" = length(c_id) == 1)
  # Fill in values for the missing months.
  if (!all(1:12 %in% x[[month_col]])) {
    complement_df <-
      tibble::tibble(
        "{id_group}" := g_id,
        "{id_col}" := c_id,
        "{month_col}" := (1:12)[!(1:12 %in% x[[month_col]])],
        "{val_col}" := 0
      )
    x <-
      x |>
      dplyr::bind_rows(complement_df)
  }
  # Compute the season.
  res <-
    x |>
    dplyr::arrange(.data[[month_col]]) |>
    dplyr::pull(tidyselect::all_of(val_col)) |>
    seasonmetrics::compute_season_double_sig(
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    ) |>
    dplyr::mutate(
      "{id_group}" := g_id,
      "{id_col}" := c_id
    )

  return(res)
}
