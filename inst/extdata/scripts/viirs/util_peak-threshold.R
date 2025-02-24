#' Compute the peak seaon using a threshold
#'
#' @description
#' Utility funcion for computing the peak threshold.
#'
#' @param x a data frame.
#' @param id_group a character. Name of the group column in x.
#' @param id_col a character. Name of the ID column in x.
#' @param val_col a character. Name of the column with a value.
#' @param month_col a character. Name of the column with month numbers.
#'
#' @return a data frame (tibble).
#'
season_peak_thres_helper <- function(
    x, id_group, id_col,
    val_col, month_col, threshold_cons) {
  g_id <- unique(x[[id_group]])
  stopifnot("Only one group ID allowed!" = length(g_id) == 1)
  c_id <- unique(x[[id_col]])[1]
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
    seasonmetrics::compute_season_peak_threshold(
      threshold_cons = threshold_cons
    ) |>
    dplyr::mutate(
      "{id_group}" := g_id,
      "{id_col}" := c_id
    )

  return(res)
}
