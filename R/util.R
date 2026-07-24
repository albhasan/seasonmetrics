#' Compute statistics over the column of a CSV file
#'
#' @description
#' Read the given CSV file and compute statistics over its columns.
#'
#' @param x A character. Path to a CSV file.
#'
#' @return a data frame with statistics.
#'
get_field_stats <- function(x) {
  longitude <- latitude <- acq_date <- NULL
  min_max_len <- list(
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    len = ~ length(.x)
  )
  res <-
    x |>
    readr::read_csv() |>
    dplyr::select(longitude, latitude, acq_date) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), min_max_len))
  return(res)
}
