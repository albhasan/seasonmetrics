#' Build an empty season data frame
#'
#' @description
#' Create a data frame of NAs with the expected columns of a season data frame.
#'
#' @return a data frame.
#'
get_na_df <- function() {
  return(data.frame(
    pos_from = NA,
    val_from = NA,
    pos_to   = NA,
    val_to   = NA,
    pos_min  = NA,
    val_min  = NA,
    pos_max  = NA,
    val_max  = NA,
    val_len  = NA,
    val_mean = NA,
    val_sd   = NA,
    coverage = NA
  ))
}

#' Determine the next values for computing season
#'
#' @description
#' Utility function. Get the next values to evaluate.
#'
#' @param y an integer vector with the current season (positions of the season
#'   values).
#' @param total_len an integer(1). The total number of elements is a cycle.
#'
#' @return  an integer(2) with the month before and after the given season.
#'
get_prev_next <- function(y, total_len) {
  stopifnot("Invalid parameters!" = total_len > length(y))
  y <- (c(y[1] - 1, y[length(y)] + 1) + total_len) %% total_len
  y <- replace(y, y == 0, total_len)
  return(y)
}
