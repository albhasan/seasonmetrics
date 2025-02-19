#' Sort columns by name
#'
#' @description
#' Sort the columns by name in a data frame.
#'
#' @param df a data frame.
#'
#' @return a data frame.
#'
#' @export
#'
sort_cols <- function(df) {
  df <-
    df |>
    dplyr::select(sort(colnames(df)))
  return(df)
}
