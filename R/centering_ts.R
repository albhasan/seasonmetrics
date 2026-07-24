#' Arrange one cycle in each column
#'
#' @description
#' Given a vector of observations and the number of time cycles it covers,
#' return a matrix where the observations of each cycle are in their own
#' column.
#'
#' @param x a numeric. Data observations.
#' @param n_cycles an integer(1). The number of times x cycles over.
#'
#' @return a numeric matrix.
#'
#' @export
#'
get_cycle_matrix <- function(x, n_cycles) {
  stopifnot("Invalid number of observations!" = length(x) > 0)
  stopifnot("Invalid number of cycles!" = n_cycles > 0)
  x_mt <- matrix(data = x, ncol = 1)
  if (n_cycles > 1) {
    x_mt <- matrix(
      data = x,
      ncol = n_cycles,
      byrow = FALSE,
      dimnames = list(NULL, paste0("cycle_", seq(n_cycles)))
    )
  }
  return(x_mt)
}


#' Center around the peak value
#'
#' @description
#' Displaces the rows in the given matrix so that the peak-value row is at the
#' center position. The peak-value row is found by aggregating the row values
#' using the given function. The displacement ensures the relative position of
#' rows is preserved.
#'
#' @param x_mt a numeric matrix. A matrix of cyclic observations; each time
#' cycle has its own column.
#' @param f a character(1). A function for aggregating data across cycles, that
#' is, by row.
#'
#' @return a data frame with at least 4 columns: the original position of each
#' observation (pos), the centered position (center_pos), and the translation
#' necessary to return to the original position (center_trans). The x column is
#' reordered according to center_pos.
#'
center_peak <- function(x_mt, f) {
  stopifnot("A matrix is expected!" = inherits(x_mt, what = "matrix"))

  # Estime the expected value by row.
  x_expected <- apply(
    X = x_mt,
    MARGIN = 1,
    FUN = f
  )

  # Find the position of the maximum value.
  pos_mid <- which.max(x_expected) - (length(x_expected) / 2)

  # Move the values to fit the maximum in the middle.
  x_mt_displaced <-
    apply(
      X = x_mt,
      MARGIN = 2,
      FUN = displace_vec,
      n_pos = pos_mid,
      simplify = TRUE
    )

  # Build a data frame with the diplaced vectors.
  data_df <- data.frame(x = x_mt_displaced)
  data_df[["pos"]] <- displace_vec(
    x = seq_len(nrow(x_mt_displaced)),
    n_pos = pos_mid
  )
  data_df[["center_pos"]] <- seq_len(nrow(x_mt_displaced))
  data_df[["center_trans"]] <- data_df[["pos"]] - data_df[["center_pos"]]

  return(data_df)
}


#' Uncentering an observation position
#'
#' @description
#' This function undoes the effects of [center_peak] by compensating the given
#' position to its original place.
#'
#' @param pos a numeric(1). A centered position of an observation in a vector.
#' @param x_df a data frame. This data frame contains columns corresponding to
#'   observations (x), their original positions (pos), their positions centered
#'   (center_pos) and the translation requited to return the centered positions
#'   to their original place (center_trans).
#'
#' @return a numeric. The transformation constant to return the given centered
#'  position to its original place.
#'
un_center <- function(pos, x_df) {
  cen_pos <- which.min(abs(x_df[["center_pos"]] - pos))
  return(x_df[["center_trans"]][x_df[["center_pos"]] == cen_pos])
}


#' Displace vector elements
#'
#' @description
#' Displace the elements in the given vector (to the left) by certain number of
#' positions.
#'
#' @param x A vector.
#' @param n_pos an integer. Number of positions to displace the vector's
#' elements.
#'
#' @return the given vector with elements in a different order.
#'
displace_vec <- function(x, n_pos) {
  stopifnot("Invalid n_pos!" = abs(n_pos) <= length(x))
  if (n_pos < 0) {
    n_pos <- n_pos + length(x)
  }
  return(rep(x, 2)[(n_pos + 1):(length(x) + n_pos)])
}
