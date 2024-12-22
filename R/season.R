#' Compute a season using cyclical observations
#'
#' @description
#' Cyclical observations refers to vector on which the element before the first
#' is the last one, and the element after the last vector element is the first
#' one.
#'
#' `compute_season_peak_threshold` estimate season's parameters using the
#' season maximum value and a threshold. For example, given a year of monthly
#' observations, the peak season is the minimum subset of consecutive values
#' around the maximum value (the peak) that reach the given threshold.
#'
#' `compute_season_double_sig` estimate season's parameters by adjusting a
#' double sigmoidal function to the observations.
#'
#' @param x a numeric. A vector of cyclical observations.
#' @param threshold_cons a numeric(1) between 0 and 1. The percentage of the
#'   total a season must reach.
#'
#' @return a data frame with metrics:
#'* `pos_from` and `pos_to` are the indices of the season's first and last
#'   elements in `x`.
#'* `val_from` and `val_to` area the values at `pos_from` and `pos_to`.
#'* `pos_min` and `pos_max` are the indices where the season reaches its
#'   mininum and maximum values.
#'* `val_min` and `val_max` are the values at `pos_min` and `pos_max`.
#'* `val_len` is the season length. Note that the first element is included
#'   when estimating the length for `compute_season_peak_threshold`.
#'* `val_mean` is the mean of the values in the season.
#'* `val_sd` is the standard deviation of the values in the season.
#'
#' @export
#'
compute_season_peak_threshold <- function(x, threshold_cons) {

  stopifnot(
    "Invalid trehshold!" = all(0 < threshold_cons, threshold_cons <= 1)
  )
  stopifnot("Too few observations!" = length(x) > 1)
  stopifnot("Can't handle NAs!" = sum(is.na(x)) == 0)

  res <- get_na_df()

  # Test if the time series is flat.
  if (length(unique(x)) == 1) {
    return(res)
  }

  # Estimate the actual threshold.
  threshold <- sum(x) * threshold_cons

  # The season's start value position (the peak) is the seed of the season.
  season_pos <- as.integer(which.max(x))

  for (i in 1:(length(x) - 1)) {
    if (sum(x[season_pos]) >= threshold) {
      break
    }

    next_pos <- get_prev_next(
      y = season_pos,
      total_len = length(x)
    )

    if (x[next_pos[1]] >= x[next_pos[2]]) {
      season_pos <- c(next_pos[1], season_pos)
    } else {
      season_pos <- c(season_pos, next_pos[2])
    }
  }

  res["pos_from"] <- season_pos[1]
  res["val_from"] <- x[season_pos[1]]
  res["pos_to"]   <- season_pos[length(season_pos)]
  res["val_to"]   <- x[season_pos[length(season_pos)]]
  res["pos_min"]  <- season_pos[which.min(x[season_pos])]
  res["val_min"]  <- min(x[season_pos])
  res["pos_max"]  <- season_pos[which.max(x[season_pos])]
  res["val_max"]  <- max(x[season_pos])
  res["val_len"]  <- length(season_pos)
  res["val_mean"] <- mean(x[season_pos])
  res["val_sd"]   <- stats::sd(x[season_pos])

  return(res)
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



#' @rdname compute_season_peak_threshold
#'
#' @param n_runs_min,n_runs_max an integer(1). Minimum and maximum number of
#'   successful fitting attempts.
#'
#' @export
#'
compute_season_double_sig <- function(x, n_runs_min = 20, n_runs_max = 500) {

  stopifnot("Too few observations!" = length(x) > 1)
  stopifnot("I can't handle NAs!" = sum(is.na(x)) == 0)

  # Return if the given time series is flat.
  res <- get_na_df()
  if (length(unique(x)) == 1) {
    return(res)
  }

  # Translate data.
  v_min <- min(x)
  x <- x - v_min

  # Center around the peak.
  x_df <- center_peak(x)
  x_df["center_trans"] <- x_df[["pos"]] - x_df[["center_pos"]]

  # Prepare data for regression.
  sicegar_df <- data.frame(
    intensity = x_df[["x"]],
    time      = seq_len(nrow(x_df))
  )
  sic_norm_df <- sicegar::normalizeData(sicegar_df)

  # Do the double-sigmoidal fit
  model_fit <- sicegar::multipleFitFunction(
    dataInput = sic_norm_df,
    model = "doublesigmoidal",
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max
  )

  # Check that the model fits.
  if (!model_fit[["isThisaFit"]]) {
    return(get_na_df())
  }

  # Estimate additional parameters.
  m_par <- sicegar::parameterCalculation(model_fit)

  # Build a data frame with season parameters.
  res["val_from"] <- m_par[["midPoint1_y"]]    + x_min
  res["val_to"]   <- m_par[["midPoint2_y"]]    + x_min
  res["val_max"]  <- m_par[["reachMaximum_y"]] + x_min
  res["val_len"] <- ifelse(
    res[["pos_from"]] <= res[["pos_to"]],
    res[["pos_to"]] - res[["pos_from"]],
    (res[["pos_to"]] + length(x)) - res[["pos_from"]]
  )
  res["pos_from"] <-
    (m_par[["midPoint1_x"]] +
     un_center(m_par[["midPoint1_x"]], x_df = x_df)) %% length(x)
  res["pos_to"] <-
    (m_par[["midPoint2_x"]] +
     un_center(m_par[["midPoint2_x"]], x_df = x_df)) %% length(x)
  res["pos_max"] <-
    (m_par[["reachMaximum_x"]] +
     un_center(m_par[["reachMaximum_x"]], x_df = x_df)) %% length(x)

  return(res)
}



#' Uncentering an observation position
#'
#' @description
#' This function undoes the effects of `center_peak` by compensating the given
#' position to its original place.
#'
#' @param pos a numeric(1). A centered position of an observation in a vector.
#' @param x_df a data frame. This data frame contains columns correspondign to
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



#' Center around the peak value
#'
#' @description
#' Center the given vector around its peak (maximum value). In this way, the
#' peak would be at or close to the center position in the vector.
#'
#' @param x a numeric. A vector of cyclic observations.
#'
#' @return a data frame with 3 columns: x, the original position of each
#' observation (pos), and the centered position (center_pos). The x column is
#' reordered according to center_pos.
#'
center_peak <- function(x) {
  data_df <- data.frame(
    x = x,
    pos = seq_along(x)
  )
  data_df["center_pos"] <- displace_vec(
    x,
    n_pos = which.max(x) - (length(x) / 2)
  )
  data_df <- data_df[order(data_df[["center_pos"]]), ]
  return(data_df)
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
  if (n_pos < 0)
    n_pos <- n_pos + length(x)
  return(rep(x, 2)[(n_pos + 1):(length(x) + n_pos)])
}



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
    val_sd   = NA
  ))
}
