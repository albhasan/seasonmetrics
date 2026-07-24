#' Compute a season using cyclical observations
#'
#' @description
#' Cyclical observations refers to vectors on which the element before the
#' first is the last one, and the element after the last vector element is the
#' first one.
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
#' @param n_cycles a integer(1). Number of cycles in x.
#' @param threshold_cons a numeric(1) between 0 and 1. The percentage of the
#'   total a season must reach.
#' @param f a character(1). A function to apply to aggregate x along cycles.
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
compute_season_peak_threshold <- function(x,
                                          threshold_cons,
                                          n_cycles = 1L,
                                          f = "sum") {
  stopifnot(
    "Invalid trehshold!" = all(0 < threshold_cons, threshold_cons <= 1)
  )
  stopifnot("Too few observations!" = length(x) > 1)
  stopifnot("Can't handle NAs!" = sum(is.na(x)) == 0)
  stopifnot("Invalid number of cycles!" = n_cycles > 0)
  stopifnot("length(x) mod n_cycles must be 0!" = length(x) %% n_cycles == 0)

  # Ensure x is always a matrix.
  if (inherits(x, what = "numeric") || inherits(x, what = "integer")) {
    x <- get_cycle_matrix(x = x, n_cycles = n_cycles)
  }

  stopifnot("Only matrixes allowed!" = inherits(x, what = "matrix"))

  res <- get_na_df()

  # Aggregate x when it covers moren than one cycle (season).
  fx <-
    apply(
      X = x,
      MARGIN = 1,
      FUN = f
    )

  # Return if the time series is flat.
  if (length(unique(fx)) == 1) {
    return(res)
  }

  # Estimate the actual threshold.
  threshold <- sum(fx) * threshold_cons

  # The season's start value position (the peak) is the seed of the season.
  season_pos <- as.integer(which.max(fx))

  for (i in 1:(length(fx) - 1)) {
    # Check the the threshold has been reached.
    if (sum(fx[season_pos]) >= threshold) {
      break
    }

    # Find the next positions to check.
    next_pos <- get_prev_next(
      y = season_pos,
      total_len = length(fx)
    )

    # Chooses which pos to add to the season: left or right.
    if (fx[next_pos[1]] >= fx[next_pos[2]]) {
      season_pos <- c(next_pos[1], season_pos)
    } else {
      season_pos <- c(season_pos, next_pos[2])
    }
  }

  res["pos_from"] <- season_pos[1]
  res["val_from"] <- x[season_pos[1]]
  res["pos_to"] <- season_pos[length(season_pos)]
  res["val_to"] <- x[season_pos[length(season_pos)]]
  res["pos_min"] <- season_pos[which.min(x[season_pos])]
  res["val_min"] <- min(x[season_pos])
  res["pos_max"] <- season_pos[which.max(x[season_pos])]
  res["val_max"] <- max(x[season_pos])
  res["val_len"] <- length(season_pos)
  res["val_mean"] <- mean(x[season_pos])
  res["val_sd"] <- stats::sd(x[season_pos])

  # Estimate the coverage of the season.
  perc_season <- rep(x = 0.0, times = nrow(x))
  if (res["pos_from"][[1]] <= res["pos_to"][[1]]) {
    perc_season[floor(res["pos_from"][[1]]):ceiling(res["pos_to"][[1]])] <- 1
  } else if (res["pos_from"][[1]] > res["pos_to"][[1]]) {
    pos_season <- floor(res["pos_to"][[1]]):ceiling(res["pos_from"][[1]])
    pos_season <- setdiff(x = seq_len(nrow(x)), y = pos_season)
    perc_season[pos_season] <- 1
  }
  res[["coverage"]] <- sum(rowSums(x, na.rm = TRUE) * perc_season)

  return(res)
}


#' @rdname compute_season_peak_threshold
#'
#' @param n_runs_min,n_runs_max an integer(1). Minimum and maximum number of
#'   successful fitting attempts.
#' @param f a character(1). A function for aggregating data across cycles used
#' to ensure the highest values are centered. See [get_cycle_matrix].
#'
#' @export
#'
compute_season_double_sig <- function(x, n_cycles = 1, n_runs_min = 20,
                                      n_runs_max = 500, f) {
  stopifnot("Too few observations!" = length(x) > 1)
  stopifnot("I can't handle NAs!" = sum(is.na(x)) == 0)
  stopifnot("Invalid number of cycles!" = n_cycles > 0)
  stopifnot("Negative values not supported!" = all(x >= 0))
  stopifnot("Too few observations per cycle!" = length(x) / n_cycles > 6)

  # Ensure x is always a matrix.
  if (inherits(x, what = "numeric") || inherits(x, what = "integer")) {
    x <- get_cycle_matrix(x = x, n_cycles = n_cycles)
  }

  stopifnot("Only matrixes allowed!" = inherits(x, what = "matrix"))

  res <- get_na_df()

  # Return if the given time series is flat.
  if (length(unique(x)) == 1) {
    return(res)
  }

  # Ensure the minimum value in the time series is 0.
  v_min <- min(x)
  x <- x - v_min

  # Center around the peak value.
  xcentered_df <- center_peak(x_mt = x, f = f)

  # Prepare data for regression.
  # NOTE: sicegar always uses two columns: intensity and time.
  sicegar_df <- data.frame(
    intensity = unlist(xcentered_df[, 1:n_cycles]),
    time = rep(xcentered_df[["center_pos"]], times = n_cycles)
  )

  sic_norm_df <- sicegar::normalizeData(sicegar_df)

  # Do the double-sigmoidal fit
  model_fit <-
    sicegar::multipleFitFunction(
      dataInput = sic_norm_df,
      model = "doublesigmoidal",
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    )

  # Check that the model fits.
  if (!model_fit[["isThisaFit"]]) {
    return(res)
  }

  # Estimate additional parameters.
  m_par <- sicegar::parameterCalculation(model_fit)

  # Build a data frame with season parameters.
  res["val_from"] <- m_par[["midPoint1_y"]] + v_min
  res["val_to"] <- m_par[["midPoint2_y"]] + v_min
  res["val_max"] <- m_par[["reachMaximum_y"]] + v_min
  res["pos_from"] <-
    (m_par[["midPoint1_x"]] +
      un_center(m_par[["midPoint1_x"]], x_df = xcentered_df)) %% length(x)
  res["pos_to"] <-
    (m_par[["midPoint2_x"]] +
      un_center(m_par[["midPoint2_x"]], x_df = xcentered_df)) %% length(x)
  res["pos_max"] <-
    (m_par[["reachMaximum_x"]] +
      un_center(m_par[["reachMaximum_x"]], x_df = xcentered_df)) %% length(x)
  res["val_len"] <- ifelse(
    res[["pos_from"]] <= res[["pos_to"]],
    res[["pos_to"]] - res[["pos_from"]],
    (res[["pos_to"]] + length(x)) - res[["pos_from"]]
  )

  # Estimate the coverage of the season.
  perc_season <- rep(x = 0.0, times = nrow(x))
  if (res["pos_from"][[1]] < res["pos_to"][[1]]) {
    perc_season[floor(res["pos_from"][[1]]):ceiling(res["pos_to"][[1]])] <- 1
  } else if (res["pos_from"][[1]] > res["pos_to"][[1]]) {
    pos_season <- floor(res["pos_to"][[1]]):ceiling(res["pos_from"][[1]])
    pos_season <- setdiff(x = seq_len(nrow(x)), y = pos_season)
    perc_season[pos_season] <- 1
  }
  perc_season[floor(res["pos_from"][[1]])] <-
    1 - (res[["pos_from"]] - trunc(res[["pos_from"]]))
  perc_season[floor(res["pos_to"][[1]])] <-
    res[["pos_to"]] - trunc(res[["pos_to"]])
  res[["coverage"]] <- sum(rowSums((x + v_min), na.rm = TRUE) * perc_season)

  return(res)
}
