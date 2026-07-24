#---- configuracion ----

# Default values for sicegar::multipleFitFunction:
n_runs_min <- 20
n_runs_max <- 500
tol <- testthat::testthat_tolerance()
tol_pos <- tol

set.seed(666)



#---- Utilitary functions ----

#' Test column names in data frame
#'
#' @description
#' Test if the mandatory column names are present in the given data frame.
#'
#' @param x a data frame.
#'
test_base_colnames <- function(x) {
  expect_true(all(
    c(
      "pos_from",
      "val_from",
      "pos_to",
      "val_to",
      "pos_min",
      "val_min",
      "pos_max",
      "val_max",
      "val_len",
      "val_mean",
      "val_sd",
      "coverage"
    ) %in% colnames(x)
  ))
}

#' Test if data frame values are in range
#'
#' @description
#' Test that the given data frame values are in valid ranges.
#'
#' @param x a data frame.
#'
test_intervals <- function(x) {
  # NOTE: season are cyclical; so pos_from could be larger than pos_to
  if (!is.na(x[["pos_from"]])) {
    expect_true(all(
      x[["pos_from"]] >= 0,
      x[["pos_from"]] <= 12
    ))
  }
  if (!is.na(x[["pos_to"]])) {
    expect_true(all(
      x[["pos_to"]] >= 0,
      x[["pos_to"]] <= 12
    ))
  }
}

#' Get the season's cap
#'
#' @description
#' Get the season paramters as a data frame ready for plot.
#'
#' @param x a data frame resulting from a call to either
#' [compute_season_peak_threshold()] or [compute_season_double_sig()]
#' @param obs_x_cycle an integer(1). Number of observations in a cycle.
#'
#' @return a data frame with two columns: `pos`` and `ba`.
#'
get_season_cap <- function(x, obs_x_cycle) {
  res <-
    data.frame(
      pos = c(x[["pos_from"]], x[["pos_max"]], x[["pos_to"]]),
      ba = c(x[["val_from"]], x[["val_max"]], x[["val_to"]])
    )
  for (i in seq_len(nrow(res))) {
    if (i == 1) next()
    if (res[["pos"]][i] < res[["pos"]][i - 1]) {
      res[["pos"]][i] <- res[["pos"]][i] + (obs_x_cycle - 1)
    }
  }
  return(res)
}

#' Estimate double sigmoid regression coverage
#'
#' @description
#' Estimate the season coverage(e.g. number of fire spots) of the double
#' sigmoid estimation.
#'
#' @param x a numeric. Vector or matrix with quantities for each time step.
#' @param season_df a data frame. Estimation of the season returned by either
#' `compute_season_double_sig` or `compute_season_peak_threshold`.
#'
#' @return a numeric.
#'
estimate_coverage_ds <- function(x, season_df) {
  season_w <- rep(0, times = length(x))
  if (is.matrix(x)) {
    season_w <- rep(0, times = nrow(x))
  }
  pos_from <- season_df["pos_from"][[1]]
  pos_to <- season_df["pos_to"][[1]]
  if (season_df["pos_from"][[1]] <= season_df["pos_to"][[1]]) {
    season_w[floor(pos_from):ceiling(pos_to)] <- 1
  } else {
    season_vec <- ceiling(pos_from):floor(pos_to)
    season_vec <- setdiff(seq_along(x), season_vec)
    season_w[season_vec] <- 1
  }
  season_w[floor(pos_from)] <- 1 - (pos_from - floor(pos_from))
  season_w[floor(pos_to)] <- pos_to - floor(pos_to)
  cov <- sum(season_w * x)
  return(cov)
}

#' Estimate peak & threshold coverage
#'
#' @description
#' Estimate the season coverage(e.g. number of fire spots) of the peak &
#' threshold estimation.
#'
#' @param x a numeric. Vector or matrix with quantities for each time step.
#' @param season_df a data frame. Estimation of the season returned by either
#' `compute_season_double_sig` or `compute_season_peak_threshold`.
#'
#' @return a numeric.
#'
estimate_coverage_pt <- function(x, season_df) {
  if (season_df["pos_from"][[1]] <= season_df["pos_to"][[1]]) {
    cov <- sum(x[season_df[["pos_from"]]:season_df[["pos_to"]]])
  } else {
    cov <-
      sum(x[
        setdiff(
          seq_along(x),
          season_df[["pos_from"]]:season_df[["pos_to"]]
        )
      ])
  }
  return(cov)
}


#---- Tests ----

test_that("compute_season_peak_threshold works", {
  thres <- 0.6

  x <- 1:12 + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 5,
    season_df[["val_from"]] == 105,
    season_df[["pos_to"]] == 12,
    season_df[["val_to"]] == 112,
    season_df[["pos_min"]] == 5,
    season_df[["val_min"]] == 105,
    season_df[["pos_max"]] == 12,
    season_df[["val_max"]] == 112,
    season_df[["val_len"]] == 8,
    season_df[["val_mean"]] == 108.5,
    sum(x[season_df[["pos_from"]]:season_df[["pos_to"]]]) >=
      sum(x) * thres,
    season_df[["pos_from"]] <= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- 12:1 + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 1,
    season_df[["val_from"]] == 112,
    season_df[["pos_to"]] == 8,
    season_df[["val_to"]] == 105,
    season_df[["pos_min"]] == 8,
    season_df[["val_min"]] == 105,
    season_df[["pos_max"]] == 1,
    season_df[["val_max"]] == 112,
    season_df[["val_len"]] == 8,
    season_df[["val_mean"]] == 108.5,
    sum(x[season_df[["pos_from"]]:season_df[["pos_to"]]]) >=
      sum(x) * thres,
    season_df[["pos_from"]] <= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- c(1:6, 6:1) + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = 0.6)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 3,
    season_df[["val_from"]] == 103,
    season_df[["pos_to"]] == 10,
    season_df[["val_to"]] == 103,
    season_df[["pos_min"]] == 3,
    season_df[["val_min"]] == 103,
    season_df[["pos_max"]] == 6,
    season_df[["val_max"]] == 106,
    season_df[["val_len"]] == 8,
    season_df[["val_mean"]] == 104.5,
    sum(x[season_df[["pos_from"]]:season_df[["pos_to"]]]) >=
      sum(x) * thres,
    season_df[["pos_from"]] <= season_df[["pos_to"]]
  ))

  x <- c(6:1, 1:6) + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 9,
    season_df[["val_from"]] == 103,
    season_df[["pos_to"]] == 4,
    season_df[["val_to"]] == 103,
    season_df[["pos_min"]] == 9,
    season_df[["val_min"]] == 103,
    season_df[["pos_max"]] == 12,
    season_df[["val_max"]] == 106,
    season_df[["val_len"]] == 8,
    season_df[["val_mean"]] == 104.5,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- rep(0, 12)
  x[6] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 6,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]] == 6,
    season_df[["val_to"]] == 1,
    season_df[["pos_min"]] == 6,
    season_df[["val_min"]] == 1,
    season_df[["pos_max"]] == 6,
    season_df[["val_max"]] == 1,
    season_df[["val_len"]] == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- rep(0, 12)
  x[1] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 1,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]] == 1,
    season_df[["val_to"]] == 1,
    season_df[["pos_min"]] == 1,
    season_df[["val_min"]] == 1,
    season_df[["pos_max"]] == 1,
    season_df[["val_max"]] == 1,
    season_df[["val_len"]] == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- rep(0, 12)
  x[length(x)] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 12,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]] == 12,
    season_df[["val_to"]] == 1,
    season_df[["pos_min"]] == 12,
    season_df[["val_min"]] == 1,
    season_df[["pos_max"]] == 12,
    season_df[["val_max"]] == 1,
    season_df[["val_len"]] == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  x <- rep(0, 12)
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    is.na(season_df[["pos_from"]]),
    is.na(season_df[["val_from"]]),
    is.na(season_df[["pos_to"]]),
    is.na(season_df[["val_to"]]),
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["pos_max"]]),
    is.na(season_df[["val_max"]]),
    is.na(season_df[["val_len"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["coverage"]])
  ))

  x <- rep(666, 12)
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    is.na(season_df[["pos_from"]]),
    is.na(season_df[["val_from"]]),
    is.na(season_df[["pos_to"]]),
    is.na(season_df[["val_to"]]),
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["pos_max"]]),
    is.na(season_df[["val_max"]]),
    is.na(season_df[["val_len"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_mean"]])
  ))

  expect_error(
    compute_season_peak_threshold(1:12, threshold_cons = 1.6)
  )

  expect_error(
    compute_season_peak_threshold(1:12, threshold_cons = 0)
  )

  expect_error(
    compute_season_peak_threshold(rep(NA, 12), threshold_cons = thres)
  )

  x <- 1:12 + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = 1)
  test_base_colnames(season_df)
  test_intervals(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 1,
    season_df[["val_from"]] == 101,
    season_df[["pos_to"]] == 12,
    season_df[["val_to"]] == 112
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_pt(x, season_df)
  )

  # Test a flat time series.
  x <- rep(100, times = 24)
  expect_true(identical(
    compute_season_peak_threshold(x, threshold_cons = thres),
    get_na_df()
  ))

  x <- rep(100, times = 24)
  x[1] <- NA
  expect_error(
    compute_season_peak_threshold(x)
  )
})


test_that("compute_season_double_sigmoidal works, ex01", {
  set.seed(123)
  x <- c(1:6, 6:1) + 100
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )
  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 3.63780647028693,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 9.37891395319866,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_true(abs(season_df[["val_to"]] - season_df[["val_from"]]) < 1)

  expect_equal(
    object = season_df[["val_from"]],
    expected = 103.50944674133,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 103.509463656801,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 5.62662983648596,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 106.018876624157,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works, ex02", {
  set.seed(123)
  x <- c(6:1, 1:6) + 100
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 9.57599385519962,
    tolerance = tol_pos
  )
  expect_equal(
    object = season_df[["pos_to"]],
    expected = 3.36546435018666,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_true(abs(season_df[["val_to"]] - season_df[["val_from"]]) < 1)

  expect_equal(
    object = season_df[["val_from"]],
    expected = 103.494642962008,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 103.494687137205,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 11.9132042938429,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 105.989332894513,
    tolerance = tol
  )

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works, ex03", {
  set.seed(123)
  x <- 1:12 + 100
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 6.40663046546666,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 0.352684658116523,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 106.489134607005,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 107.738478781142,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 0.00641640278177569,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 111.978265380267,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works, ex04", {
  set.seed(123)
  x <- 12:1 + 100
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = stats::median
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 0.46730764166864,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 6.45361543975974,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 106.539190444763,
    tolerance = tol
  )
  expect_equal(
    object = season_df[["val_to"]],
    expected = 106.539477576522,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 0.597747677689764,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 112.078962294384,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works with flat time series 1", {
  x <- rep(100, times = 24)
  expect_true(identical(
    compute_season_double_sig(
      x = x,
      n_cycles = 1,
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    ),
    get_na_df()
  ))
})


test_that("compute_season_double_sigmoidal works with flat time series 2", {
  set.seed(123)
  x <- rep(0, 12)
  x[1] <- 1
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 0.772229608876197,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 1.35838853819359,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 0.520274315374213,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 0.520254951890993,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 0.98427869761494,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 1.04057779844879,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works with flat time series 3", {
  set.seed(124)
  x <- rep(1, 12)
  x[length(x)] <- 2
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    # expected = 11.7722296088762,
    expected = 11.6903557,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    # expected = 1.358388538193585,
    expected = 0.45351277,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    # expected = 0.520274315374213,
    expected = 1.51436639,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    # expected = 0.520254951890993,
    expected = 1.51436660,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    # expected = 11.9842786976149,
    expected = 11.97772050,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    # expected = 1.04057779844879,
    expected = 2.02872090,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))


  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sigmoidal works with flat time series 4", {
  x <- rep(100, times = 24)
  x[1] <- NA
  expect_error(
    compute_season_double_sig(
      x = x,
      x_cycles = 1,
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    )
  )
})


test_that("compute_season_double_sig works with real examples 1", {
  set.seed(123)
  x <- c(1, 0, 0, 0, 0, 0, 0, 11, 14, 50, 64, 2)
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = stats::median
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 9.71199584332247,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 11.2556997901152,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 37.5949919447913,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 37.6937549225931,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 10.712891438536,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 75.1899823830584,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sig works with real examples 2", {
  set.seed(123)
  x <- c(5, 4, 1, 2, 23, 43, 36, 16, 38, 12, 21, 3) # cell_id 454113
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = stats::median
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 4.95974504609739,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 9.29997512732917,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 21.2841037771478,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 21.2844518355417,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 5.40317372667019,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 41.5687073520289,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sig works with real examples 3", {
  set.seed(123)
  x <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0) # cell_id 58814
  season_df <- compute_season_double_sig(
    x = x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 4.92233160090436,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 6.15217798987165,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 0.654549888873069,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 0.654576152486286,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 5.05519003436538,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 1.30916366980806,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_double_sig works with real examples 4", {
  set.seed(123)
  x <- c(6, 8, 4, 7, 11, 19, 10, 9, 8, 12, 4, 7)
  season_df <- compute_season_double_sig(
    x,
    n_cycles = 1,
    n_runs_min = n_runs_min,
    n_runs_max = n_runs_max,
    f = "median"
  )

  test_base_colnames(season_df)
  test_intervals(season_df)

  expect_equal(
    object = season_df[["pos_from"]],
    expected = 5.3738582996005,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["pos_to"]],
    expected = 6.87649362130302,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_len"]],
    expected = season_df[["pos_to"]] - season_df[["pos_from"]],
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_from"]],
    expected = 13.8200139700672,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["val_to"]],
    expected = 15.8185285527704,
    tolerance = tol
  )

  expect_equal(
    object = season_df[["pos_max"]],
    expected = 6.62551024251672,
    tolerance = tol_pos
  )

  expect_equal(
    object = season_df[["val_max"]],
    expected = 23.6400531654872,
    tolerance = tol
  )

  expect_true(is.na(season_df[["pos_min"]]))
  expect_true(is.na(season_df[["val_min"]]))
  expect_true(is.na(season_df[["val_mean"]]))
  expect_true(is.na(season_df[["val_sd"]]))

  expect_equal(
    object = season_df[["coverage"]],
    expected = estimate_coverage_ds(x, season_df)
  )
})


test_that("compute_season_peak_threshold works with multi-cycle data", {
  n_cyc <- 4
  x <- sample.int(n = 100, size = 12 * n_cyc, replace = TRUE)
  x_sum <- rowSums(matrix(data = x, ncol = length(x) / 12, byrow = FALSE))

  x1_df <- compute_season_peak_threshold(
    x = x,
    n_cycles = n_cyc,
    threshold_cons = 0.6,
    f = "sum"
  )

  x2_df <- compute_season_peak_threshold(
    x = x_sum,
    threshold_cons = 0.6,
    n_cycles = 1,
    f = "mean"
  )

  test_base_colnames(x1_df)
  test_base_colnames(x2_df)
  test_intervals(x1_df)
  test_intervals(x2_df)

  expect_equal(x1_df[["pos_from"]], x2_df[["pos_from"]])
  expect_equal(x1_df[["pos_to"]], x2_df[["pos_to"]])
  expect_equal(x1_df[["val_len"]], x2_df[["val_len"]])
  expect_equal(x1_df[["coverage"]], x2_df[["coverage"]])
})


test_that("compute_season_peak_threshold returns the right number of points", {
  ba_file <- system.file("extdata", "test_data", "burnedarea_vector.rds",
    package = "seasonmetrics",
    mustWork = TRUE
  )
  ba_vec <- readRDS(ba_file)

  # Number of observations in each cycle.
  obs_x_cycle <- 12

  stopifnot("Incomplete cycles detected!" = length(ba_vec) %% obs_x_cycle == 0)

  res_pt <-
    compute_season_peak_threshold(
      x = ba_vec,
      n_cycles = length(ba_vec) / obs_x_cycle,
      threshold_cons = 0.6,
      f = "max"
    )

  test_base_colnames(res_pt)
  test_intervals(res_pt)

  res_pt_cap <- get_season_cap(res_pt, obs_x_cycle = obs_x_cycle)

  # NOTE: It's all right when peak & threshold returns 2 positions instead of 3
  expect_true(length(unique(res_pt_cap[["pos"]])) %in% 2:3)
})


test_that("compute_season_double_sig works with real multicycle data", {
  # NOTE: These time series has two anual peaks instead of one.
  ba_file <- system.file("extdata", "test_data", "burnedarea_vector.rds",
    package = "seasonmetrics",
    mustWork = TRUE
  )
  ba_vec <- readRDS(ba_file)

  # Number of observations in each cycle.
  obs_x_cycle <- 12

  stopifnot("Incomplete cycles detected!" = length(ba_vec) %% obs_x_cycle == 0)

  # ==== Use the actual package to do the regression ====

  res_ds <-
    compute_season_double_sig(
      x = ba_vec,
      n_cycles = length(ba_vec) / obs_x_cycle,
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max,
      f = "mean"
    )

  res_pt <-
    compute_season_peak_threshold(
      x = ba_vec,
      n_cycles = length(ba_vec) / obs_x_cycle,
      threshold_cons = 0.6,
      f = "mean"
    )

  test_base_colnames(res_ds)
  test_intervals(res_ds)
  test_base_colnames(res_pt)
  test_intervals(res_pt)

  res_ds_cap <- get_season_cap(res_ds, obs_x_cycle = obs_x_cycle)
  res_pt_cap <- get_season_cap(res_pt, obs_x_cycle = obs_x_cycle)

  # ==== Fit a double sigmoid function by hand and compare ====

  ba_df <-
    ba_vec |>
    matrix(
      nrow = obs_x_cycle,
      byrow = FALSE
    ) |>
    as.data.frame() |>
    (function(x) {
      colnames(x) <- sprintf("cycle%02d", seq(length(ba_vec) / obs_x_cycle))
      return(x)
    })() |>
    dplyr::mutate(
      pos = dplyr::row_number()
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("cycle"),
      names_to = "cycle",
      values_to = "ba"
    )

  # Fit a double sigmod function and compare.
  sicegar_df <- data.frame(
    intensity = ba_df[["ba"]],
    time = ba_df[["pos"]]
  )

  # Fit a double sigmoidal function.
  model_fit <-
    sicegar::multipleFitFunction(
      dataInput = sicegar_df,
      model = "doublesigmoidal",
      n_runs_min = n_runs_min,
      n_runs_max = n_runs_max
    )

  # Estimate the intensity using the fitted model.
  intensity_estimated <-
    sicegar::doublesigmoidalFitFormula(
      x = unique(sicegar_df[["time"]]),
      finalAsymptoteIntensityRatio = model_fit[["finalAsymptoteIntensityRatio_Estimate"]],
      maximum = model_fit[["maximum_Estimate"]],
      slope1 = model_fit[["slope1Param_Estimate"]],
      midPoint1Param = model_fit[["midPoint1Param_Estimate"]],
      slope2 = model_fit[["slope2Param_Estimate"]],
      midPointDistanceParam = model_fit[["midPointDistanceParam_Estimate"]]
    )

  # Create a data frame using the estimated intensities.
  ds_esti <- data.frame(
    intensity = intensity_estimated,
    time = seq_along(intensity_estimated)
  )

  # Plot to visually inspect the results of the fits.
  # NOTE: Here the test is to plot and compare the resulting season caps.
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = pos, y = ba, group = cycle, color = cycle),
      data = ba_df
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = pos, y = ba, group = cycle, color = cycle),
      data = ba_df
    ) +
    # NOTE: This triangular shape is an approximation to the results of double
    # sigmoid function derived from the regression results.
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = pos, y = ba),
      data = res_ds_cap,
      linewidth = 2,
      color = "red"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = pos, y = ba),
      data = res_ds_cap,
      size = 3,
      color = "red"
    ) +
    # NOTE: This triangular shape is an approximation to the peak & threshold
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = pos, y = ba),
      data = res_pt_cap,
      linewidth = 2,
      color = "green"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = pos, y = ba),
      data = res_pt_cap,
      size = 3,
      color = "green"
    ) +
    # NOTE: This is the results of fitting a double sigmoidal by hand with no
    # centering.
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = time, y = intensity),
      data = ds_esti,
      linewidth = 2,
      color = "blue"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = time, y = intensity),
      data = ds_esti,
      size = 3,
      color = "blue"
    )
})
