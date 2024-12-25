
# Default values for sicegar::multipleFitFunction:
n_runs_min <- 20
n_runs_max <- 500
tol <- testthat::testthat_tolerance()
tol_pos <- tol

set.seed(666)

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
      "val_sd"
    ) %in% colnames(x)
  ))
}

compare_num <- function(x, y, tol) {
  return(abs(x - y) <= tol)
}

test_that("displace_vec works", {

  x <- 1:7
  expect_equal(displace_vec(x, 0), expected = x)
  expect_equal(displace_vec(x, length(x)), expected = x)
  expect_error(displace_vec(x, length(x) + 1))
  expect_error(displace_vec(x, (length(x) + 1)) * -1)
  for (i in sort(c(x * -1, x))) {
    res <- displace_vec(x, i)
    expect_equal(length(res), expected = length(x))
    expect_true(all(x %in% res))
    expect_true(length(unique(res - x)) %in% 1:2)
  }
  for (i in seq_along(x)) {
    expect_equal(displace_vec(displace_vec(x, i), -i), expected = x)
  }

  x <- 7:1
  expect_equal(displace_vec(x, 0), expected = x)
  expect_equal(displace_vec(x, length(x)), expected = x)
  expect_error(displace_vec(x, length(x) + 1))
  expect_error(displace_vec(x, (length(x) + 1)) * -1)
  for (i in sort(c(x * -1, x))) {
    res <- displace_vec(x, i)
    expect_equal(length(res), expected = length(x))
    expect_true(all(x %in% res))
    expect_true(length(unique(res - x)) %in% 1:2)
  }
  for (i in seq_along(x)) {
    expect_equal(displace_vec(displace_vec(x, i), -i), expected = x)
  }

})

test_that("center_peak works", {

  x <- 1:12
  res <- center_peak(x)
  expect_equal(colnames(res), expected = c("x", "pos", "center_pos"))
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = 3)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["x"]] == res[["pos"]]))
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
                    c(floor(length(x) / 2) - 1,
                      floor(length(x) / 2),
                      floor(length(x) / 2) + 1)))

  x <- 1:12 + 100
  res <- center_peak(x)
  expect_equal(colnames(res), expected = c("x", "pos", "center_pos"))
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = 3)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
                    c(floor(length(x) / 2) - 1,
                      floor(length(x) / 2),
                      floor(length(x) / 2) + 1)))

  x <- 12:1
  res <- center_peak(x)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
                    c(floor(length(x) / 2) - 1,
                      floor(length(x) / 2),
                      floor(length(x) / 2) + 1)))

  x <- 1:13
  res <- center_peak(x)
  expect_equal(colnames(res), expected = c("x", "pos", "center_pos"))
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = 3)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["x"]] == res[["pos"]]))
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
                    c(floor(length(x) / 2) - 1,
                      floor(length(x) / 2),
                      floor(length(x) / 2) + 1)))

})

test_that("compute_season_peak_threshold works", {

  thres <- 0.6

  x <- 1:12 + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
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

  x <- 12:1 + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
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

  x <- c(1:6, 6:1) + 100
  season_df <- compute_season_peak_threshold(x, threshold_cons = 0.6)
  test_base_colnames(season_df)
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

  x <- rep(0, 12)
  x[6] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 6,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]]   == 6,
    season_df[["val_to"]]   == 1,
    season_df[["pos_min"]]  == 6,
    season_df[["val_min"]]  == 1,
    season_df[["pos_max"]]  == 6,
    season_df[["val_max"]]  == 1,
    season_df[["val_len"]]  == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  x <- rep(0, 12)
  x[1] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 1,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]]   == 1,
    season_df[["val_to"]]   == 1,
    season_df[["pos_min"]]  == 1,
    season_df[["val_min"]]  == 1,
    season_df[["pos_max"]]  == 1,
    season_df[["val_max"]]  == 1,
    season_df[["val_len"]]  == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  x <- rep(0, 12)
  x[length(x)] <- 1
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
  expect_true(all(
    season_df[["pos_from"]] == 12,
    season_df[["val_from"]] == 1,
    season_df[["pos_to"]]   == 12,
    season_df[["val_to"]]   == 1,
    season_df[["pos_min"]]  == 12,
    season_df[["val_min"]]  == 1,
    season_df[["pos_max"]]  == 12,
    season_df[["val_max"]]  == 1,
    season_df[["val_len"]]  == 1,
    season_df[["val_mean"]] == 1,
    season_df[["pos_from"]] >= season_df[["pos_to"]]
  ))

  x <- rep(0, 12)
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
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
    is.na(season_df[["val_mean"]])
  ))

  x <- rep(666, 12)
  season_df <- compute_season_peak_threshold(x, threshold_cons = thres)
  test_base_colnames(season_df)
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
  expect_true(all(
    season_df[["pos_from"]] == 1,
    season_df[["val_from"]] == 101,
    season_df[["pos_to"]] == 12,
    season_df[["val_to"]] == 112
  ))

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
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 3.63780647028693, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   9.37891395319866, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]], tol = tol)
  ))

  expect_true(all(
    abs(season_df[["val_to"]] - season_df[["val_from"]]) < 1,
    compare_num(season_df[["val_from"]], 103.50944674133,  tol = tol),
    compare_num(season_df[["val_to"]],   103.509463656801, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 5.62662983648596, tol = tol_pos),
    compare_num(season_df[["val_max"]], 106.018876624157, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works, ex02", {

  set.seed(123)
  x <- c(6:1, 1:6) + 100
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 9.57599385519962, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   3.36546435018666, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    abs(season_df[["val_to"]] - season_df[["val_from"]]) < 1,
    compare_num(season_df[["val_from"]], 103.494642962008, tol = tol),
    compare_num(season_df[["val_to"]],   103.494687137205, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 11.9132042938429, tol = tol_pos),
    compare_num(season_df[["val_max"]], 105.989332894513, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works, ex03", {

  set.seed(123)
  x <- 1:12 + 100
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)

  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 6.40663046546666, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   0.352684658116523, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 106.489134607005, tol = tol),
    compare_num(season_df[["val_to"]],   107.738478781142, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 0.00641640278177569, tol = tol_pos),
    compare_num(season_df[["val_max"]], 111.978265380267, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works, ex04", {

  set.seed(123)
  x <- 12:1 + 100
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 0.46730764166864, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   6.45361543975974, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 106.539190444763, tol = tol),
    compare_num(season_df[["val_to"]],   106.539477576522, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 0.597747677689764, tol = tol_pos),
    compare_num(season_df[["val_max"]], 112.078962294384, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works with flat time series 1", {

  x <- rep(100, times = 24)
  expect_true(identical(
    compute_season_double_sig(x, n_runs_min = n_runs_min,
                              n_runs_max = n_runs_max),
    get_na_df()
  ))

})



test_that("compute_season_double_sigmoidal works with flat time series 2", {

  set.seed(123)
  x <- rep(0, 12)
  x[1] <- 1
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 0.772229608876197, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   1.35838853819359, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 0.520274315374213, tol = tol),
    compare_num(season_df[["val_to"]],   0.520254951890993, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 0.98427869761494, tol = tol_pos),
    compare_num(season_df[["val_max"]], 1.04057779844879, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works with flat time series 3", {

  set.seed(123)
  x <- rep(0, 12)
  x[length(x)] <- 1
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                         n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 11.7722296088762, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   0.358388538193585, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                (season_df[["pos_to"]] + length(x)) - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 0.520274315374213, tol = tol),
    compare_num(season_df[["val_to"]],   0.520254951890993, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 11.9842786976149, tol = tol_pos),
    compare_num(season_df[["val_max"]], 1.04057779844879 , tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sigmoidal works with flat time series 4", {

  x <- rep(100, times = 24)
  x[1] <- NA
  expect_error(
      compute_season_double_sig(x, n_runs_min = n_runs_min,
                                n_runs_max = n_runs_max)
  )

})



test_that("compute_season_double_sig works with real examples 1", {

  set.seed(123)
  x <- c(1, 0, 0, 0, 0, 0, 0, 11, 14, 50, 64, 2)
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                            n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 9.71199584332247, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   11.2556997901152, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 37.5949919447913, tol = tol),
    compare_num(season_df[["val_to"]],   37.6937549225931, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 10.712891438536, tol = tol_pos),
    compare_num(season_df[["val_max"]], 75.1899823830584, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sig works with real examples 2", {

  set.seed(123)
  x <- c( 5, 4, 1, 2, 23, 43, 36, 16, 38, 12, 21, 3) # cell_id 454113
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                            n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 4.95974504609739, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   9.29997512732917, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 21.2841037771478, tol = tol),
    compare_num(season_df[["val_to"]],   21.2844518355417, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 5.40317372667019, tol = tol_pos),
    compare_num(season_df[["val_max"]], 41.5687073520289, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sig works with real examples 3", {

  set.seed(123)
  x <- c( 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0) # cell_id 58814
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                            n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 4.92233160090436, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   6.15217798987165, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 0.654549888873069, tol = tol),
    compare_num(season_df[["val_to"]],   0.654576152486286, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 5.05519003436538, tol = tol_pos),
    compare_num(season_df[["val_max"]], 1.30916366980806, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})



test_that("compute_season_double_sig works with real examples 4", {

  set.seed(123)
  x <- c(6, 8, 4, 7, 11, 19, 10, 9, 8, 12, 4, 7)
  season_df <- compute_season_double_sig(x, n_runs_min = n_runs_min,
                                            n_runs_max = n_runs_max)
  test_base_colnames(season_df)

  expect_true(all(
    compare_num(season_df[["pos_from"]], 5.3738582996005, tol = tol_pos),
    compare_num(season_df[["pos_to"]],   6.87649362130302, tol = tol_pos),
    compare_num(season_df[["val_len"]],
                season_df[["pos_to"]] - season_df[["pos_from"]],
                tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["val_from"]], 13.8200139700672, tol = tol),
    compare_num(season_df[["val_to"]],   15.8185285527704, tol = tol)
  ))

  expect_true(all(
    compare_num(season_df[["pos_max"]], 6.62551024251672, tol = tol_pos),
    compare_num(season_df[["val_max"]], 23.6400531654872, tol = tol)
  ))

  expect_true(all(
    is.na(season_df[["pos_min"]]),
    is.na(season_df[["val_min"]]),
    is.na(season_df[["val_mean"]]),
    is.na(season_df[["val_sd"]])
  ))

})
