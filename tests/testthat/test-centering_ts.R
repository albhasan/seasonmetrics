center_peak_cnames <- c("x", "pos", "center_pos", "center_trans")
center_peak_ncol <- length(center_peak_cnames)

test_that("center_peak works", {
  x <- 1:12
  res <- center_peak(get_cycle_matrix(x = x, n_cycles = 1), f = "mean")
  res <- center_peak(get_cycle_matrix(x = x, n_cycles = 1), f = mean)
  res <- center_peak(get_cycle_matrix(x = x, n_cycles = 1), f = median)
  res <- center_peak(get_cycle_matrix(x = x, n_cycles = 1), f = stats::median)
  expect_equal(colnames(res), expected = center_peak_cnames)
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = center_peak_ncol)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["x"]] == res[["pos"]]))
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
    c(
      floor(length(x) / 2) - 1,
      floor(length(x) / 2),
      floor(length(x) / 2) + 1
    )))

  x <- 1:12 + 100
  res <- center_peak(get_cycle_matrix(x, n_cycles = 1), f = stats::median)
  expect_equal(colnames(res), expected = center_peak_cnames)
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = center_peak_ncol)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
    c(
      floor(length(x) / 2) - 1,
      floor(length(x) / 2),
      floor(length(x) / 2) + 1
    )))

  x <- 12:1
  res <- center_peak(get_cycle_matrix(x, n_cycles = 1), f = stats::median)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
    c(
      floor(length(x) / 2) - 1,
      floor(length(x) / 2),
      floor(length(x) / 2) + 1
    )))

  x <- 1:13
  res <- center_peak(get_cycle_matrix(x, n_cycles = 1), f = "median")
  expect_equal(colnames(res), expected = center_peak_cnames)
  expect_equal(nrow(res), expected = length(x))
  expect_equal(ncol(res), expected = center_peak_ncol)
  expect_true(trunc(which.max(res[["x"]]) - (length(x) / 2)) %in% -1:1)
  expect_true(all(res[["x"]] == res[["pos"]]))
  expect_true(all(res[["pos"]] %in% res[["center_pos"]]))
  expect_true(all(abs(res[["pos"]] - res[["center_pos"]]) %in%
    c(
      floor(length(x) / 2) - 1,
      floor(length(x) / 2),
      floor(length(x) / 2) + 1
    )))
})

test_that("center_peak works with multi-season data", {
  obs_x_cycle <- 12
  n_cycles <- 3
  data_mt <-
    t(sapply(
      X = seq(obs_x_cycle),
      FUN = function(x) {
        rnorm(n = n_cycles, mean = x^2, sd = 1)
      }
    ))
  center_mt <- center_peak(
    x = data_mt,
    f = "mean"
  )

  expect_true(all(abs(center_mt[["center_trans"]]) == obs_x_cycle / 2))
  expect_equal(object = sum(center_mt[["center_trans"]]), expected = 0)
  expect_true(center_mt[obs_x_cycle / 2, "pos"] == obs_x_cycle)
})

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

test_that("un_center works", {
  x <- 1:12
  x <- get_cycle_matrix(x, n_cycles = 1)
  x_df <- center_peak(x, f = "median")
  expect_equal(
    object = un_center(pos = x_df[["pos"]], x_df = x_df),
    expected = 6
  )
})

test_that("get_cycle_matrix works", {
  x <- rnorm(14)
  expect_error(get_cycle_matrix(numeric(0), n_cycles = 1))
  expect_error(get_cycle_matrix(x, 0))

  res <- get_cycle_matrix(x, n_cycles = 1)
  expect_equal(
    object = dim(res),
    expected = c(14, 1)
  )
  expect_true(inherits(res, what = "matrix"))
})

test_that("un_center works with multi-season data", {
  obs_x_cycle <- 12
  n_cycles <- 3
  data_mt <-
    t(sapply(
      X = seq(obs_x_cycle),
      FUN = function(x) {
        rnorm(n = n_cycles, mean = x^2, sd = 1)
      }
    ))
  center_mt <- center_peak(
    x = data_mt,
    f = "mean"
  )

  res <-
    vapply(
      X = center_mt[["pos"]],
      FUN = un_center,
      FUN.VALUE = integer(1),
      x_df = center_mt
    )
  expect_equal(object = sum(res), expected = 0)
  expect_true(all(abs(res) == obs_x_cycle / 2))
})
