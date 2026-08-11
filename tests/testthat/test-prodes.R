test_that("compute_prodes_year works 00", {
  test_dates <- c("2008-07-31", "2008-01-01")
  expect_equal(
    object = compute_prodes_year(
      adate = test_dates,
      start_month = "01",
      start_day = "01"
    ),
    expected = c(
      compute_prodes_year(
        adate = test_dates[1],
        start_month = "01",
        start_day = "01"
      ),
      compute_prodes_year(
        adate = test_dates[2],
        start_month = "01",
        start_day = "01"
      )
    )
  )
})

test_that("compute_prodes_year works", {
  test_dates <- c(
    "2008-08-01", "2008-08-02", "2008-12-31", "2008-01-01", "2008-07-31"
  )

  exp_years <- as.integer(c(2009, 2009, 2009, 2008, 2008))
  expect_equal(
    compute_prodes_year(
      adate = as.Date(test_dates),
      start_month = "08",
      start_day = "01"
    ),
    exp_years
  )

  exp_years <- as.integer(c(2009, 2009, 2009, 2008, 2009))
  expect_equal(
    compute_prodes_year(
      adate = as.Date(test_dates),
      start_month = "02",
      start_day = "01"
    ),
    exp_years
  )

  exp_years <- as.integer(c(2009, 2009, 2009, 2009, 2009))
  expect_equal(
    object = compute_prodes_year(
      adate = as.Date(test_dates),
      start_month = "01",
      start_day = "01"
    ),
    expected = exp_years
  )
})
