test_that("monthdec2date & date2monthdec works", {
  # Parameters.
  tolerance <- 1 / (30 * 24 * 60 * 60) # one second.
  n_size <- 20 # Number of random dates to test.
  # Create years & months for testing.
  test_months <- c(
    1:12,
    exp(1), exp(2),
    pi, pi^2,
    sample(
      x = 1:12,
      size = n_size,
      replace = TRUE
    ) + runif(n = n_size)
  )
  test_years <- sample(
    x = 1970:2030,
    size = length(test_months),
    replace = TRUE
  )
  # Test known problematic dates.
  expect_true(!is.na(monthdec2date(2003, 1.004611)))
  expect_true(!is.na(monthdec2date(2021, 4.002715)))
  expect_error(monthdec2date(2023, 0.1))
  expect_error(monthdec2date(2023, 13))
  # Cast decimal month to dates.
  mdecs <-
    purrr::map2(
      .x = test_years,
      .y = test_months,
      .f = monthdec2date
    )
  # Cast dates back to decimal months.
  dmonths <-
    purrr::map(
      .x = mdecs,
      .f = date2monthdec
    )
  # Compare the original decimal dates to the results.
  for (i in seq(mdecs)) {
    expect_true(
      abs(dmonths[[i]][["year"]] - test_years[i]) < tolerance
    )
    expect_true(
      abs(dmonths[[i]][["month"]] - test_months[i]) < tolerance
    )
  }
})
