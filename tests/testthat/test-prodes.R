test_that("compute_prodes_year works", {
    test_dates <- c("2008-08-01", "2008-08-02", "2008-12-31", "2008-01-01",
                    "2008-07-31")
    exp_years <- c(2009, 2009, 2009, 2008, 2008)
    expect_equal(
        compute_prodes_year(as.Date(test_dates)),
        exp_years
    )
})
