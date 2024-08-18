test_that("compute_season works", {

    expect_equal(
        compute_season(1:12, threshold_cons = 0.6),
        expected = 8:12
    )
    expect_equal(
        compute_season(12:1, threshold_cons = 0.6),
        expected = 1:5
    )
    expect_equal(
        compute_season(c(1:6, 6:1), threshold_cons = 0.6),
        expected = 4:8
    )
    expect_equal(
        compute_season(c(6:1, 1:6), threshold_cons = 0.6),
        expected = c(10:12, 1:2)
    )

})
