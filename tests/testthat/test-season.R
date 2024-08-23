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

compare_num <- function(x, y, tol = testthat::testthat_tolerance()) {
    return(abs(x - y) <= tol)
}

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
    season_df <- compute_season_peak_threshold(x, 
                                               threshold_cons = 0.6)
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
    x[6] = 1
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
    x[1] = 1
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
    x[length(x)] = 1
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

test_that("compute_season_double_sigmoidal works", {

    set.seed(123)
    x <- c(1:6, 6:1) + 100
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]],   5.2129348606),
        compare_num(season_df[["val_from"]], 104.3082104837),
        compare_num(season_df[["pos_to"]],     7.3770465154),
        compare_num(season_df[["val_to"]],   105.2816070484),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],    5.3392368687),
        compare_num(season_df[["val_max"]],  107.6166545264),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    set.seed(123)
    x <- c(6:1, 1:6) + 100
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]],   9.6378064703),
        compare_num(season_df[["val_from"]], 103.5094467413),
        compare_num(season_df[["pos_to"]],     3.3789139532),
        compare_num(season_df[["val_to"]],   103.5094636568),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],    11.6266298365),
        compare_num(season_df[["val_max"]],   106.0188766242),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    set.seed(123)
    x <- 1:12 + 100
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]],   6.4066304655),
        compare_num(season_df[["val_from"]], 106.4891346070),
        compare_num(season_df[["pos_to"]],      0.3526846581),
        compare_num(season_df[["val_to"]],   107.7384787811),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],     0.0064164028),
        compare_num(season_df[["val_max"]],   111.9782653803),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    set.seed(123)
    x <- 12:1 + 100
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]],   12.4824006787),
        compare_num(season_df[["val_from"]], 106.5435030312),
        compare_num(season_df[["pos_to"]],     6.5181026413),
        compare_num(season_df[["val_to"]],   106.5433749131),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],    0.6127421648),
        compare_num(season_df[["val_max"]],   112.0867506874),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    # Test a flat time series.
    x <- rep(100, times = 24) 
    expect_true(identical(
        compute_season_double_sig(x),
        get_na_df()
    ))

    set.seed(123)
    x <- rep(0, 12)
    x[1] <- 1
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]], 0.7615065830),
        compare_num(season_df[["val_from"]], 0.5231298599),
        compare_num(season_df[["pos_to"]],   1.3415147750),
        compare_num(season_df[["val_to"]],   0.5231369166),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],  0.9923823449),
        compare_num(season_df[["val_max"]],  1.0462732964),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    set.seed(123)
    x <- rep(0, 12)
    x[length(x)] <- 1
    season_df <- compute_season_double_sig(x)
    test_base_colnames(season_df)
    #lapply(season_df, sprintf, fmt = "%.10f")
    expect_true(all(
        compare_num(season_df[["pos_from"]], 11.7722296089),
        compare_num(season_df[["val_from"]],  0.5202743154),
        compare_num(season_df[["pos_to"]],    0.3583885382),
        compare_num(season_df[["val_to"]],    0.5202549519),
        is.na(season_df[["pos_min"]]),
        is.na(season_df[["val_min"]]),
        compare_num(season_df[["pos_max"]],  11.9842786976),
        compare_num(season_df[["val_max"]],   1.0405777984),
        is.na(season_df[["val_len"]]),
        is.na(season_df[["val_mean"]]),
        is.na(season_df[["val_sd"]])
    ))

    x <- rep(100, times = 24) 
    x[1] <- NA
    expect_error(
        compute_season_double_sig(x)
    )

})

