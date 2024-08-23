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
#' @return a data frame with season's metrics.
#'
#' @export
#'
compute_season_peak_threshold <- function(x, threshold_cons) {

    stopifnot("Invalid trehshold!" = 
        all(0 < threshold_cons, threshold_cons <= 1))

    stopifnot("Can't handle NAs!" = sum(is.na(x)) == 0)

    # Test for a flat season. 
    if (length(unique(x)) == 1)
        return(get_na_df())

    # Estimate the threshold.
    threshold <- sum(x) * threshold_cons

    # The sason's start value position (the peak) is the seed of the season.
    season_pos <- as.integer(which.max(x))

    for (i in 1:(length(x) - 1)) {

        # Test the threshold.
        if (sum(x[season_pos]) >= threshold)
            break

        # Get new observations for testing.
        next_pos <- get_prev_next(
            y = season_pos,
            total_len = length(x)
        )

        if (x[next_pos[1]] >= x[next_pos[2]]) {
            season_pos <- c(next_pos[1], season_pos)
        }else{
            season_pos <- c(season_pos, next_pos[2])
        }

    }

    season_df <- data.frame(
        pos_from = season_pos[1],
        val_from = x[season_pos[1]],
        pos_to   = season_pos[length(season_pos)],
        val_to = x[season_pos[length(season_pos)]],
        pos_min = season_pos[which.min(x[season_pos])],
        val_min = min(x[season_pos]),
        pos_max = season_pos[which.max(x[season_pos])],
        val_max = max(x[season_pos]),
        val_len = length(season_pos),
        val_mean = mean(x[season_pos]),
        val_sd = stats::sd(x[season_pos])
    )

    return(season_df)

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
#' @export
#'
compute_season_double_sig <- function(x) {

    stopifnot("Can't handle NAs!" = sum(is.na(x)) == 0)

    # Test for a flat season. 
    if (length(unique(x)) == 1)
        return(get_na_df())

    # Translate data.
    x_min <- min(x)
    x <- x - x_min

    # Center around the peak.
    x_df <- center_peak(x)
    x_df["center_trans"] <- x_df[["pos"]] - x_df[["center_pos"]]

    # Prepare data for regression.
    sicegar_df <- data.frame(
        intensity = x_df[["x"]],
        time      = seq(nrow(x_df))
    )
    sic_norm_df <- sicegar::normalizeData(sicegar_df)

    # Do the double-sigmoidal fit
    model_fit <- sicegar::multipleFitFunction(
        dataInput = sic_norm_df,
        model = "doublesigmoidal"
    )

    # Check that the model fits.
    if (!model_fit[["isThisaFit"]]) {
        #TODO: Return NAs.
    }

    # Estimate extra parameters.
    m_par <- sicegar::parameterCalculation(model_fit)

    # Build a data frame with season parameters.
    season_df <- data.frame(
        pos_from = m_par[["midPoint1_x"]] + 
            un_center(m_par[["midPoint1_x"]], x_df = x_df),
        val_from = m_par[["midPoint1_y"]] + x_min,
        pos_to   = (m_par[["midPoint2_x"]] +
            un_center(m_par[["midPoint2_x"]], x_df = x_df)) %% length(x),
        val_to   = m_par[["midPoint2_y"]] + x_min,
        pos_min  = NA,
        val_min  = NA,
        pos_max  = (m_par[["reachMaximum_x"]] + 
            un_center(m_par[["reachMaximum_x"]], x_df = x_df)) %% length(x),
        val_max  = m_par[["reachMaximum_y"]] + x_min,
        val_len  = NA,
        val_mean = NA,
        val_sd   = NA
    )

    return(season_df)

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
    return(x_df[["center_trans"]] [x_df[["center_pos"]] == cen_pos])
}



#' Center around the peak value
#'
#' @description
#' Center the given vector around its peak. In this way, the peak would be at
#' the center position in the vector.
#'
#' @param x a numeric. A vector of cyclic observations.
#'
#' @return a data frame with 3 columns: x, the original position of each
#' observation (pos), and the centered position (center_pos). The x column is
#' reordered according to center_pos.
#'
center_peak <- function(x) {

    peak_pos <- as.integer(which.max(x))
    len_x <- length(x)

    data_df <- data.frame(
        x = x,
        pos = 1:length(x)
    )
    data_df["center_pos"] <- data_df[["pos"]] - floor(len_x/2)
    data_df["center_pos"] <- ifelse(data_df[["center_pos"]] < 1,
                                    data_df[["center_pos"]] + len_x,
                                    data_df[["center_pos"]])
    data_df <- data_df[order(data_df[["center_pos"]]), ]
    return(data_df)

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

