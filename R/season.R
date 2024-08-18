#' Compute the season
#'
#' @description
#' Given a vector representing a cycle of observations (e.g. 12 observations),
#' estimate the peak season. For example, given a year of monthly observations,
#' the peak season is the minimum subset of consecutive values around the peak
#' value that add to the threshold of the season total.
#'
#' @param x a numeric. The values observed during a cycle.
#' @param threshold_cons a numeric(1) between 0 and 1. The percentage of the
#'   total a season must reach.
#'
#' @return an integer. The positions of the observations in x corresponding to
#'  the season.
#'
#' @export
#'
compute_season <- function(x, threshold_cons) {

    stopifnot("Invalid trehshold!" = 
        all(0 <= threshold_cons, threshold_cons <= 1))

    # Test for no peak value.
    if (sum(x) == 0)
        return(0L)

    # Estimate the threshold.
    threshold <- sum(x) * threshold_cons

    # The sason's start value position is the seed of the season.
    season_pos <- as.integer(which.max(x))

    # Test for an one-observation season.
    if (x[season_pos] >= threshold)
        return(as.integer(season_pos))

    for (i in 1:(length(x) - 1)) {
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

        # Test the threshold.
        if (sum(x[season_pos]) >= threshold)
            return(as.integer(season_pos))
    }
    return(NA_integer_)
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

