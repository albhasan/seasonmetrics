#' @title Compute the PRODES year of the given date.
#'
#' @name compute_prodes_year
#'
#' @description
#' This function computes the corresponding PRODES year of the given date
#' (PRODES is the brazilian deforestation monitoring system). The PRODES year
#' starts on August and it takes the year of its last July.
#' @param adate A date vector.
#' @param start_month A character. The month (number) of the first month of
#' a PRODES year.
#' @param start_day A character. The day (number) of the first day of a PRODES
#' year.
#' @return An integer.
#' @export
compute_prodes_year <- function(adate, start_month = "08", start_day = "01") {
    stopifnot("PRODES month length should be 1" = length(start_month) == 1)
    stopifnot("PRODES day length should be 1" = length(start_day) == 1)
    if (length(adate) > 1) {
        return(
            vapply(adate, FUN = compute_prodes_year,
                   FUN.VALUE = integer(1))
        )
    } else if (length(adate) == 1) {
        date_year <- lubridate::year(adate)
        prodes_start <- as.Date(paste(date_year, start_month, start_day,
                                      sep = "-"))
        if (adate >= prodes_start) {
            return(as.integer(date_year + 1))
        } else {
            return(as.integer(date_year))
        }
    } else {
        stop("Invalid length of PRODES date!")
    }
}
