#' Convert from decimal month to date
#'
#' @description
#' Given a decimal month, return the equivalent date.
#'
#' @param year an integer. A number representing a year.
#' @param month_dec a numeric. A number (1 <= x < 13) representing a month.
#' @param time_zone a character(1). The time zone of the resulting date.
#'
#' @return a POSIXct or POSIXt object. The date corresponding to the input data.
#'
#' @export
#'
monthdec2date <- function(year, month_dec, time_zone = "UTC") {
  if (any(is.na(year), is.na(month_dec))) {
    return(NA)
  }
  stopifnot("Invalid month!" = month_dec >= 1 && month_dec < 13)
  # Get the date to the first day of the month.
  base_date <-
    lubridate::ymd_hms(
      paste(floor(year), floor(month_dec), "01 00:00:00", sep = "-"),
      tz = time_zone
    )
  # Get the number of seconds in the month.
  top_date <-
    lubridate::ymd_hms(
      paste(
        floor(year), "-",
        floor(month_dec), "-",
        as.vector(lubridate::days_in_month(x = base_date)),
        " 23:59:59.99",
        sep = ""
      ),
      tz = time_zone
    )
  month_secs <-
    as.vector(difftime(
      time1 = top_date,
      time2 = base_date,
      units = "secs"
    ))
  # Convert the remaining fraction of month to seconds.
  residual_sec <- month_secs * as.double(month_dec - floor(month_dec))
  # Add the fraction month to the base date (in seconds).
  new_date <- as.vector(base_date) + residual_sec
  # Cast to date.
  return(lubridate::as_datetime(new_date, tz = time_zone))
}


#' Convert a date into a decimal month
#'
#' @description
#' Given a date, this function returns its equivalent year and month fraction.
#'
#' @param adate a POSIXlt date
#'
#' @return a list with 2 elements: A year and a decimal month.
#'
#' @export
#'
date2monthdec <- function(adate) {
  base_date <-
    lubridate::ymd_hms(
      paste(
        lubridate::year(adate), "-",
        lubridate::month(adate), "-",
        "01 00:00:00",
        sep = ""
      ),
      tz = lubridate::tz(adate)
    )
  # Get the number of seconds in the month.
  top_date <-
    lubridate::ymd_hms(
      paste(
        lubridate::year(adate), "-",
        lubridate::month(adate), "-",
        as.vector(lubridate::days_in_month(x = base_date)),
        " 23:59:59.99",
        sep = ""
      ),
      tz = lubridate::tz(adate)
    )
  month_secs <-
    as.vector(difftime(
      time1 = top_date,
      time2 = base_date,
      units = "secs"
    ))
  fraction_sec <-
    as.vector(difftime(
      time1 = adate,
      time2 = base_date,
      units = "secs"
    )) / month_secs

  return(list(
    year = lubridate::year(adate),
    month = lubridate::month(adate) + fraction_sec
  ))
}
