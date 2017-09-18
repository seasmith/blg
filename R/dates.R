#' Find the number of days in a date's year.
#'
#' Enter a date and get the number of days in that date's year.
#'
#' @param date A date object.
#' @export

blg_days_in_year <- function(date) {
  c(366, 365)[c(lpyr <- lubridate::leap_year(date), !lpyr)]
}