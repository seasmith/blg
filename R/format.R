#' Control how percentages are printed.
#'
#' Uses `sprintf()` to truncate decimal and print as a percent.
#'
#' @param x A number (percent) to be formatted.
#' @param i Number of figures to the right
#'   of the decimal to show (default = 1)
#' @export

blg_frmt_percent <- function(x, i = 1) {
  sprintf(sprintf("%s%s%s", "%.", i, "f%%"), x * 100)
}

#' Wrap long strings with a line break literal
#'
#' Wraps `stringi::stri_wrap()` with a literal line break 'prefix' and a blank
#' 'initial'. Ment for use with 'ggplot2'.
#'
#' @param x A character vector of one or more strings.
#' @param length A number specifying how many characters to include in each line
#'   before inserting the line break literal.
#' @export

blg_frmt_wrap <- function(x, length) {
  paste0(stringi::stri_wrap(x, length, prefix = "\n", initial = ""), collapse = "")
}

#' Capitalize the first word in a string, not the
#' individual words.
#'
#' @param string A single character string.
#' @export

blg_frmt_cap <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}