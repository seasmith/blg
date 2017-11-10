#' Control how percentages are printed.
#'
#' Uses `sprintf()` to truncate decimal and print as a percent.
#'
#' @param i Number of figures to the right
#'   of the decimal to show (default = 1)
#' @export

blg_frmt_percent <- function(i = 1) {

  fun <- function(labels) {
    sprintf(sprintf("%s%s%s", "%.", i, "f%%"), labels * 100)
  }

  structure(fun, class = "scale_labeller")

}

#' Wrap long strings with a line break literal
#'
#' Wraps `stringi::stri_wrap()` with a literal line break 'prefix' and a blank
#' 'initial'. Ment for use with 'ggplot2'.
#'
#' @param length A number specifying how many characters to include in each line
#'   before inserting the line break literal.
#' @export

blg_frmt_wrap <- function(length = 18) {

  fun <- function(labels) {
    paste0(stringi::stri_wrap(labels, length, prefix = "\n", initial = ""), collapse = "")
  }

  structure(fun, class = "scale_labeller")

}

#' Capitalize the first word in a string, not the
#' individual words.
#'
#' Capitalize the first word in a string, not the
#' individual words.
#'
#' @export

blg_frmt_capitalize <- function() {

  fun <- function(labels) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
  }

  structure(fun, class = "scale_labeller")

}

#' Add a prefixed line break literal to every other string in a character
#' vector.
#'
#' Prefix every other string in a character vector with a line break. Number of
#' breaks determined by `n`. Useful for staggering overlapping labels on an
#' x-axis.
#'
#' @param n Number of line breaks to prefix to each string.
#' @export

blg_frmt_stagger <- function(n = 1) {

  fun <- function(labels) {
    paste0(c(paste0(rep("\n", n), collapse = ""), ""), labels)
  }

  structure(fun, class = "scale_labeller")
}

#' Delete every other label in a character vector.
#'
#' Assign blank `""` (erase) values to every other string in a character vector.
#' Convenient for eliminating every other label in a plot without removing the
#' grid lines.
#'
#' @param eliminate_which String indicating whether to erase the even (default)
#'   or odd labels.
#' @export

blg_frmt_erase <- function(eliminate_which = c("even", "odd")) {

  fun <- function(labels) {

    if (eliminate_which[[1]] == "even") {
      labels[seq.int(from = 0, to = length(labels), by = 2)] <- ""
    } else if (eliminate_which[[1]] == "odd") {
      labels[seq.int(from = 1, to = length(labels), by = 2)] <- ""
    } else {
      warning('Must specify either "even" or "odd". Default "even" chosen.')
      labels[seq.int(from = 0, to = length(labels), by = 2)] <- ""
    }

    return(labels)

  }

  structure(fun, class = "scale_labeller")

}

#' Wrap and stagger labels.
#'
#' Applies a wrapping and staggering line break.
#'
#' @param length A number specifying how many characters to include in each line
#'   before inserting the line break literal.
#' @param n Number of line breaks to prefix to each string.
#'
#' @export
blg_frmt_wrap_stagger <- function(length = 18, n = 1) {

  fun <- function(labels) {
    new_labels <- paste0(stringi::stri_wrap(labels, length, prefix = "\n", initial = ""), collapse = "")
    new_labels <- paste0(c(paste0(rep("\n", n), collapse = ""), ""), new_labels)
    return(new_labels)
  }

  structure(fun, class = "scale_labeller")

}