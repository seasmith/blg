#' Collect only the 'value' from the `source()` function.
#'
#' An explicit and easy-to-understand method of extracting
#' the value from the `source()` function.
#'
#' @param file A `.R` file to source into the environment.
#' @export

blg_source_value <- function(file) {
  stopifnot(file.exists(file))
  value <- source(file, echo = FALSE)
  value <- value["value"][[1]]
}