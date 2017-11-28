#' A file path generator that ends with file separator.
#'
#' Generate file paths that end with the system's file separator, or whichever
#' other separator suits your needs
#' @param ... Characters to generate the file path
#' @param fsep The separator to use between characters and at the end of the
#'   file path
#' @export
file_path <- function(..., fsep = .Platform$file.sep) {
  if (length(list(...)) == 0) return(file.path())

  paste0(file.path(..., fsep = fsep), fsep)
}
