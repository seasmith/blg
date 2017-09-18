#' Bind a list of 'rows' (vectors) into a single tibble.
#'
#' This is a wrapper of `data.table::rbindlist()` that
#' converts the final output to a tibble.
#'
#' @param list_rows A list containing 'rows' of data (vectors).
#' @export

blg_bind_list_rows <- function(list_rows) {
  tibble::as_tibble(data.table::rbindlist(list_rows))
}

#' Convert parsed JSON data (list) into a nested tibble.
#'
#' Converts vectors into columns and converts data frames
#' into list columns. Tibblefies data.drames and produces
#' a tibble whose columns represent the top most elements
#' of the list.
#'
#' @param x A list of vectors and data.frames (from parsed JSON).
#' @export

blg_tibblefy <- function(x) {
  isdf <- unname(which(unlist(purrr::map(x, is.data.frame))))
  isndf <- seq_along(x)[-isdf]
  tbbl <- tibble::add_column(dplyr::bind_cols(x[isndf]), x[isdf])
  names(tbbl)[isdf] <- names(x)[isdf]
  names(tbbl)[isndf] <- names(x)[isndf]
  return(tbbl)
}

#' Combine (union) a list of data frames or tibbles into a
#' single tibble.
#'
#' Should just use `dplyr::bind_rows()`
#'
#' @param list_tibbles A list containing tibbles or data.frames.

blg_bind_list_tibbles <- function(list_tibbles){
  purrr::reduce(list_tibbles, function(...) dplyr::union_all(...))
}

