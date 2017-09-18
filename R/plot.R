#' A 'theme_minimal' based theme.
#'
#' This is 'theme_minimal' without minor grids and with
#' diferent text settings (Open Sans; 16).
#'
#' @param text_size Base font size.
#' @export

blg_theme_default <- function(text_size = 16) {
  ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(family = "Open Sans",
                              size = text_size),
          complete = TRUE)
}

