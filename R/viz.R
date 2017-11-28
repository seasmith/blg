#' A new way of getting line plots of all the variables.
#'
#' Plot all numeric variables against any existing base or zoo date variable.
#' You can specify which by using the `dc_order` argument.
#'
#' @param data The data to plot
#' @param dc_order Which date column should be used as the x-axis. Default is the first ordinal occurring date column.
#' @export
plot_lines <- function(data, dc_order = 1L) {
  is_yearmon <- function(x) identical("yearmon", class(x))
  stopifnot(any(are_date <- purrr::map_lgl(data, ~ lubridate::is.Date(.x) | is_yearmon(.x))))

  date_col <- which(are_date)[dc_order]

  stopifnot(any(are_numbers <- purrr::map_lgl(data, is.numeric)))

  num_cols <- which(are_numbers)

  data <- select(data, date_col, num_cols)
  data <- gather(data, ... = -c(date_col))

  names(data)[1] <- "date"

  gg <- ggplot(data, aes(date, value, group = key)) +
    geom_line() +
    facet_wrap(~key, scales = "free")

  return(gg)
}