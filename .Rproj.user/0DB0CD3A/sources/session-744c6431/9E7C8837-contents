#' Visualise mean arrival delays by airport
#'
#' This function summarises flight arrival delays by destination airport
#' and returns a scatter plot of airports
#' positioned by longitude/latitude and coloured by mean delay.
#'
#' @return A `ggplot2` plot object.
#' @import nycflights13
#'
#' @details
#' Packages used:
#' - dplyr (for data manipulation)
#' - ggplot2 (for plotting)
#' - nycflights13 (for the datasets)
#'
#' @examples
#' \dontrun{
#'   visualize_airport_delays()
#' }
#'
#' @export
visualize_airport_delays <- function() {

  delay_data <- nycflights13::flights |>
    dplyr::group_by(dest) |>
    dplyr::summarise(
      mean_delay = mean(arr_delay, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      nycflights13::airports,
      by = c("dest" = "faa")
    ) |>
    dplyr::select(dest, mean_delay, lat, lon)

  ggplot2::ggplot(
    delay_data,
    ggplot2::aes(x = lon, y = lat, colour = mean_delay)
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::labs(
      title = "Mean Arrival Delay by Airport",
      x = "Longitude",
      y = "Latitude",
      colour = "Mean Delay (min)"
    ) +
    ggplot2::theme_minimal()
}
