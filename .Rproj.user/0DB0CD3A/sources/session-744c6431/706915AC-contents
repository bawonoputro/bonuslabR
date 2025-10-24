#' Visualize Airport Delays
#'
#' Creates a plot of mean arrival delays by airport using dplyr and ggplot2.
#' @return A ggplot object
#' @export
#' @importFrom dplyr group_by summarise left_join select
#' @importFrom ggplot2 ggplot geom_point aes labs theme_minimal
#' @import nycflights13
visualize_airport_delays <- function() {
  library(dplyr)
  library(ggplot2)
  library(nycflights13)

  # Calculate mean delay per destination
  delay_data <- flights %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    select(dest, mean_delay, lat, lon)

  # Plot
  ggplot(delay_data, aes(x = lon, y = lat, color = mean_delay)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = "Mean Arrival Delay by Airport",
      x = "Longitude",
      y = "Latitude",
      color = "Mean Delay (min)"
    ) +
    theme_minimal()
}
