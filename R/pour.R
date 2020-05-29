#' Pouring a Drink
#'
#' Plots layered drinks based on specific gravity
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 theme_void
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#'
#' @param drink = a dataframe containing two columns, drink and density, maximumn rows  = 4
#' @param area_pct a vector of fractional areas, must sum to 1, maximum length = 4
#' @return a plot of the layered beverage
#'
#' @examples
#' drinks <- data.frame(drink = c("soda", "cranberry", "milk"),
#'                      density = c(1, 2, 3),
#'                      stringsAsFactors = FALSE)
#' area_pcts <- c(0.25, 0.25, 0.5)
#' pour(drinks, area_pcts = area_pcts)
#'
#' @export

# positions_glass <- data.frame(
#   id = rep("glass", each = 4),
#   x = c(3.5, 6.5, 7.5, 2.5),
#   y = c(0, 0, 10, 10),
#   fill = rep("a")
# )

# ggplot(positions_glass, aes(x = x, y = y)) +
#   geom_polygon(aes(fill = fill, group = id)) +
#   coord_cartesian(xlim = c(0, 10),
#                   ylim = c(0, 10))


pour <- function(drink, area_pcts) {
  if(sum(area_pcts) != 1) stop('area_pcts must sum to 1')
  if(length(area_pcts) > 5 | nrow(drink) > 5) stop('only up to 5 drinks are supported')
  if("density" %in% names(drink) == FALSE) stop("drink must contain a column named density")
  if("drink" %in% names(drink) == FALSE) stop("drink must contain a column named drink")
  df <- point_finder(drink = drink, area_pcts = area_pcts)

  positions_notglass <- data.frame(
    id = c(rep("left", 4), rep("right", 4)),
    x = c(0, 3.5, 2.5, 0, 6.5, 10, 10, 7.5),
    y = c(0, 0, 10, 10, 0, 0, 10, 10),
    fill = rep("not_glass", 8),
    stringsAsFactors = FALSE
  )

  df <- df %>%
    arrange(density)
  p <- df %>%
    mutate(drink = factor(drink, levels = unique(drink))) %>%
    ggplot(aes(x = x, y = y)) +
    # geom_polygon(aes(group = drink, fill = drink)) +
    geom_polygon(aes(group = drink), fill = df$color) +
    coord_cartesian(xlim = c(0, 10),
                    ylim = c(0, 10)) +
    # geom_polygon(data = positions_glass, aes(x = x, y = y, group = id), color = "black", fill = NA, linetype = 2) +
    geom_segment(aes(x = 3.5, y = 0, xend = 2.5, yend = 10), color = "black") +
    geom_segment(aes(x = 6.5, y = 0, xend = 7.5, yend = 10), color = "black") +
    geom_segment(aes(x = 3.5, y = 0, xend = 6.5, yend = 0), color = "black", linetype = 2) +
    geom_polygon(data = positions_notglass, aes(x = x, y = y, group = id), fill = "white") +
    theme_void()

  return(p)
}

