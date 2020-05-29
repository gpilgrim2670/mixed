#' Point finder
#'
#' Finds points defining area of each liquid
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param area_pct a vector of fractional areas, must sum to 1, maximum length = 4
#' @param drink = a dataframe containing two columns, drink and density, maximumn rows  = 4
#' @return the points defining the verticies of trapezoids based on drink area and density
#'
#' @examples
#' drinks <- data.frame(drink = c("soda", "cranberry", "milk"),
#'                      density = c(1, 2, 3),
#'                      stringsAsFactors = FALSE)
#'
#' area_pcts <- c(0.25, 0.25, 0.5)
#' df <- point_finder(drink = drinks, area_pcts = area_pcts)

point_finder <- function(drink, area_pcts) {
  numb_samples <- length(area_pcts)
  df <- cbind(area_pcts, drink)
  df <- df[order(df$density, decreasing = TRUE),]
  if (numb_samples == 1) {
    b_bottom = 3
    b_top = 5
    height = 10
    d_1 <-
      data.frame(
        y = c((10 - height), (10 - height), height, height),
        x = c((5 - 0.5 * b_bottom),
              (5 + 0.5 * b_bottom),
              (5 - 0.5 * b_top),
              (5 + 0.5 * b_top)
        ),
        id = rep(drinks[[1]][1], each = 4),
        stringsAsFactors = FALSE
      )
    return(d_1)
  } else if (numb_samples == 2) {
    b_bottom = 3
    b_1 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_bottom^2)-(40 * df$area_pcts[1] * 2)
    )
    height_1 = 2 * (40 * df$area_pcts[1]) / (b_bottom + b_1)
    b_top = 5
    height_total = 10
    d_2 <-
      data.frame(
        y = c(
          (10 - height_total),
          (10 - height_total),
          height_1,
          height_1,
          height_1,
          height_1,
          height_total,
          height_total
        ),
        x = c((5 - 0.5 * b_bottom),
              (5 + 0.5 * b_bottom),
              (5 + 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 + 0.5 * b_1),
              (5 + 0.5 * b_top),
              (5 - 0.5 * b_top)

        ),
        id = rep(c(df$drink[1], df$drink[2]), each = 4),
        stringsAsFactors = FALSE
      )
    d_2 <- merge(df, d_2, by.x = "drink", by.y = "id")
    return(d_2)
  } else if (numb_samples == 3) {
  b_bottom = 3
  b_1 = quadratic_solver(
    a = 5,
    b = 0,
    c = (-5 * b_bottom^2)-(40 * df$area_pcts[1] * 2)
  )
  height_1 = 2 * (40 * df$area_pcts[1]) / (b_bottom + b_1)
  b_2 = quadratic_solver(
    a = 5,
    b = 0,
    c = (-5 * b_1^2)-(40 * df$area_pcts[2] * 2)
  )
  height_2 = 2 * (40 * df$area_pcts[2]) / (b_1 + b_2) + height_1
  b_top = 5
  height_total = 10
  d_3 <-
    data.frame(
      y = c(
        (10 - height_total),
        (10 - height_total),
        height_1,
        height_1,
        height_1,
        height_1,
        height_2,
        height_2,
        height_2,
        height_2,
        height_total,
        height_total
      ),
      x = c((5 - 0.5 * b_bottom),
            (5 + 0.5 * b_bottom),
            (5 + 0.5 * b_1),
            (5 - 0.5 * b_1),
            (5 - 0.5 * b_1),
            (5 + 0.5 * b_1),
            (5 + 0.5 * b_2),
            (5 - 0.5 * b_2),
            (5 - 0.5 * b_2),
            (5 + 0.5 * b_2),
            (5 + 0.5 * b_top),
            (5 - 0.5 * b_top)

      ),
      id = rep(c(df$drink[1], df$drink[2], df$drink[3]), each = 4),
      stringsAsFactors = FALSE
    )
  d_3 <- merge(df, d_3, by.x = "drink", by.y = "id")
  return(d_3)
  } else if (numb_samples == 4) {
    b_bottom = 3
    b_1 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_bottom^2)-(40 * df$area_pcts[1] * 2)
    )
    height_1 = 2 * (40 * df$area_pcts[1]) / (b_bottom + b_1)
    b_2 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_1^2)-(40 * df$area_pcts[2] * 2)
    )
    height_2 = 2 * (40 * df$area_pcts[2]) / (b_1 + b_2) + height_1
    b_3 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_2^2)-(40 * df$area_pcts[3] * 2)
    )
    height_3 = 2 * (40 * df$area_pcts[3]) / (b_2 + b_3) + height_2
    b_top = 5
    height_total = 10
    d_4 <-
      data.frame(
        y = c(
          (10 - height_total),
          (10 - height_total),
          height_1,
          height_1,
          height_1,
          height_1,
          height_2,
          height_2,
          height_2,
          height_2,
          height_3,
          height_3,
          height_3,
          height_3,
          height_total,
          height_total
        ),
        x = c((5 - 0.5 * b_bottom),
              (5 + 0.5 * b_bottom),
              (5 + 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 + 0.5 * b_1),
              (5 + 0.5 * b_2),
              (5 - 0.5 * b_2),
              (5 - 0.5 * b_2),
              (5 + 0.5 * b_2),
              (5 + 0.5 * b_3),
              (5 - 0.5 * b_3),
              (5 - 0.5 * b_3),
              (5 + 0.5 * b_3),
              (5 + 0.5 * b_top),
              (5 - 0.5 * b_top)

        ),
        id = rep(c(df$drink[1], df$drink[2], df$drink[3], df$drink[4]), each = 4),
        stringsAsFactors = FALSE
      )
    d_4 <- merge(df, d_4, by.x = "drink", by.y = "id")
    return(d_4)
  } else if (numb_samples == 5) {
    b_bottom = 3
    b_1 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_bottom^2)-(40 * df$area_pcts[1] * 2)
    )
    height_1 = 2 * (40 * df$area_pcts[1]) / (b_bottom + b_1)
    b_2 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_1^2)-(40 * df$area_pcts[2] * 2)
    )
    height_2 = 2 * (40 * df$area_pcts[2]) / (b_1 + b_2) + height_1
    b_3 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_2^2)-(40 * df$area_pcts[3] * 2)
    )
    height_3 = 2 * (40 * df$area_pcts[3]) / (b_2 + b_3) + height_2
    b_4 = quadratic_solver(
      a = 5,
      b = 0,
      c = (-5 * b_3^2)-(40 * df$area_pcts[4] * 2)
    )
    height_4 = 2 * (40 * df$area_pcts[4]) / (b_3 + b_4) + height_3
    b_top = 5
    height_total = 10
    d_5 <-
      data.frame(
        y = c(
          (10 - height_total),
          (10 - height_total),
          height_1,
          height_1,
          height_1,
          height_1,
          height_2,
          height_2,
          height_2,
          height_2,
          height_3,
          height_3,
          height_3,
          height_3,
          height_4,
          height_4,
          height_4,
          height_4,
          height_total,
          height_total
        ),
        x = c((5 - 0.5 * b_bottom),
              (5 + 0.5 * b_bottom),
              (5 + 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 - 0.5 * b_1),
              (5 + 0.5 * b_1),
              (5 + 0.5 * b_2),
              (5 - 0.5 * b_2),
              (5 - 0.5 * b_2),
              (5 + 0.5 * b_2),
              (5 + 0.5 * b_3),
              (5 - 0.5 * b_3),
              (5 - 0.5 * b_3),
              (5 + 0.5 * b_3),
              (5 + 0.5 * b_4),
              (5 - 0.5 * b_4),
              (5 - 0.5 * b_4),
              (5 + 0.5 * b_4),
              (5 + 0.5 * b_top),
              (5 - 0.5 * b_top)

        ),
        id = rep(c(df$drink[1], df$drink[2], df$drink[3], df$drink[4], df$drink[5]), each = 4),
        stringsAsFactors = FALSE
      )
    d_5 <- merge(df, d_5, by.x = "drink", by.y = "id")
    return(d_5)
  }
}
