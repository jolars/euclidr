#' Plot circles
#'
#' Plots any numbers of circles using base R graphics, starting by plotting the
#' first circle on the bottom and going up.
#'
#' draw_circles uses \code{\link[graphics]{polygon}} to draw the circles,
#' which means that they are technically just circle-shaped polygons. The
#' function is vectorized and can handle any number of circles.
#'
#' @param x x Coordinates for the circles' centers
#' @param y y coordinates for the circles' centers
#' @param r Radiuses of the circles
#' @param vertices The number of vertices used for drawing the circumference of
#'     each circle.
#' @param polygon_args List of arguments to pass to
#'     \code{\link[graphics]{polygon}} to adjust borders, fill, shadings, etc.
#' @param add If \code{FALSE} initializes a new plotting frame with
#'     \code{\link[graphics]{plot}}.
#' @param asp Controls the aspect ratio of the plot. Will not produce circles
#'     if it ses to anything other than 1.
#' @param \dots Arguments to pass to \code{\link[graphics]{plot}}
#' @return Plots circles either in the current plotting window or on a new
#'     plotting window.
#' @seealso \code{\link[graphics]{polygon}}, \code{\link[graphics]{plot}}
#' @examples
#' x <- runif(3)
#' y <- runif(3)
#' r <- runif(3, 0.2, 0.6)
#'
#' draw_circles(x, y, r, polygon_args = list(border = "transparent",
#'                                           col = c("goldenrod1",
#'                                                   "steelblue1",
#'                                                   "black")))
#' @export
#' @import assertthat
draw_circles <- function(x, y, r, polygon_args = list(), vertices = 500,
                         add = FALSE, asp = 1, ...) {
  assert_that(
    is.numeric(x),
    is.numeric(y),
    is.numeric(r),
    is.count(vertices),
    length(vertices) == 1,
    is.flag(add),
    length(asp) == 1,
    is.numeric(asp),
    all(r >= 0),
    is.list(polygon_args),
    length(x) == length(y),
    length(r) == length(x),
    length(r) == length(y)
  )

  x_coords <- double(0L)
  y_coords <- double(0L)
  g <- seq(0L, 2L * pi, length = vertices)

  for (i in seq_along(x)) {
    x_coords <- c(x_coords, r[i] * cos(g) + x[i], NA)
    y_coords <- c(y_coords, r[i] * sin(g) + y[i], NA)
  }

  if (!add) {
    graphics::plot(NULL,
         xlim = range(x_coords, na.rm = TRUE),
         ylim = range(y_coords, na.rm = TRUE),
         asp = asp,
         ...)
  }

  polygon_args[["x"]] <- x_coords
  polygon_args[["y"]] <- y_coords

  do.call(graphics::polygon, polygon_args)
}
