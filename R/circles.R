#' Overlap of two disks
#'
#' Compute the overlap of two disks given the euclidean distance between their
#' centers. If no distance is given the distance will be computed by the
#' function provided that the coordinates for the center are provided.
#'
#' @param r1 Radius of the first disk
#' @param r2 Radius of the second disk
#' @param d Distance between the two disks
#' @param x1 x coordinate for the first disk's center
#' @param x2 x coordinate for the second disk's center
#' @param y1 y coordinate for the first disk's center
#' @param y2 y coordinate for the second disk's center
#'
#' @return The area of the overlap between the two disks.
#'
#' @examples
#' r1 <- runif(4, 0.2, 0.5)
#' r2 <- runif(4, 0.2, 0.5)
#' d  <- runif(4)#'
#' two_disk_overlap(r1, r2, d)
#'
#' @import assertthat
#'
#' @export

two_disk_overlap <- function(r1, r2, d = NULL, x1 = NULL, x2 = NULL, y1 = NULL,
                             y2 = NULL) {
  assert_that(
    all(length(r1) == length(r2)),
    all(r1 > 0),
    all(r2 > 0),
    all(is.numeric(r1)),
    all(is.numeric(r2))
  )

  if(is.null(d)) {
    assert_that(
      all(!is.null(x1)),
      all(!is.null(x2)),
      all(!is.null(y1)),
      all(!is.null(y2)),
      all(is.numeric(x1)),
      all(is.numeric(x2)),
      all(is.numeric(y1)),
      all(is.numeric(y2)),
      all(length(x1) == length(r1)),
      all(length(x2) == length(y1)),
      all(length(y2) == length(r2))
    )
    d <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  } else {
    assert_that(
      all(d >= 0),
      all(is.numeric(d)),
      all(length(d) == length(r1))
    )
  }

  i1 <- d + pmin.int(r1, r2) < pmax.int(r1, r2)
  i2 <- d > r1 + r2
  i3 <- !(i1 | i2)
  out <- double(length(d))

  out[i1] <- pi * pmin.int(r1[i1], r2[i1]) ^ 2
  out[i2] <- 0
  out[i3] <-
    r1[i3] ^ 2L *
      acos((d[i3] ^ 2L + r1[i3] ^ 2L - r2[i3] ^ 2L) / (2L * d[i3] * r1[i3])) +
    r2[i3] ^ 2L *
      acos((d[i3] ^ 2L + r2[i3] ^ 2L - r1[i3] ^ 2L) / (2L * d[i3] * r2[i3])) -
    sqrt((r1[i3] + r2[i3] - d[i3]) * (d[i3] + r1[i3] - r2[i3]) *
         (d[i3] - r1[i3] + r2[i3]) * (d[i3] + r1[i3] + r2[i3])) / 2L
}

#' Find intersection points of circles
#'
#' Intersects any number of circles to find the points at which they intersect.
#'
#' @param x X coordinates of the circle centers
#' @param y Y coordinates of the circle centers
#' @param r Radiuses of the circles
#' @param nan.rm Set to \code{TRUE} to drop all combinations of circles with no
#'     intersections.
#'
#' @return Return a matrix of x and y coordinates as well as the indices of the
#'     circles.
#'
#' @examples
#' r <- runif(5, 0.3, 0.8)
#' x <- runif(5)
#' y <- runif(5)
#'
#' int <- intersect_circles(x, y, r)
#' int
#'
#' draw_circles(x, y, r)
#' points(int[, 1], int[, 2], pch = 16)
#'
#' @export
#' @import assertthat
intersect_circles <- function(x, y, r, nan.rm = FALSE) {
  assert_that(
    is.numeric(x),
    is.numeric(y),
    is.numeric(r),
    all(r >= 0),
    is.flag(nan.rm),
    length(x) == length(y),
    length(x) == length(r),
    length(y) == length(r)
  )

  i <- utils::combn(length(x), 2, FUN = function(x) x)
  a <- i[1, ]
  b <- i[2, ]

  x_d <- x[a] - x[b]
  y_d <- y[a] - y[b]
  x_c <- matrix(x[i], 2)
  y_c <- matrix(y[i], 2)
  r1  <- r[a]
  r2  <- r[b]
  d   <- sqrt(x_d ^ 2 + y_d ^ 2)

  l  <- (r2 ^ 2L - r1 ^ 2L + d ^ 2L) / (2L * d)
  h  <- sqrt(r2 ^ 2L - l ^ 2L)

  x1 <- x_d * l / d + y_d * h / d + x[b]
  x2 <- x_d * l / d - y_d * h / d + x[b]
  y1 <- y_d * l / d - x_d * h / d + y[b]
  y2 <- y_d * l / d + x_d * h / d + y[b]

  out <- cbind(x = c(x1, x2), y = c(y1, y2), circle1 = a, circle2 = b)

  if (nan.rm)
    out[stats::complete.cases(out), ]
  else
    out
}

#' Calculate the area of a polygon
#'
#' The function starts by finding the centroid of the points and then orders
#' them from their angle to the center, whereafter it computes the area using
#' the triangle method.
#'
#' \code{polygon_area} is vectorized and hence faster than implementations
#' relying on for loops.
#'
#' @param x X coordinates for the vertices of the polygon
#' @param y Y coordinates for the vertices of the polygon
#' @return Area of the polygon
#'
#' @seealso \code{\link[geometry]{polyarea}}
#' @export
polygon_area <- function(x, y) {
  assert_that(
    is.numeric(x),
    is.numeric(y),
    are_equal(length(x), length(y))
  )

  n <- length(x)
  ux <- sum(x) / n
  uy <- sum(y) / n
  ranks <- order(atan2(x - ux, y - ux), method = "radix")
  x <- x[ranks]
  y <- y[ranks]
  s <- seq_along(x)
  k <- c(n, s[-n])
  sum((x[k] + x[s]) * (y[k] - y[s])) / 2L
}