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
#' @param warnings Returns warnings if disks are separated ore one is contained
#'     within the otherr. Set to FALSE to disable.
#' @return The area of the overlap between the two disks.
#' @examples
#'
#' r1 <- runif(4, 0.2, 0.5)
#' r2 <- runif(4, 0.2, 0.5)
#' d  <- runif(4)
#'
#' two_disk_overlap(r1, r2, d)
#' @import assertthat
#'
#' @export

two_disk_overlap <- function(r1, r2, d = NULL, x1 = NULL, x2 = NULL, y1 = NULL,
                             y2 = NULL, warnings = TRUE) {
  assert_that(
    all(length(r1) == length(r2)),
    all(r1 > 0),
    all(r2 > 0),
    all(is.numeric(r1)),
    all(is.numeric(r2)),
    all(length(x1) == length(r1)),
    all(length(x1) == length(y1)),
    all(length(y2) == length(r2)),
    all(length(d) == length(x2)),
    length(warnings) == 1,
    is.flag(warnings)
  )

  if(is.null(d)) {
    assertthat::assert_that(
      all(!is.null(x1)),
      all(!is.null(x2)),
      all(!is.null(y1)),
      all(!is.null(y2)),
      all(is.numeric(x1)),
      all(is.numeric(x2)),
      all(is.numeric(y1)),
      all(is.numeric(y2))
    )
    d <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  } else {
    assert_that(all(is.numeric(d)))
    assert_that(all(d >= 0))
  }

  i1 <- d + pmin.int(r1, r2) < pmax.int(r1, r2)
  i2 <- d > r1 + r2
  i3 <- !(i1 | i2)
  out <- double(length(d))

  out[i1] <- pi * pmin.int(r1[i1], r2[i1]) ^ 2
  out[i2] <- 0
  out[i3] <-
    r1[i3] ^ 2L * acos((d[i3] ^ 2L + r1[i3] ^ 2L - r2[i3] ^ 2L) /
                       (2L * d[i3] * r1[i3])) +
    r2[i3] ^ 2L * acos((d[i3] ^ 2L + r2[i3] ^ 2L - r1[i3] ^ 2L) /
                       (2L * d[i3] * r2[i3])) -
    sqrt((r1[i3] + r2[i3] - d[i3]) * (d[i3] + r1[i3] - r2[i3]) *
         (d[i3] - r1[i3] + r2[i3]) * (d[i3] + r1[i3] + r2[i3])) / 2L
}

#' Find intersection points of circles
#'
#' Intersects any number of circles to find the points at which they intersect.
#'
#' @param x x coordinates of the circle centers
#' @param y y coordinates of the circle centers
#' @param r Radiuses of the circles
#' @param nan.rm Whether to drop
#' @return Return a matrix of x and y coordinates as well as the indices of the
#'     circles.
#' @examples
#' r <- runif(5, 0.3, 0.8)
#' x <- runif(5)
#' y <- runif(5)
#'
#' int <- intersect_circles(x, y, r)
#' int
#'
#' euclidr::draw_circles(x, y, r)
#' points(int[, 1], int[, 2], pch = 16)
#'
#' @export
#' @import assertthat
intersect_circles <- function(x, y, r, nan.rm = FALSE) {
  assert_that(all(is.numeric(x)))
  assert_that(all(is.numeric(y)))
  assert_that(all(is.numeric(r)))
  assert_that(all(r >= 0))
  assert_that(is.logical(nan.rm))
  assert_that(length(x) == length(y) & length(y) == length(r))
  assert_that(length(nan.rm) == 1)

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

  out <- cbind(x = c(x1, x2),
               y = c(y1, y2),
               circle1 = a,
               circle2 = b)

  if (nan.rm)
    out[stats::complete.cases(out), ]
  else
    out
}

# Compute the area of a polygon
find_polygon_area <- function(x, y, n) {
  s <- seq_along(x)
  k <- c(length(s), s[-length(s)])
  sum((x[k] + x[s]) * (y[k] - y[s])) / 2L
}

# Compute the overlap of three or more circles
find_threeplus_areas <- function(x_int, y_int, radiuses, circles) {
  # Sort points clockwise from center
  j <- n <- length(x_int)
  ind <- order(atan2(x_int - sum(x_int) / n, y_int - sum(y_int) / n),
               method = "radix")
  x_int <- x_int[ind]
  y_int <- y_int[ind]
  circles <- circles[, ind]

  arc_areas <- double(n)
  for (i in 1:n) {
    circle_now <- circles[, i][circles[, i] %in% circles[, j]]

    d <- sqrt((x_int[j] - x_int[i]) ^ 2L + (y_int[j] - y_int[i]) ^ 2L)
    r <- radiuses[circle_now]

    # Find angle from center to segment
    u <- 2L * asin(d / (2L * r))

    # Find area of circle segment, in case we have to competing circles
    A <- ((r ^ 2L) / 2L) * (u - sin(u))

    # Pick the smallest area in case there are two competing areas
    arc_areas[i] <- min(A)

    j <- i
  }
  sum(arc_areas, find_polygon_area(x_int, y_int, n))
}

find_sets_containing_points <- function(points, x, y, r) {
  x_int <- points[1]
  y_int <- points[2]
  (x_int - x) ^ 2L + (y_int - y) ^ 2L <= r ^ 2L
}
