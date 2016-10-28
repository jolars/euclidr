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
#' @import assertthat
#' @export

two_disk_overlap <- function(r1, r2, d = NULL, x1 = NULL, x2 = NULL, y1 = NULL,
                             y2 = NULL, warnings = TRUE) {
  assert_that(is.numeric(r1))
  assert_that(is.numeric(r2))
  assert_that(r1 > 0)
  assert_that(r2 > 0)

  if(is.null(d)) {
    assert_that(!is.null(x1))
    assert_that(!is.null(x2))
    assert_that(!is.null(y1))
    assert_that(!is.null(y2))
    assert_that(is.numeric(x1))
    assert_that(is.numeric(x2))
    assert_that(is.numeric(y1))
    assert_that(is.numeric(y2))

    d <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  } else {
    assert_that(is.numeric(d))
    assert_that(d >= 0)
  }

  if (d + min(r1, r2) < max(r1, r2)) {
    if (warnings) warning("One disk is a subset of the other. The smallest disks's area is returned.")
    pi * min(r1, r2) ^ 2
  } else if (d > r1 + r2) {
    if (warnings) warning("The disks to not intersect.")
    0
  } else {
    r1 ^ 2L * acos((d ^ 2L + r1 ^ 2L - r2 ^ 2L) / (2L * d * r1)) +
      r2 ^ 2L * acos((d ^ 2L + r2 ^ 2L - r1 ^ 2L) / (2L * d * r2)) -
      sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2L
  }
}

#' Find intersection points of circles
#'
#'
locate_intersections <- function(r1, r2, x_d, y_d, x_c, y_c, d) {
  l  <- (r2 ^ 2L - r1 ^ 2L + d ^ 2L) / (2L * d)
  h  <- sqrt(r2 ^ 2L - l ^ 2L)
  ld <- l / d
  hd <- h / d
  x1 <- x_d * ld + y_d * hd + x_c
  x2 <- x_d * ld - y_d * hd + x_c
  y1 <- y_d * ld - x_d * hd + y_c
  y2 <- y_d * ld + x_d * hd + y_c
  cbind(c(x1, x2), c(y1, y2))
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

find_sets_containing_points <- function (points, x, y, r) {
  x_int <- points[1]
  y_int <- points[2]
  (x_int - x) ^ 2L + (y_int - y) ^ 2L <= r ^ 2L
}
