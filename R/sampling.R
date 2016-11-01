#' Find the farthest points
#'
#' Select a subset of points that maximize the smallest pairwise distance
#' between the points
#'
#' This function begins with a random sample of n points and iterately puts them
#' back into the mother set and tries to see if there is a better candidate than
#' the point that was put back.
#'
#' @param dat A matrix of points to choose n from
#' @param n The number of points to select
#' @return A vector of indices for the points in the subset
#' @seealso \code{\link[stats]{dist}}
#' @examples
#' xy <- matrix(runif(200), ncol = 2)
#' id <- farthest_points(dat = xy, n = 5)
#' xyt[id, ]
#'
#' plot(xy)
#' points(xy[id, ], pch = 16)
#'
#' @export
#' @importFrom assertthat assert_that
farthest_points <- function(data, n) {
  assert_that(is.numeric(data))
  assert_that(nrow(data) >= length(n))
  assert_that(is.numeric(n))
  assert_that(length(n) > 1)
  assert_that(n %% 1 == 0) # check that it is an integer

  dmat <- as.matrix(stats::dist(data))
  r <- sample.int(nrow(dmat), n)
  repeat {
    r_old <- r
    for (i in 1:n) {
      mm <- dmat[r[-i], -r[-i], drop = FALSE]
      k <- which.max(mm[(1:ncol(mm) - 1) * nrow(mm) + max.col(t(-mm))])
      r[i] <- as.numeric(dimnames(mm)[[2]][k])
    }
    if (identical(r_old, r)) break #return(r)
  }
}
