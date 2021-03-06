% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circles.R
\name{two_disk_overlap}
\alias{two_disk_overlap}
\title{Overlap of two disks}
\usage{
two_disk_overlap(r1, r2, d = NULL, x1 = NULL, x2 = NULL, y1 = NULL,
  y2 = NULL)
}
\arguments{
\item{r1}{Radius of the first disk}

\item{r2}{Radius of the second disk}

\item{d}{Distance between the two disks}

\item{x1}{x coordinate for the first disk's center}

\item{x2}{x coordinate for the second disk's center}

\item{y1}{y coordinate for the first disk's center}

\item{y2}{y coordinate for the second disk's center}
}
\value{
The area of the overlap between the two disks.
}
\description{
Compute the overlap of two disks given the euclidean distance between their
centers. If no distance is given the distance will be computed by the
function provided that the coordinates for the center are provided.
}
\examples{

r1 <- runif(4, 0.2, 0.5)
r2 <- runif(4, 0.2, 0.5)
d  <- runif(4)

two_disk_overlap(r1, r2, d)
}

