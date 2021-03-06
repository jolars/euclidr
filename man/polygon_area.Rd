% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circles.R
\name{polygon_area}
\alias{polygon_area}
\title{Calculate the area of a polygon}
\usage{
polygon_area(x, y)
}
\arguments{
\item{x}{X coordinates for the vertices of the polygon}

\item{y}{Y coordinates for the vertices of the polygon}
}
\value{
Area of the polygon
}
\description{
The function starts by finding the centroid of the points and then orders
them from their angle to the center, whereafter it computes the area using
the triangle method.
}
\details{
\code{polygon_area} is vectorized and hence faster than implementations
relying on for loops.
}
\seealso{
\code{\link[geometry]{polyarea}}
}

