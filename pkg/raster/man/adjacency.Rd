\name{adjacency}
\alias{adjacency}

\title{ adjacency }
\description{
	calculate adjacent cells in a grid
}
\usage{
adjacency(raster, fromCells, toCells, mode, outerMeridianConnect) 
}

\arguments{
  \item{raster}{ an object of the Raster family }
  \item{fromCells}{ a vector of cell numbers for which adjacent cells should be calculated }
  \item{toCells}{ a vector of cell numbers from which adjacent cells are selected }
  \item{directions}{ in how many direction cells should be connected: 4, 8 or 16. }
  \item{outerMeridianConnect}{ logical value indicating whether the outer columns of the raster should be connected as a cylinder }
}

\details{
Cell numbers start with 1 in the upperleft corner and increase from left to right and from top to bottom.
Number of directions: 4 connects cells with one-cell rook moves, 8 with one-cell queen moves, and 16 with knight and one-cell queen moves.
}

\value{
  a two column matrix with each row containing a pair of adjacent cells. 
}
\author{ Jacob van Etten \email{jacobvanetten@yahoo.com} }

\examples{
	rs <- newRaster(nrows=10, ncols=10)
	adjacency(raster = rs, fromCells = c(1,30,55,72,100), toCells = c(1:ncells(rs)), directions=16, outerMeridianConnect = TRUE) 
}

\keyword{ spatial }