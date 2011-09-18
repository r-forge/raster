\name{adjacent}

\alias{adjacent}

\title{Adjacent cells}

\description{
Identify cells that are adjacent to a set of cells on a raster. See \code{\link{adjacency}} if you want to know which cells are adjacent to a particular cell.
}

\usage{
adjacent(x, cells, directions) 
}

\arguments{
  \item{x}{Raster* object}
  \item{cells}{vector of cell numbers for which adjacent cells should be found}
  \item{directions}{the number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case)}
}

\details{
Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom.

The function connects the outer meridians if the raster is a global raster with a longitude/latitude coordinate reference system.
}

\value{
vector with adjacent cells. 
}

\seealso{
\code{\link{adjacency}}
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrows=10, ncols=10)
adjacent(r, cells = c(1,55,100), directions=4) 
}

\keyword{spatial}
