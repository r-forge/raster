\name{cellFrom}

\alias{cellFromRowCol}
\alias{cellFromRowColCombine}
\alias{cellFromRow}
\alias{cellFromCol}

  
\title{Get cell number}

\description{
Get cell number(s) of a Raster* object from row and/or column numbers.   
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
The last cell number equals the number of cells of the Raster* object.
}

\usage{
cellFromRowCol(object, rownr, colnr)
cellFromRowColCombine(object, rownr, colnr)
cellFromRow(object, rownr)
cellFromCol(object, colnr)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
}
  
\details{
  In \code{cellFromRowCol}, \code{rownr} and  \code{colnr} should have the same length.
  This is not the case for \code{cellFromRowColCombine}. This function returns the cell numbers obtained by 
  the combination of row and column numbers.
}

\value{
cell number(s) 
}
\author{Robert J. Hijmans }

\examples{
r <- raster(ncols=10, nrows=10)
cellFromRowCol(r, 5, 5)
cellFromRowCol(r, 1:2, 1:2)
cellFromRowColCombine(r, 1:3, 1:2)
cellFromCol(r, 1)
cellFromRow(r, 1)
}

\keyword{spatial}
