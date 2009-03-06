\name{getRowColCell}

\alias{colFromCell}
\alias{rowFromCell}
\alias{cellFromRowCol}
\alias{cellFromRowColCombine}
\alias{cellFromRow}
\alias{cellFromCol}

  
\title{Get row, column, or cell number}

\description{
These functions get the row or column number from a cell number of a Raster* object, or vice versa)
}

\usage{
colFromCell(object, cell)
rowFromCell(object, cell)
cellFromRowCol(object, rownr, colnr)
cellFromRowColCombine(object, rownr, colnr)
cellFromRow(object, rownr)
cellFromCol(object, colnr)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cell}{cell number(s)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
}
  
\details{
  The colFromCell and similar functions accept a single value (or x, y pair), or a vector or list of these values,
  Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
  The last cell number equals the number of cells of the Raster* object.
  
  In \code{cellFromRowCol}, \code{rownr} and  \code{colnr} should have the same length.
  This is not the case for \code{cellFromRowColCombine}. This function returns the cell numbers obtained by the combination of row and column numbers.
 
}

\value{
  row, col or cell number(s) 
}
\author{Robert J. Hijmans }

\examples{
#using a new default raster (1 degree global)
r <- raster()
colFromCell(r, 10000)
rowFromCell(r, 10000)
cellFromRowCol(r, 5, 5)
cellFromRowCol(r, 4:5, 4:5)
cellFromRowColCombine(r, 4:5, 4:5)
cellFromCol(r, 1)
cellFromRow(r, 1)

}

\keyword{spatial}
