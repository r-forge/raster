\name{xyFromCell}

\alias{colFromX}
\alias{rowFromY}
\alias{cellFromXY}
\alias{xFromCol}
\alias{yFromRow}
\alias{xyFromCell}
\alias{cxyFromBbox}

  
\title{Coordiates from a row, column or cell number}

\description{
These functions get coordinates of the center of raster cells for a row, column, or cell number of a Raster* object.
}

\usage{
colFromX(object, x)
rowFromY(object, y)
cellFromXY(object, xy)
xFromCol(object, colnr)
yFromRow(object, rownr)
xyFromCell(object, cell, asSpatialPoints=FALSE)
cxyFromBbox(object, bbox)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cell}{cell number(s)}
  \item{x}{x coordinate(s)}
  \item{y}{y coordinate(s)}
  \item{xy}{matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
  \item{bbox}{A BoundingBox object (or an object that can be coerced to a BoundingBox object)}
  \item{asSpatialPoints}{return a SpatialPoints object (sp package) instead of a matrix}
}
  
\details{
  Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
  The last cell number equals the number of cells of the Raster* object.
}

\value{
  colFromX, rowFromY, cellFromXY: row, col or cell number(s) 
  xFromCol, yFromCol: vector of x or y coordinate(s) 
  xyFromCell: matrix(x,y) of pairs of coordinates 
  cxyFromBbox: matrix(cell, x, y) of cellnumbers and x and y coordinates
}
\author{Robert J. Hijmans }

\examples{
#using a new default raster (1 degree global)
rs <- raster()
colFromX(rs, 0.5)
rowFromY(rs, 0.5)
cellFromXY(rs, c(0.5, 0.5))
xFromCol(rs, c(1, 120, 180))
yFromRow(rs, 90)
xyFromCell(rs, 10000)
xyFromCell(rs, c(0, 1, 32581, ncell(rs), ncell(rs)+1))

#using a file from disk
rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
rs
cellFromXY(rs, c(180000, 330000))
#xy for corners of grid:
xyFromCell(rs, c(1, ncol(rs), nrow(rs), ncell(rs)))
}

\keyword{spatial}
