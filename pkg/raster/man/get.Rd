\name{get}
\alias{colFromCell}
\alias{rowFromCell}
\alias{cellFromRowCol}
\alias{colFromX}
\alias{rowFromY}
\alias{cellFromXY}
\alias{xFromCol}
\alias{yFromRow}
\alias{xyFromCell}
\alias{cxyFromBox}
\alias{validCells}
\alias{validCols}
\alias{validRows}

  
\title{ Get the column, row, or cell number of a raster from coordinates and vice versa }

\description{
These functions get the column, row, or cell number of a Raster* object for a x and/or y coordinate or get the coordinates of the center of a raster cell from a column, or cell number(s)
}

\usage{
colFromCell(object, cell)
rowFromCell(object, cell)
cellFromRowCol(object, rownr, colnr)
colFromX(object, x)
rowFromY(object, y)
cellFromXY(object, xy)
xFromCol(object, colnr)
yFromRow(object, rownr)
xyFromCell(object, cell, asSpatialPoints, projstring)
cxyFromBox(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx=ymax(object))
validCells(object, cell) 
validCols(object, colnr) 
validRows(object, rownr) 
}

\arguments{
  \item{object}{ Raster* object (or a SpatialPixels* or SpatialGrid* object) }
  \item{cell}{ cell number(s) }
  \item{x}{ x coordinate(s) }
  \item{y}{ y coordinate(s) }
  \item{xy}{ matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object }
  \item{colnr}{ column number(s) }
  \item{rownr}{ row number(s) }
  \item{asSpatialPoints}{ return a SpatialPoints object (sp package) instead of a matrix}
  \item{projstring}{ PROJ4 format character string to set projection for SpatialPoints object }
  \item{xmn}{ minimum x coordinate of bounding box to get cell numbers for }
  \item{xmx}{ maximum x coordinate of bounding box to get cell numbers for }
  \item{ymn}{ minimum y coordinate of bounding box to get cell numbers for }
  \item{ymx}{ maximum y coordinate of bounding box to get cell numbers for }
 }
  
\details{
  The colFromCell and similar functions accept a single value (or x, y pair), or a vector or list of these values,
  Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
  The last cell number equals the number of cells of the Raster* object.
}
\value{
  row, col or cell number(s) 
  x or y coordinate(s)
  matrix(x,y) of pairs of coordinates 
  matrix(cell, x, y) of cellnumbers and x and y coordinates
  TRUE of FALSE for valid*
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
rs <- newRaster()
ncells(rs)
colFromCell(rs, 10000)
rowFromCell(rs, 10000)
colFromX(rs, 0.5)
rowFromY(rs, 0.5)
cellFromXY(rs, c(0.5, 0.5))
xFromCol(rs, c(1, 120, 180))
yFromRow(rs, 90)
xyFromCell(rs, 10000)
xyFromCell(rs, c(0, 1, 32581, ncells(rs), ncells(rs)+1))

#using a file from disk
rs <- rasterFromFile(system.file("external/test.ag", package="sp"))
rs
cellFromXY(rs, c(180000, 330000))
#xy for corners of grid:
xyFromCell(rs, c(1, ncol(rs), nrow(rs), ncells(rs)))
}
\keyword{ spatial }
