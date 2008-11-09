\name{get}
\alias{get.col.from.cell}
\alias{get.row.from.cell}
\alias{get.cell.from.rowcol}
\alias{get.col.from.x}
\alias{get.row.from.y}
\alias{get.cell.from.xy}
\alias{get.x.from.col}
\alias{get.y.from.row}
\alias{get.xy.from.cell}
\alias{get.cxy.from.box}
\alias{valid.cells}
\alias{valid.cols}
\alias{valid.rows}

  
\title{ Get the column, row, or cell number of a raster from coordinates and vice versa }

\description{
These functions get the column, row, or cell number of a Raster object (and of all other descendants of VirtualRaster, e.g. a RasterStack) for a x and/or y coordinate or get the coordinates of the center of a raster cell from a column, or cell number(s)
}

\usage{
get.col.from.cell(raster, cell)
get.row.from.cell(raster, cell)
get.cell.from.rowcol(raster, rownr, colnr)
get.col.from.x(raster, x)
get.row.from.y(raster, y)
get.cell.from.xy(raster, xy)
get.x.from.col(raster, colnr)
get.y.from.row(raster, rownr)
get.xy.from.cell(raster, cell)
get.cxy.from.box(raster, xmin=xmin(raster), xmax=xmax(raster), ymin=ymin(raster), ymax=ymax(raster))
valid.cells(raster, cell) 
valid.cols(raster, colnr) 
valid.rows(raster, rownr) 
}

\arguments{
  \item{raster}{ raster or stack object }
  \item{x}{  x coordinate(s) }
  \item{y}{  y coordinate(s) }
  \item{xy}{ matrix of x and y coordinates }
  \item{colnr}{  column number(s) }
  \item{rownr}{  row number(s) }
  \item{cell}{  cell number(s) }
  \item{xmin}{ minimum x coordinate of bounding box to get cell numbers for }
  \item{xmax}{ maximum x coordinate of bounding box to get cell numbers for }
  \item{ymin}{ minimum y coordinate of bounding box to get cell numbers for }
  \item{ymax}{ maximum y coordinate of bounding box to get cell numbers for }
 }
  
\details{
  The get.col.from.cell and similar functions accept a single value (or x, y pair), or a vector or list of these values,
  Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
  The last cell number equals the number of cells of the raster.
}
\value{
  row, col or cell number(s) 
  x or y coordinate(s)
  matrix(x,y) of pairs of coordinates 
  matrix(cell, x, y) of cellnumbers and x and y coordinates
  TRUE of FALSE for raster.is.valid.cell
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
#using a new default raster (1 degree global)
rs <- raster.new()
ncells(rs)
get.col.from.cell(rs, 10000)
get.row.from.cell(rs, 10000)
get.col.from.x(rs, 0.5)
get.row.from.y(rs, 0.5)
get.cell.from.xy(rs, c(0.5, 0.5))
get.x.from.col(rs, c(1, 120, 180))
get.y.from.row(rs, 90)
get.xy.from.cell(rs, 10000)
get.xy.from.cell(rs, c(0, 1, 32581, ncells(rs), ncells(rs)+1))

#using a file from disk
rs <- raster.from.file(system.file("external/test.ag", package="sp"))
rs
get.cell.from.xy(rs, c(180000, 330000))
#xy for corners of grid:
get.xy.from.cell(rs, c(1, ncol(rs), nrow(rs), ncells(rs)))
}
\keyword{ spatial }
