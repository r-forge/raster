\name{setValues}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterStack-method}
\alias{setValuesBlock}
\alias{setValuesRows}
\alias{setValuesSparse}
\alias{clearValues}

\title{ Set values of a RasterLayer object }

\description{
Use these functions to assign values ot a RasterLayer object. 
While you can access the slots of the object directly, you would do that at your own peril because when setting values,
multiple slots need to be changed. 
}

\usage{
setValues(object, values, rownr=-1, layer=-1) 
setValuesRows(object, values, startrow, endrow) 
setValuesBlock(raster, blockvalues, firstcell, lastcell) 
setValuesSparse(raster, sparsevalues, cellnumbers) 
clearValues(object)
}

\arguments{
  \item{raster}{ A \code{RasterLayer} object }
  \item{object}{ A \code{RasterLayer} or \code{RasterStack} object }
  \item{values}{ Data values to associate with the RasterLayer object. There should be values for all cells or for a single row}
  \item{layer}{ layer number (only relevant for RasterStack objects) }
  \item{rownr}{ row number, between 1 and nrow(raster) }
  \item{blockvalues}{ Data values to associate with the RasterLayer object for a block of data, i.e. a rectangular area within the entire raster }
  \item{startrow}{ first row}
  \item{endrow}{ last row}
  \item{firstcell}{ first cell (upper left corner) of the block values }
  \item{lastcell}{ last cell (upper left corner) of the block values }
  \item{sparsevalues}{ not implemented yet -- sparse data values. I.e. values for a number of raster cells }
  \item{cellnumbers}{ the cell numbers corresponding to the sparse data }
}


\value{
a Raster* object
}
\author{ Robert J. Hijmans }

\examples{ 
	rs <- raster()
	vals <- 1:ncell(rs)
	rs <- setValues(rs, vals)
	rs <- clearValues(rs)
	rowvals <- 1:ncol(rs)
	rs <- setValues(rs, rowvals, 3)
}

\keyword{ spatial }

