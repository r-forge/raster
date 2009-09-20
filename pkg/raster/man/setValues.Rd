\name{setValues}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterStackBrick-method}
\alias{setValuesBlock}
\alias{setValuesRows}
\alias{setValuesSparse}
\alias{clearValues}

\title{Set values}

\description{
Use these functions to assign values to a RasterLayer object. While you can access the slots of the object directly, you would do that at your own peril because when setting values,
multiple slots need to be changed. 
}

\usage{
setValues(object, values, rownr=-1, layer=-1) 
setValuesRows(object, values, firstcell, lastcell) 
setValuesBlock(raster, blockvalues, firstcell, lastcell) 
setValuesSparse(raster, sparsevalues, cellnumbers) 
clearValues(object)
}

\arguments{
  \item{raster}{A \code{RasterLayer} object }
  \item{object}{A \code{RasterLayer} or \code{RasterStack} object }
  \item{values}{Data values to associate with the RasterLayer object. There should be values for all cells or for a single row}
  \item{layer}{Layer number (only relevant for RasterStack objects) }
  \item{rownr}{Row number, between 1 and nrow(raster) }
  \item{blockvalues}{Data values to associate with the RasterLayer object for a block of data, i.e. a rectangular area within the entire raster }
  \item{firstcell}{First cell (upper left corner) of the block values }
  \item{lastcell}{Last cell (upper left corner) of the block values }
  \item{sparsevalues}{Not implemented yet -- sparse data values. I.e. values for a number of raster cells }
  \item{cellnumbers}{The cell numbers corresponding to the sparse data}
}


\seealso{ \code{\link[raster]{replacement}} }


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

