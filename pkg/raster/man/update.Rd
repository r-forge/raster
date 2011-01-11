\name{update}

\docType{methods}

\alias{update}
\alias{update,RasterLayer-method}
\alias{update,RasterBrick-method}

\title{Update raster cells of files (on disk)}

\description{
Update cell values of a file (i.e., cell values on disk) associated with a RasterLayer or RasterBrick. 

User beware: this function _will_ make changes to your file (first make a copy if you are not sure what you are doing).

Writing starts at a cell number \code{cell}. You can write a vector of values (in cell order), or a matrix. You can also provide a vector of cell numbers (of the same lenght as vector \code{v}) to update individual cells.

See \code{\link{writeFormats}} for supported formats. 
}

\usage{
update(object, ...) 
}

\arguments{
  \item{object}{RasterLayer or RasterBrick that is associated with a file}
  \item{...}{Additional arguments. 
  
  \code{v} - vector or matrix with new values 
  
  \code{cell} - cell from where to start writing. Or a vector of cell numbers if v is a vector of the same lenght.

  \code{band} - band (layer) to update (for RasterBrick objects).
  }  
}

\value{
RasterLayer or RasterBrick
}

\author{Robert J. Hijmans}

\examples{
# setting up an example RasterLayer with file
r <- raster(nrow=5, ncol=10)
r[] = 0
r <- writeRaster(r, 'test', overwrite=TRUE, datatype='INT2S')
as.matrix(r)

# update with a vector starting a cell
r <- update(r, v=rep(1, 5), cell=6)
# 99.99 gets rounded because this is an integer file
r <- update(r, v=9.99, cell=50)
as.matrix(r)

# update with a vector of values and matching vector of cell numbers
r <- update(r, v=5:1, cell=c(5,15,25,35,45))
as.matrix(r)

# updating with a marix, anchored at a cell number
m = matrix(1:10, ncol=2)
r <- update(r, v=m, cell=2)
as.matrix(r)

}

\keyword{methods}
\keyword{spatial}
