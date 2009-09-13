\name{initialize}

\alias{init}
\alias{initXYID}

\title{Intitialize}

\description{
Set values for a new raster (from scratch), either using a formula (init) or by choosing the x or y coordinate or cell number (initXYID)
}

\usage{
init(raster, fun=runif, filename="", overwrite=FALSE, datatype='FLT4S', filetype='raster', track=-1) 
initXYID(raster, v='id', filename="", overwrite=FALSE, datatype='FLT4S', filetype='raster', track=-1) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied. This must be a function that can take the number of cells as a single argument to return a vector of values with a length equal to the number of cells}
  \item{v}{'x', 'y', or 'id'}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical. if \code{TRUE}, the file will be overwritten if it exists}
  \item{filetype}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{datatype}{output data type; see \code{\link[raster]{dataType}}}
  \item{track}{vector of row numbers for which the function will report that they have been processed}  
}

\details{
 \code{init} sets a value without reference to existing values. E.g. initialize with a random number.
}

\value{
A new RasterLayer object, and, in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
 r <- init(r, fun=runif)
 
# there are different ways to set all values to 1 
# with a function (this will work for very large files if a file name is supplied)
 set1function <- function(x){rep(1, x)}
 r <- init(r, fun=set1function, filename='test.grd', overwrite=TRUE)
# "manually", if all the values can be kept in memory.
 r <- setValues(r, rep(1, ncell(r)))
# or  
 r[] <- rep(1, ncell(r))
 
}

\keyword{spatial}

