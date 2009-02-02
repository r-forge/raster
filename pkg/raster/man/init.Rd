\name{initialize}

\alias{init}

\title{Intitialize values of a RasterLayer}
\description{
  Set values for a new raster (from scratch)
}
\usage{
init(raster, fun=runif, filename="", overwrite=FALSE, asInt = FALSE) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{fun}{The function to be applied}
  \item{filename}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{overwrite}{Logical to indicate whether an existing output file should be overwritten}
  \item{asInt}{logical. If \code{TRUE}, output will be integer values}
}

\details{
\code{init} sets a value without reference to existing values. E.g. initialize with a single, sequential, or random number.
	
If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
 
# there are different ways to set all values to 1 
# with a function (this will work for very large files if a file name is supplied)
 set1function <- function(x){rep(1, x)}
 r <- init(r, fun=set1function, filename='test.grd')
# "manually", if all the values can be kept in memory.
 r <- setValues(r, rep(1, ncell(r)))
# or  
 r[] <- rep(1, ncell(r))
 
}

\keyword{spatial}

