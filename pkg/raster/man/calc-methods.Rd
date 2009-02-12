\name{calc-methods}
\docType{methods}
\alias{calc}
\alias{calc,RasterLayer,function-method}
\alias{calc,RasterStack,function-method}
\alias{calc,RasterStack,list-method}

\title{ Calculate }

\description{
 Calculate values for a new RasterLayer, from a single RasterLayer or RasterStack
}


\section{Methods}{
\describe{
\item{x = "RasterLayer", fun = "function"}{ 
  \code{calc(x, fun, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')}
  \item{\code{x}}{A RasterLayer object}
  \item{\code{fun}}{The function to be applied}
  \item{\code{filename}}{Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot}
  \item{\code{overwrite}}{Logical to indicate whether an existing output file should be overwritten}
  \item{\code{filetype}}{output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}}}
  \item{\code{datatype}}{output data type; see \code{\link[raster]{setDatatype}}}
  \item{\code{track}}{vector of row numbers for which the function will report that they have been processed}  
}


\item{x = "RasterStack", fun = "function"}{
  as above
}


\item{x = "RasterStack", fun = "list"}{ 
   coming soon...
}

}}

\details{
if \code{x = "RasterLayer"}, \code{fun} should be a function that takes a single value as input, and returns a single value.
if \code{x = "RasterStack"}, \code{fun} should be a function that takes a one dimensional vector of value as input, and returns a single value.
}


\value{
	a new RasterLayer 
}
\author{ Robert J. Hijmans}

\examples{
r <- raster(ncols=36, nrows=18)
r[] <- 1:ncell(r)

# multiply values with 10
fun <- function(x) { x * 10 }
rc <- calc(r, fun)

# set values below 100 to NA. 
fun <- function(x) { x[x<100] <- NA; return(x) }
rc <- calc(r, fun)

# set NA values to -9999
fun <- function(x) { x[is.na(x)] <- -9999; return(x)} 
rc <- calc(rc, fun)
}

\keyword{methods}
\keyword{spatial}



