\name{calc}

\docType{methods}

\alias{calc}
\alias{calc,RasterLayer,function-method}
\alias{calc,RasterStack,function-method}
\alias{calc,RasterStack,list-method}

\title{Calculate}

\description{
 Calculate values for a new RasterLayer, from a single RasterLayer or from a RasterStack
}

\usage{
calc(x, fun, ...) 
}


\arguments{
  \item{x}{a Raster* object}
  \item{fun}{a function}
  \item{...}{Additional arguments. See below, under Methods}  
}

\section{Methods}{
\describe{
 
\bold{1) \code{x} is a RasterLayer object; \code{fun} is a function}

\code{calc(x, fun, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterLayer object \cr
  \tab  \code{fun}  \tab The function to be applied\cr
  \tab  \code{filename}  \tab Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot \cr
  \tab  \code{overwrite}  \tab Logical to indicate whether an existing output file should be overwritten \cr
  \tab  \code{filetype}  \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab  \code{datatype}  \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab  \code{track}  \tab vector of row numbers for which the function will report that they have been processed \cr
}


\bold{2) \code{x} is a RasterLayer object; \code{fun} is a function}

\code{calc(x, fun, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterStack object \cr
}
Other items as above

\bold{3) \code{x} is a RasterLayer object; \code{fun} is a list of functions}

\code{calc(x, fun, filename='', overwrite=FALSE, filetype='raster', datatype='FLT4S')}
  
\tabular{rll}{  
  \tab  \code{x} \tab a RasterStack object \cr
  \tab  \code{fun}  \tab a list of the functions to be applied \cr
}
Other items as above

}}

\details{
if \code{x = "RasterLayer"}, \code{fun} should be a function that takes a single value as input, and returns a single value.
if \code{x = "RasterStack"}, \code{fun} should be a function that takes a vector of value as input, and returns a single value.
}


\value{
a RasterLayer object
}


\seealso{ \code{ \link[raster]{overlay}} , \link[raster]{Arith-methods}, \link[raster]{Math-methods}} 


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
