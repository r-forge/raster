\name{cover}
\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}

\title{ cover }
\description{
 Replace \code{NA} values in the first RasterLayer with the values of the second RasterLayer
 
 
}
\section{Methods}{
\describe{
\code{cover(x, y, filename="", overwrite=TRUE) }

  \item{\code{x}}{ a RasterLayer object }
  \item{\code{y}}{ a RasterLayer object }
  \item{\code{filename}}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{\code{overwrite}}{logical. If \code{TRUE}, existing files will be overwritten}


\item{x = "RasterLayer", y = "RasterLayer"}{ ~~describe this method here }
}}

\details{
The function returns a RasterLayer with the values of the second RasterLayer, \code{y}, 
where the values of the first RasterLayer, \code{x}, are NA and the values of the first RasterLayer elsewhere.

If the input RasterLayer objects have all values in memory (e.g. after readAll(raster)), 
the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 

If no filename is specified, and the resulting RasterLayer is too large to hold in memory, it is saved to a temporary file.  
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=36, nrows=18)
r1[] <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2 <- calc(r2, fun=function(x){ x[x<0.5] <- NA; return(x)} )
#equivalent to: 
#r2[r2<0.5] <- NA

r3 <- cover(r2, r1)
}	

\keyword{methods}
\keyword{spatial}
