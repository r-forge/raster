\name{cover}
\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}

\title{ Cover }
\description{
 Replace \code{NA} values in the first RasterLayer (x) with the values of the second RasterLayer (y)
 
 
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
The function returns a RasterLayer with the values of the second RasterLayer (\code{y}) where the first RasterLayer (\code{x}) has \code{NA} values, and the values of the first RasterLayer elsewhere.
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
