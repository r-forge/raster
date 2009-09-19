\name{cover}
\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}

\title{ Cover }
\description{
 Replace \code{NA} values in the first RasterLayer (x) with the values of the second RasterLayer (y), and so forth for additional RasterLayers.
}


\section{Methods}{
\describe{
\code{cover(x, y, ..., filename="") }
  \item{\code{x}}{ RasterLayer object }
  \item{\code{y}}{ RasterLayer object }
  \item{\code{...}}{ Additional RasterLayer objects or other arguments (see Details) }
  \item{\code{filename}}{ filename for the output RasterLayer}
}}

\details{
The function returns a RasterLayer with the values of the second RasterLayer (\code{y}) where the first RasterLayer (\code{x}) has \code{NA} values, and the values of the first RasterLayer elsewhere.

\tabular{rll}{
  \tab filetype \tab output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeRaster}} \cr
  \tab datatype \tab output data type; see \code{\link[raster]{dataType}} \cr
  \tab overwrite \tab if TRUE, "filename" will be overwritten if it exists \cr
  \tab track \tab vector of row numbers for which the function will report that they have been processed  \cr
 }
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
