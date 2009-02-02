\name{cover}
\alias{cover}

\title{ Raster cover calculation }
\description{
  Replace NA values in a raster with the values of another raster 
}

\usage{
cover(x, y, filename="", overwrite=TRUE) 
}

\arguments{
  \item{x}{ a RasterLayer object }
  \item{y}{ a RasterLayer object }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{logical. If \code{TRUE}, existing files will be overwritten}
}

\details{
The function returns a RasterLayer with the values of the second RasterLayer where the values of the first RasterLayer are NA and the values of the first RasterLayer elsewhere.

If the input RasterLayer object has all values in memory (e.g. after readAll(raster)), the function will also return the new values in memory. If a filename is provided, the values will also be saved to that file. 
If the values are not in memory the new values will be written to file. 
}

\value{
A new RasterLayer object (in the R environment), and in some cases the side effect of a new file on disk.
}
\author{Robert J. Hijmans}

\examples{
r1 <- raster()
r1[] <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
fun <- function(x) { x[x<0.5] <- NA; return(x) }
r2 <- calc(r2, fun)
r3 <- cover(r2, r1)
}	

\keyword{ spatial }
