\name{cover}
\alias{cover}

\title{ Raster cover calculation }
\description{
  Replace NA values in a raster with the values of another raster 
}

\usage{
	cover(raster1, raster2, filename="", overwrite=TRUE) 
}

\arguments{
  \item{raster1}{ a RasterLayer object }
  \item{raster2}{ a RasterLayer object }
  \item{filename}{ filename for the output raster. A valid filename must be provided when the data of the input rasters are on disk }
  \item{overwrite}{logical. If \code{TRUE}, existing files will be overwritten}
}

\details{
  the function returns a RasterLayer with the values of the second RasterLayer where the first RasterLayer values are NA; and the values of the first RasterLayer elsewhere.
}
\value{
  a new RasterLayer object (if a filename is provided, a file on disk) 
}
\author{ Robert J. Hijmans \email{r.hijmans@gmail.com} }

\examples{
	r <- newRaster()
	r1 <- init(r)
	r2 <- init(r)
	r3 <- setNA(r2, value=0.5)
	r4 <- cover(r3, r1)
}	

\keyword{ spatial }
