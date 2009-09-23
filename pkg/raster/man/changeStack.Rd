\name{changeStack}

\alias{addLayer}
\alias{addLayer,RasterStack-method}
\alias{addLayer,RasterBrick-method}
\alias{addFiles}
\alias{dropLayer}

\title{Change a RasterStack}

\description{
  A RasterStack is a collection of RasterLayers with the same spatial extent and resolution. 
  With these functions, you can add RasterLayers to, or remove RasterLayers from, a RasterStack object. 
}

\usage{
addLayer(x, ...) 
addFiles(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) 
dropLayer(rstack, indices) 
}

\arguments{
  \item{x}{ a RasterStack or RasterBrick object }
  \item{...}{Raster* objects or filenames (character) }
  \item{rstack}{ a RasterStack object }
  \item{rasterfiles}{ Filename(s) of (a) raster dataset(s) }
  \item{bands}{ a vector or list of bands of raster data files (default values = 1)}
  \item{indices}{ a vector of the indices of the raster(s) to remove from a RasterStack }  
}


\value{
  a RasterStack object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}}}

\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stack(rasterfile)
# now adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- addFiles(st, c(rasterfile, rasterfile))
  rs <- raster(rasterfile)
  st <- addLayer(st, c(rs, rs))
  st
  st <- dropLayer(st, c(3, 5))
  nlayers(st)
}

\keyword{ spatial }
