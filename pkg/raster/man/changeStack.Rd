\name{changestack}

\alias{stackFromFiles}

\alias{addFiles}
\alias{addRasters}
\alias{dropLayer}

\title{ Change a RasterStack }

\description{
  A RasterStack is a collection of rasters with the same spatial extent and resolution. 
  With these functions, you can add raster to or remove raster from a rasterstack. 
}

\usage{
addFiles(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) 
addRasters(rstack, rasters) 
dropLayer(rstack, indices) 
}

\arguments{
  \item{rasterfiles}{ Filename(s) of (a) raster dataset(s) }
  \item{rasters}{ a list of RasterLayer objects }
  \item{bands}{ a vector or list of bands of raster data files (default values = 1)}
  \item{rstack}{ a RasterStack object }
  \item{indices}{ a vector of the indices of the raster(s) to remove from a RasterStack }  
}


\value{
  a RasterStack object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}}}

\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stackFromFiles(rasterfile)
# now adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- addFiles(st, c(rasterfile, rasterfile))
  rs <- raster(rasterfile)
  st <- addRasters(st, c(rs, rs))
  st
  st <- dropLayer(st, c(3, 5))
  nlayers(st)
}
\keyword{ spatial }
