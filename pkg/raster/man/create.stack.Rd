\name{rasterstack.create}
\alias{makeStack}
\alias{makeStack,RasterLayer-method}
\alias{stackFromFiles}
\alias{stackFromRasters}
\alias{addFiles}
\alias{addRasters}
\alias{dropLayer}

\title{ Create or change a new RasterStack }
\description{
  A RasterStack is a collection of rasters with the same spatial extent and resolution. They can be crated from RasterLayer objects, or from raster files.
  You can add raster to or remove raster from a rasterstack. 
}

\usage{
makeStack(x, ...)
stackFromFiles(rasterfiles, bands= rep(1, length(rasterfiles)))
addFiles(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) 
addRasters(rstack, rasters) 
dropLayer(rstack, indices) 
}

\arguments{
   \item{rstack}{ a RasterStack object }
  \item{x}{a RasterLayer object}
  \item{...}{additional RasterLayer objects (or filenames) }
  \item{rasterfiles}{ Filename(s) of (a) raster dataset(s) }
  \item{bands}{ A vector or list of bands of raster data files (default values = 1)}
  \item{rasters}{ RasterLayer object(s) }
  \item{indices}{ the indices of the raster(s) to remove from a RasterStack }
}
\details{
  When a  RasterStack is saved to a file, only pointers (filenames) to raster datasets are saved, not the data. If the name or location of a raster file changes, the RasterStack becomes invalid.
  In stackFromFiles, use index=-1 to add all bands of a single file to the RasterStack

}
\value{
  a RasterStack object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}}, \code{\link[raster]{rasterFromFile}}}

\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stackFromFiles(rasterfile)
# now adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- addFiles(st, c(rasterfile, rasterfile))
  rs <- rasterFromFile(rasterfile)
  st <- addRasters(st, c(rs, rs))
  st
  st <- dropLayer(st, c(3, 5))
  nlayers(st)
}
\keyword{ spatial }
