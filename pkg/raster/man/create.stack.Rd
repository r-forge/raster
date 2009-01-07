\name{rasterstack.create}
\alias{stackFromFile}
\alias{stackFromRasterfiles}
\alias{stackFromRasters}
\alias{stackAddFiles}
\alias{stackAddRasters}
\alias{stackRemoveRasters}
\alias{stackSave}

\title{ Create or change a new stack }
\description{
  A raster stack is a collection of rasters with the same spatial extent and resolution. They can be crated from RasterLayer objects, or from raster files.
  You can add raster to or remove raster from a rasterstack. You can also save a stack to a file, to recreate it later.  
}

\usage{
stackFromFile(stackfile)
stackFromRasterfiles(rasterfiles, bands= rep(1, length(rasterfiles)))
stackFromRasters(rasters)
stackAddFiles(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) 
stackAddRasters(rstack, rasters) 
stackRemoveRasters(rstack, indices) 
stackSave(rstack)
}

\arguments{
  \item{stackfile}{ Filename for the stack (to save it on disk) }
  \item{rstack}{ a stack object }
  \item{rasterfiles}{ Filename(s) of (a) raster dataset(s) }
  \item{bands}{ A vector or list of bands of raster data files (default values = 1)}
  \item{rasters}{ RasterLayer object(s) }
  \item{indices}{ the indices (in stack@rasters) of the raster(s) to remove from a stack }
}
\details{
  When a  stack is saved to a file, only pointers (filenames) to raster datasets are saved, not the data. If the name or location of a raster file changes, the stack becomes invalid.
  rasters contained in a stack can be accessed like this stack@rasters[[index]] where index is between 1 and stack@nrasters.  
}
\value{
  a stack object
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stackFromRasterfiles(rasterfile)
# now adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- stackAddFiles(st, c(rasterfile, rasterfile))
  rs <- rasterFromFile(rasterfile)
  st <- stackAddRasters(st, c(rs, rs))
  st
  st <- stackRemoveRasters(st, c(3, 5))
  nlayers(st)
  st <- setFilename(st, "mystack")
  st <- stackSave(st)
# note that setFilename adds an extension .stk to a stackfile  
  st2 <- stackFromFile("mystack.stk")
  st2
}
\keyword{ spatial }
