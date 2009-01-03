\name{rasterstack.create}
\alias{rasterstack.from.stackfile}
\alias{rasterstack.from.rasterfiles}
\alias{rasterstack.from.rasters}
\alias{rasterstack.add.files}
\alias{rasterstack.add.rasters}
\alias{rasterstack.remove.rasters}
\alias{rasterstack.save}

\title{ Create or change a new stack }
\description{
  A raster stack is a collection of rasters with the same spatial extent and resolution. They can be crated from RasterLayer objects, or from raster files.
  You can add raster to or remove raster from a rasterstack. You can also save a stack to a file, to recreate it later.  
}

\usage{
rasterstack.from.stackfile(stackfile)
rasterstack.from.rasterfiles(rasterfiles, bands= rep(1, length(rasterfiles)))
rasterstack.from.rasters(rasters)
rasterstack.add.files(rstack, rasterfiles, bands= rep(1, length(rasterfiles))) 
rasterstack.add.rasters(rstack, rasters) 
rasterstack.remove.rasters(rstack, indices) 
rasterstack.save(rstack)
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
  st <- rasterstack.from.rasterfiles(rasterfile)
# adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- rasterstack.add.files(st, c(rasterfile, rasterfile))
  rs <- rasterFromFile(rasterfile)
  st <- rasterstack.add.rasters(st, c(rs, rs))
  st
  st <- rasterstack.remove.rasters(st, c(3, 5))
  nlayers(st)
}
\keyword{ spatial }
