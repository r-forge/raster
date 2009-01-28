\name{rasterstack.create}
\alias{stackSave}
\alias{stackOpen}

\title{ Create or change a new stack }
\description{
  A raster stack is a collection of rasters with the same spatial extent and resolution. They can be crated from RasterLayer objects, or from raster files.
  These two functions allow you to save the references to raster files and recreate a rasterStack object later. 
}

\usage{
stackOpen(stackfile)
stackSave(rstack)
}

\arguments{
  \item{stackfile}{ Filename for the stack (to save it on disk) }
  \item{rstack}{ a RasterStack object }
}
\details{
  When a  stack is saved to a file, only pointers (filenames) to raster datasets are saved, not the data. If the name or location of a raster file changes, the stack becomes invalid.
  In stackFromFiles, use index=-1 to add all bands of a single file to the RasterStack

}
\value{
  a stack object
}
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  rasterfile <- system.file("external/test.ag", package="sp")
  st <- stackFromFiles(c(rasterfile, rasterfile))
  st <- setFilename(st, "mystack")
  st <- stackSave(st)
# note that setFilename adds an extension .stk to a stackfile  
  st2 <- stackOpen("mystack.stk")
  st2
}
\keyword{ spatial }
