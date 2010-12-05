\name{update}

\docType{methods}

\alias{update}
\alias{update,RasterLayer-method}

\title{Update cells of files (on disk)}

\description{
Update cell values of a file (i.e. the values in a file on disk). 
New experimental function. So far only for RasterLayer objects (that are linked to a file).

User beware: this function _will_ change your files (first make a copy if you are not sure what you are doing).
}

\usage{
update(object, ...) 
}

\arguments{
  \item{object}{RasterLayer}
  \item{...}{Additional arguments. 
  
  \code{v} - vector of new values 
  
  \code{cell} - cell from where to start writing
  }  
}

\value{
RasterLayer
}

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=5, ncol=10)
r[] = 0
r <- writeRaster(r, 'test', overwrite=TRUE, datatype='INT2S')
as.matrix(r)
r <- update(r, v=rep(1, 5), cell=6)
r <- update(r, v=1:10, cell=21)
# 99.99 gets rounded because this is an integer file
r <- update(r, v=99.99, cell=50)
as.matrix(r)

}

\keyword{methods}
\keyword{spatial}

