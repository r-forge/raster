\name{properties}

\alias{dataType} 
\alias{dataSize} 
\alias{dataSigned} 

  
\title{Raster properties}

\description{
  Properties of Raster* objects
}

\usage{
dataSize(object)
dataSigned(object)
dataType(object)
}

\arguments{
  \item{object}{Raster* object}
}

\note{
  Some of these functions simply take the values from the data slots in the RasterLayer or RasterStack object. It is advisable to use these and other 'getter' functions rather than dirctly accessing the slots of an object. This will ensure that code will not break in the future, even if the underlying class definitions change. 
}

\seealso{ \code{\link[raster]{filename}} }


\value{
  varies
}

\author{Robert J. Hijmans }

\examples{
#using a new default raster (1 degree global)
r <- raster(nrow=10, ncol=10)
dataContent(r)
r[] <- 1:ncell(r)
dataContent(r)
dataSource(r)
}

\keyword{spatial}
