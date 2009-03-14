\name{set}
\alias{setMinMax}

\title{Set extreme values }

\description{
The minimum and maximum value of a RasterLayer are computed from a file in disk and stored in the resulting RasterLayer object. 
}

\usage{
setMinMax(raster)
}

\arguments{
  \item{raster}{ A \code{RasterLayer} object }
}

\value{
a Raster* object
}

\seealso{ \code{\link[raster]{values}} }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster(system.file("pictures/Rlogo.jpg", package="rgdal"))
r
r <- setMinMax(r)
r
	
}

\keyword{ spatial }

