\name{isLatLon}

\alias{isLatLon}
\alias{isLatLon,Raster-method}
\alias{isLatLon,CRS-method}
\alias{isLatLon,character-method}

  
\title{Raster extent}
\description{
  get or set the projection of Raster* objects
}

\usage{
isLatLon(object)
}

\arguments{
  \item{object}{Raster* object}
}


\value{
Logical
}

\author{Robert J. Hijmans }

\examples{
r <- raster()
isLatLon(r)
projection(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
isLatLon(r)
}

\keyword{spatial}
