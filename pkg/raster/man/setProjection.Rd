\name{setProjection}
\alias{setProjection}
\alias{projection<-}

\title{ Set projection }

\description{
Set the map projection of a Raster* object
}

\usage{
setProjection(x, value)
projection(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{value}{a CRS object or a character string describing a projection and datum in PROJ4 format }
}


\value{
a Raster* object
}

\seealso{ \code{\link[raster]{project}, \link[rgdal]{CRS-class}, \link[rgdal]{projInfo}}}

\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
r <- setProjection(r, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")

}

\keyword{ spatial }

