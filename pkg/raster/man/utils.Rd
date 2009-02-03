\name{utilities}
\alias{roundCoords}
\alias{newCRS}

  
\title{ Utility functions }
\description{
  roundCoords rounds the coordinates to a number of digits specified. 
  newCRS is a helper function to create a CRS map projection object.
}

\usage{
roundCoords(object, digits=0)
newCRS(projstring)
}

\arguments{
  \item{object}{ a Raster* object }
  \item{digits}{ integer indicating the precision to be used} 
  \item{projstring}{ a PROJ4 type character string describing projection and datum}
 }
  
\author{Robert J. Hijmans }
\examples{
	crsproj <- newCRS("+proj=longlat +datum=WGS84")
	bb <- newBbox(-179.999999,  180.000011, -60, 60)
	bb <- roundCoords(bb, 5)
}
\keyword{ spatial }

