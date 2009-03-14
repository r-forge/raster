\name{utilities}
\alias{roundCoords}
\alias{newCRS}
  
\title{helper functions }

\description{
  roundCoords rounds the coordinates of the BoundingBox of a Raster* to a number of digits specified. 
  newCRS is a helper function to create a CRS map projection object.
}

\usage{
roundCoords(object, digits=0)
newCRS(projs)
}

\arguments{
  \item{object}{ a Raster* object }
  \item{digits}{ integer indicating the precision to be used} 
  \item{projs}{charater. a PROJ4 type description of a projection, its paramaters, and the datum}
 }
  
\author{Robert J. Hijmans }
\examples{
	crsproj <- newCRS("+proj=longlat +datum=WGS84")
	bb <- newBbox(-179.999999,  180.000011, -60, 60)
	bb <- roundCoords(bb, 5)
}
\keyword{ spatial }

