\name{newCRS}
\alias{newCRS}
  
\title{Coordiante Reference System object}

\description{
newCRS is a helper function to create a CRS map projection object.
}

\usage{
newCRS(projs)
}

\arguments{
  \item{projs}{charater. a PROJ4 type description of a projection, its paramaters, and the datum}
 }
  
\author{Robert J. Hijmans }
\examples{
	crsproj <- newCRS("+proj=longlat +datum=WGS84")
}
\keyword{ spatial }

