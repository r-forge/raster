\name{nudgeExtent}

\alias{nudgeExtent}
  
\title{Nudge extent coordinates}

\description{
nudgeCoords takes the floor (lower integer) of the mimumum x and y of the exent and the ceiling (upper integer) 
of the maximum x and y of an extent (or the extent of a Raster* object). Thus, the extent of the Raster* object, or the Extent, returned has rounded coordinates and that always includes the original extent. This can be useful when creating raster objects based on the extent of other objects.

use \code{round(x, digits=0)} to round the coordinates of the extent of a Raster* object to a number of digits specified. This can be useful when dealing with a small inprecision in the data (e.g. 179.9999 instead of 180).
}

\usage{
nudgeExtent(x)
}

\arguments{
\item{x}{Extent or Raster* object }
}
  
\author{Robert J. Hijmans }
\examples{
r <- raster(xmn=0.999999,  xmx=10.000011, ymn=-60, ymx=60)
nudgeExtent(r)	
round(r)
}

\keyword{ spatial }


