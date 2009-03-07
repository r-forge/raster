\name{draw}

\alias{drawPoly}
\alias{drawLine}

\title{ Draw a line or polygon on a map }
\description{
Draw a line or polygon on a map and save it for later use
}

\usage{
drawPoly(sp=TRUE, col='red')
drawLine(sp=TRUE, col='red')
}

\arguments{
  \item{sp}{logical. If \code{TRUE}, the output will be a sp object (SpatialPolygons or SpatialLines). Otherwise a matrix of coordinates is returned}  
  \item{col}{the color of the lines to be drawn}    
}
 
\value{
If \code{sp==TRUE} a sp object (SpatialPolygons or SpatialLines)
otherwise a matrix of coordinates
}
 
\author{ Robert J. Hijmans }


\keyword{ spatial }
