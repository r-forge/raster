\name{extent}

\alias{extent}
\alias{extent,BoundingBox-method}
\alias{extent,BasicRaster-method}
\alias{extent,Spatial-method}
\alias{extent,matrix-method}
\alias{extent,vector-method}
\alias{bbox,Raster-method}

  
\title{Get a BoundingBox}
\description{
extent returns the extent, as a BoundingBox object, for a Raster* or Spatial* object (or from a BoundingBox object). 
It will also create a BoundingBox object from a matrix (2x2; rows=minx, maxx; cols=miny, maxy) or vector (length=4; order= xmin, xmax, ymin, ymax)
\code{bbox(x)} returns a SP type bbox object (a matrix)
}

\usage{
extent(x)
}

\arguments{
  \item{x}{A Raster* or BoundingBox object } 
}
 
\value{
A BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{setBoungingBox}}, \code{\link[raster]{drawBbox}} }

\examples{
r <- raster()
extent(r)
extent(c(0, 20, 0, 20))
#is equivalent to
newBbox(0, 20, 0, 20)

extent(matrix(c(0, 0, 20, 20), nrow=2))


}

\keyword{spatial}
