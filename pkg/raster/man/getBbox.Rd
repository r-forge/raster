\name{getBBox}

\alias{getBbox}
\alias{getBbox,BoundingBox-method}
\alias{getBbox,BasicRaster-method}
\alias{getBbox,Spatial-method}
\alias{getBbox,matrix-method}
\alias{getBbox,vector-method}
\alias{bbox,Raster-method}

  
\title{Get a BoundingBox}
\description{
getBbox extracts a bounding box from a Raster* or Spatial* object (or from a BoundingBox object). 
It will also create a BoundingBox object from a matrix (2x2; rows=minx, maxx; cols=miny, maxy) or vector (length=4; order= xmin, xmax, ymin, ymax)
\code{bbox(x)} returns a SP type bbox object (a matrix)
}

\usage{
getBbox(x)
}

\arguments{
  \item{x}{A Raster* or BoundingBox object } 
}
 
\value{
A BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{setBoungingBox}}, \code{\link[raster]{clickBbox}} }

\examples{
r <- raster()
getBbox(r)
getBbox(c(0, 20, 0, 20))
#is equivalent to
newBbox(0, 20, 0, 20)

getBbox(matrix(c(0, 0, 20, 20), nrow=2))


}

\keyword{spatial}
