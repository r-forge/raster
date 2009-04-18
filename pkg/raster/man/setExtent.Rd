\name{setExtent}

\alias{setExtent}
  
\title{Set the extent of a RasteLayer}

\description{
setExtent sets the extent of a Raster* object. Either by providing a new BoundingBox object or by setting the extreme
coordinates one by one.
}

\usage{
setExtent(x, bndbox, keepres=FALSE, snap=FALSE)
}

\arguments{
  \item{x}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (see Details)}
  \item{snap}{logical. If \code{TRUE}, the extent is adjusted so that the cells of the input and output RasterLayer are aligned}
}

\details{

If keepres is \code{FALSE}, 
(by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted
(the RasterLayers get the same origin)

}

\value{
a Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
r <- setExtent(r, bb, keepres=TRUE)
}

\keyword{spatial}
