\name{setExtent}

\alias{setExtent}
\alias{extent<-}
  
\title{Set the extent of a RasteLayer}

\description{
setExtent sets the extent of a Raster* object. Either by providing a new Extent object or by setting the extreme
coordinates one by one.
}

\usage{
setExtent(x, bndbox, keepres=FALSE, snap=FALSE)
extent(x) <- value
}

\arguments{
  \item{x}{A Raster* object}
  \item{bndbox}{An object of class Extent (which you can create with \code{\link[raster]{extent}} )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (see Details)}
  \item{snap}{logical. If \code{TRUE}, the extent is adjusted so that the cells of the input and output RasterLayer are aligned}
  \item{value}{An object of class Extent (which you can create with \code{\link[raster]{extent}} )}  
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

\seealso{  \code{\link[raster]{extent}}, \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
bb <- extent(-10, 10, -20, 20)
extent(r) <- bb
r <- setExtent(r, bb, keepres=TRUE)

}

\keyword{spatial}
