\name{setExtent}

\alias{setExtent}
\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}
  
\title{Set the extent of a RasteLayer}

\description{

setExtent sets the extent (bounding box) of a RasterLayer
}

\usage{
setExtent(x, bndbox, keepres=FALSE, snap=FALSE)
}

\arguments{
  \item{x}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted}
  \item{snap}{logical. If \code{TRUE}, the object's BoundingBox is adjusted so that the cells of the output RasterLayer are aligned with those of the input RasterLayer }
  
}
 
\value{
a Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{changeExtent}}, \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
r <- setExtent(r, bb, keepres=TRUE)
}

\keyword{spatial}
