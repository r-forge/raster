\name{setBoundingBox}

\alias{newBbox}
\alias{setBbox}
\alias{changeBbox}
\alias{alignBbox}
\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}
  
\title{create or change a BoundingBox}
\description{
newBbox creates a new BoundingBox object
getBbox extracts a bounding box from a Raster* or Spatial* object (or from a BoundingBox object). It will also create a BoundingBox object from a matrix (2 x 2) or vector (length=4)
setBbox sets the bounding box of a Raster* object
changeBbox changes the bounding box of a Raster* object
alignBbox aligns a bounding box with the cells of a Raster* object
}

\usage{
newBbox(xmn, xmx, ymn, ymx)
setBbox(object, bndbox, keepres=FALSE, snap=FALSE)
changeBbox(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) 
alignBbox(bndbox, object)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the bounding box}
  \item{xmx}{the maximum x coordinate of the bounding box}
  \item{ymn}{the minimum y coordinate of the bounding box}
  \item{ymx}{the maximum y coordinate of the bounding box}
  \item{object}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted}
  \item{snap}{logical. If \code{TRUE}, the BoundingBox is adjusted so that the cells of the output RasterLayer are aligned with those of the input RasterLayer }
}
 
\value{
a BoundingBox object, except for setBbox which returns an Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{getBoungingBox}}, \code{\link[raster]{BoungingBox-class}}, \code{\link[raster]{clickBbox}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
r <- setBbox(r, bb, keepres=TRUE)
r <- changeBbox(r, xmn=xmin(r)+1, xmx=xmax(r)+1)
# change BoundingBox coordinates one by one
xmin(r) <- -30
ymax(r) <- 60
}

\keyword{spatial}
