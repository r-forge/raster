\name{BoundingBox}
\alias{getBbox}
\alias{getBbox,BoundingBox-method}
\alias{getBbox,BasicRaster-method}
\alias{getBbox,Spatial-method}
\alias{getBbox,matrix-method}
\alias{getBbox,vector-method}
\alias{bbox,Raster-method}

\alias{newBbox}
\alias{setBbox}
\alias{changeBbox}
\alias{unionBbox}
\alias{intersectBbox}
\alias{alignBbox}

\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}
  
\title{Bounding box functions}
\description{
getBbox extracts a bounding box from a Raster* or Spatial* object (or from a BoundingBox object). It will also create a BoundingBox object from a matrix (2 x 2) or vector (length=4)
newBbox creates a new bounding box (as in the "Spatial" object from the SP package)
setBbox sets the bounding box of a Raster* object
changeBbox changes the bounding box of a Raster* object
alignBbox aligns a bounding box with the cells of a Raster* object
unionBbox returns the union of multiple bounding boxes
intersectBbox returns the instersection of multiple bounding boxes
}

\usage{
newBbox(xmn, xmx, ymn, ymx)
getBbox(object)
setBbox(object, bndbox, keepres=FALSE, snap=FALSE)
changeBbox(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) 
alignBbox(bndbox, object)
unionBbox(x, ...)
intersectBbox(x, ...)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the bounding box}
  \item{xmx}{the maximum x coordinate of the bounding box}
  \item{ymn}{the minimum y coordinate of the bounding box}
  \item{ymx}{the maximum y coordinate of the bounding box}
  \item{object}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted}
  \item{snap}{Adjust the bounding box so that the new raster is aligned with the old raster }
  \item{x}{A BoundingBox or Raster* object } 
  \item{...}{ additional BoundingBox or Raster* objects } 
}
 
\value{
in most cases a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{clickBbox}} }

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
