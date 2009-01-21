\name{box}
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

  
\title{Bounding box functions}
\description{
getBbox extracts a bounding box from a Raster* or Spatial* object (or from a BoundingBox object). It will also create a BoundingBox object from a matrix (2 x 2) or vector (length=4)
newBbox creates a new bounding box (as in the "Spatial" object from the SP package)
setBbox sets the bounding box of a Raster* object
changeBbox changes the bounding box of a Raster* object

You can also use bbox(object) to get a sp type bbox (a 2 x 2 matrix), but such an object cannot be used as a bounding box in this package.
}

\usage{
getBbox(object)
newBbox(xmn, xmx, ymn, ymx)
setBbox(object, bndbox, keepres=FALSE, snap=FALSE)
changeBbox(object, xmn=xmin(object), xmx=xmax(object), ymn=ymin(object), ymx = ymax(object), keepres=FALSE) 
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
}
  
\author{Robert J. Hijmans  \email{r.hijmans@gmail.com}}
\examples{
	rs <- newRaster()
	bb <- newBbox(-10, 10, -20, 20)
	rs <- setBbox(rs, bb, keepres=TRUE)
	rs <- changeBbox(rs, ymx = 90)
}

\keyword{spatial}
