\name{setExtent}

\alias{setExtent}
\alias{changeExtent}
\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}
  
\title{change extent of a RasteLayer}

\description{

setExtent sets a new bounding box of a RasterLayer

changeExtent changes the bounding box of a Raster* object

the extent can also be changed by assignment functions (e.g. \code{xmin(x) <- -180}).
}

\usage{
setExtent(x, bndbox, keepres=FALSE, snap=FALSE)
changeExtent(x, xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx = ymax(x), keepres=FALSE) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted}
  \item{snap}{logical. If \code{TRUE}, the object's BoundingBox is adjusted so that the cells of the output RasterLayer are aligned with those of the input RasterLayer }
  \item{xmn}{the minimum x coordinate of the object}
  \item{xmx}{the maximum x coordinate of the object}
  \item{ymn}{the minimum y coordinate of the object}
  \item{ymx}{the maximum y coordinate of the object}
  
}
 
\value{
a Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{getBbox}}, \code{\link[raster]{BoungingBox-class}}, \code{\link[raster]{clickBbox}} }

\examples{
r <- raster()
bb <- newBbox(-10, 10, -20, 20)
r <- setExtent(r, bb, keepres=TRUE)
r <- changeExtent(r, xmn=xmin(r)+1, xmx=xmax(r)+1)
xmin(r) <- -30
ymax(r) <- 60
}

\keyword{spatial}
