\name{changeExtent}

\alias{changeExtent}
  
\title{Change the extent of a RasteLayer}

\description{

changeExtent changes the extent (the bounding box) of a Raster* object

the extent can also be changed by assignment functions (e.g. \code{xmin(x) <- -180}).
}

\usage{
changeExtent(x, xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx = ymax(x), keepres=FALSE) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). if \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted}
  \item{xmn}{the minimum x coordinate of the object}
  \item{xmx}{the maximum x coordinate of the object}
  \item{ymn}{the minimum y coordinate of the object}
  \item{ymx}{the maximum y coordinate of the object}
  
}
 
\value{
a Raster* object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{setExtent}}, \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
r <- changeExtent(r, xmn=1, xmx=50)
xmin(r) <- -30
ymax(r) <- 60
}

\keyword{spatial}
