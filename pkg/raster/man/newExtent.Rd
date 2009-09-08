\name{newExtent}

\alias{newExtent}
\alias{alignBbox}
  
\title{create or align an Extent}

\description{

newExtent creates a new Extent object

alignBbox aligns a bounding box with the cells of a Raster* object
}

\usage{
newExtent(xmn, xmx, ymn, ymx)
alignBbox(bndbox, object)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the bounding box}
  \item{xmx}{the maximum x coordinate of the bounding box}
  \item{ymn}{the minimum y coordinate of the bounding box}
  \item{ymx}{the maximum y coordinate of the bounding box}
  \item{bndbox}{An object of class BoundingBox (which you can create with newExtent() )}  
  \item{object}{A Raster* object}
}
 
\value{
a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawBox}}, \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
bb <- newExtent(-10.1, 10.1, -20.1, 20.1)
bba <- alignBbox(bb, r)
bb
extent(r)
bba

}

\keyword{spatial}
