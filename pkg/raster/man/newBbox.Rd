\name{newBbox}

\alias{newBbox}
\alias{alignBbox}
  
\title{create or align a BoundingBox}

\description{

newBbox creates a new BoundingBox object
alignBbox aligns a bounding box with the cells of a Raster* object

}

\usage{
newBbox(xmn, xmx, ymn, ymx)
alignBbox(bndbox, object)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the bounding box}
  \item{xmx}{the maximum x coordinate of the bounding box}
  \item{ymn}{the minimum y coordinate of the bounding box}
  \item{ymx}{the maximum y coordinate of the bounding box}
  \item{bndbox}{An object of class BoundingBox (which you can create with newBbox() )}  
  \item{object}{A Raster* object}
}
 
\value{
a BoundingBox object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{getBoungingBox}}, \code{\link[raster]{BoungingBox-class}}, \code{\link[raster]{clickBbox}} }

\examples{
r <- raster()
bb <- newBbox(-10.1, 10.1, -20.1, 20.1)
bba <- alignBbox(bb, r)
}

\keyword{spatial}
