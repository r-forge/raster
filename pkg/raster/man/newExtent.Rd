\name{newExtent}

\alias{newExtent}
\alias{alignExtent}
  
\title{create or align an Extent}

\description{

newExtent creates a new Extent object

alignExtent aligns an extent (BoundingBox object) with the cells of a Raster* object
}

\usage{
newExtent(xmn, xmx, ymn, ymx)
alignExtent(extent, object)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the bounding box}
  \item{xmx}{the maximum x coordinate of the bounding box}
  \item{ymn}{the minimum y coordinate of the bounding box}
  \item{ymx}{the maximum y coordinate of the bounding box}
  \item{extent}{An object of class BoundingBox (which you can create with \code{newExtent()} )}  
  \item{object}{A Raster* object}
}
 
\value{
a object of class BoundingBox
}
 
\details{
Aligning extents assures that the two extents have the same origin and resolution. This should only be used to adjust objects because of imprecision in the data. alignExtent should not be used to force data to match that really does not match (use resample, (dis)aggregate for this).
}
  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawBox}}, \code{\link[raster]{BoungingBox-class}} }

\examples{
r <- raster()
bb <- newExtent(-10.1, 10.1, -20.1, 20.1)
bba <- alignExtent(bb, r)
bb
extent(r)
bba

}

\keyword{spatial}
