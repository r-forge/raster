\name{newExtent}

\alias{newExtent}
\alias{alignExtent}
  
\title{Create or align an extent (object of class Extent)}

\description{

newExtent creates a new Extent object

alignExtent aligns an extent (Extent object) with the cells of a Raster* object
}

\usage{
newExtent(xmn, xmx, ymn, ymx)
alignExtent(extent, object)
}

\arguments{
  \item{xmn}{the minimum x coordinate of the extent object}
  \item{xmx}{the maximum x coordinate of the extent object}
  \item{ymn}{the minimum y coordinate of the extent object}
  \item{ymx}{the maximum y coordinate of the extent object}
  \item{extent}{An object of class Extent (which you can create with \code{newExtent()} )}  
  \item{object}{A Raster* object}
}
 
\value{
a object of class Extent
}
 
\details{
Aligning extents assures that the two extents have the same origin and resolution. This should only be used to adjust objects because of imprecision in the data. alignExtent should not be used to force data to match that really does not match (use resample, (dis)aggregate for this).
}
  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawBox}}, \code{\link[raster]{Extent-class}} }

\examples{
r <- raster()
bb <- newExtent(-10.1, 10.1, -20.1, 20.1)
bba <- alignExtent(bb, r)
bb
extent(r)
bba

}

\keyword{spatial}
